(ns loam.index
  "Build navigation indexes from Loam documents."
  (:require [loam.anchor :as anchor]
            [loam.ast :as ast]
            [loam.route :as route]
            [clojure.string :as str]
            #?(:clj [clojure.edn :as edn])))

(defn document-url
  ([ast] (document-url {} ast))
  ([system ast]
   (let [source (-> ast ast/props :path)
         export-name (:EXPORT_FILE_NAME (ast/props ast))
         slug (or export-name (ast/source-stem source) (some-> (ast/document-title ast) route/slugify))]
     (if-let [url-for-document (:url-for-document system)]
       (url-for-document {:ast ast
                          :source source
                          :id (ast/document-id ast)
                          :title (ast/document-title ast)
                          :slug slug})
       (str (or (:url-prefix system) "/notes/") slug)))))

(defn normalize-document [system doc]
  (let [ast (or (:ast doc) doc)
        source (or (:source doc) (-> ast ast/props :path))
        ast (if source (assoc-in ast [:properties :path] source) ast)]
    {:ast ast
     :source source
     :source-rel (:source-rel doc)
     :source-dir (:source-dir doc)
     :id (ast/document-id ast)
     :title (ast/document-title ast)
     :url (or (:url doc) (document-url system ast))}))

(defn node-entry [document node]
  (let [p (ast/props node)
        type (:type node)
        anchor (anchor/node-anchor-value node)
        anchor-id (anchor/node-anchor-id node)]
    {:type type
     :source (:source document)
     :source-rel (:source-rel document)
     :source-dir (:source-dir document)
     :page-id (:id document)
     :page-title (:title document)
     :page-url (:url document)
     :id (:ID p)
     :custom-id (:CUSTOM_ID p)
     :value (:value p)
     :title (ast/node-title node)
     :anchor anchor
     :anchor-id anchor-id
     :href (if (and (not= type :org-data) anchor-id)
             (anchor/with-fragment (:url document) anchor-id)
             (:url document))}))

(defn page-entry [document]
  {:type :org-data
   :source (:source document)
   :source-rel (:source-rel document)
   :source-dir (:source-dir document)
   :page-id (:id document)
   :page-title (:title document)
   :page-url (:url document)
   :id (:id document)
   :title (:title document)
   :anchor (:id document)
   :anchor-id (anchor/anchor-id :id (:id document))
   :href (:url document)})

(defn- index-entry [idx entry]
  (cond-> idx
    (:id entry) (assoc-in [:ids (:id entry)] entry)
    (:custom-id entry) (assoc-in [:custom-ids (:custom-id entry)] entry)
    (:source entry) (update-in [:sources (:source entry)] (fnil conj []) entry)
    (:title entry) (update-in [:titles (:title entry)] (fnil conj []) entry)
    (:value entry) (update-in [:targets (:value entry)] (fnil conj []) entry)))

(defn walk-contexts [document]
  (letfn [(step [ctx node]
            (when (ast/node? node)
              (let [entry (node-entry document node)
                    ctx' (if (= :headline (:type node))
                           (assoc ctx :heading entry)
                           ctx)]
                (cons [ctx node]
                      (mapcat #(step ctx' %) (ast/children node))))))]
    (step {:document document} (:ast document))))

(defn link-target-key [link-props]
  (let [{:keys [type path resolved]} link-props]
    (cond
      (:resolved-id resolved) [:id (:resolved-id resolved)]
      (:resolved-custom-id resolved) [:custom-id (:resolved-custom-id resolved)]
      (and (:resolved-value resolved) (= :radio-target (:resolved-type resolved))) [:target (:resolved-value resolved)]
      (and (:resolved-value resolved) (= :target (:resolved-type resolved))) [:target (:resolved-value resolved)]
      (and (:resolved-value resolved) (= :headline (:resolved-type resolved))) [:title (:resolved-value resolved)]
      (= type "id") [:id path]
      (= type "custom-id") [:custom-id path]
      (= type "radio") [:target path]
      (= type "fuzzy") [:title (str/replace (or path "") #"^\*+" "")]
      :else nil)))

(defn link-entry [document ctx node]
  (let [p (ast/props node)
        heading (:heading ctx)]
    {:source (:source document)
     :source-rel (:source-rel document)
     :source-dir (:source-dir document)
     :source-id (:id document)
     :source-title (:title document)
     :source-url (:url document)
     :heading-id (:id heading)
     :heading-title (:title heading)
     :heading-url (:href heading)
     :text (ast/text node)
     :link p
     :target-key (link-target-key p)}))

(defn- add-link [idx link]
  (cond-> (update idx :links (fnil conj []) link)
    (:target-key link) (update-in [:backlinks (:target-key link)] (fnil conj []) link)))

(defn default-index
  "Build the default Loam index without extension indexers."
  [documents]
  (reduce
   (fn [idx document]
     (let [page (page-entry document)
           nodes (map second (walk-contexts document))
           entries (cons page
                         (keep (fn [node]
                                 (when (contains? #{:headline :target :radio-target} (:type node))
                                   (node-entry document node)))
                               nodes))
           links (keep (fn [[ctx node]]
                         (when (= :link (:type node))
                           (link-entry document ctx node)))
                       (walk-contexts document))]
       (-> (reduce index-entry idx entries)
           (assoc-in [:pages (:source document)] page)
           (update :documents conj document)
           (#(reduce add-link % links)))))
   {:documents []
    :pages {}
    :sources {}
    :ids {}
    :custom-ids {}
    :titles {}
    :targets {}
    :links []
    :backlinks {}}
   documents))

(defn build-index
  "Build a Loam index from documents using SYSTEM indexers."
  ([documents] (build-index {} documents))
  ([system documents]
   (let [documents (map #(normalize-document system %) documents)
         idx (default-index documents)]
     (reduce (fn [idx indexer] (indexer idx documents)) idx (:indexers system)))))

(defn lookup-target [idx target-key]
  (let [[kind value] target-key]
    (case kind
      :id (get-in idx [:ids value])
      :custom-id (get-in idx [:custom-ids value])
      :target (first (get-in idx [:targets value]))
      :title (first (get-in idx [:titles value]))
      nil)))

(defn resolve-link [idx link-props]
  (some->> (link-target-key link-props)
           (lookup-target idx)
           :href))

(defn link-resolver [idx]
  (fn [link-props]
    (resolve-link idx link-props)))

(defn backlinks-for [idx target]
  (let [target-key (cond
                     (vector? target) target
                     (map? target) (or (when-let [id (:id target)] [:id id])
                                       (when-let [custom-id (:custom-id target)] [:custom-id custom-id])
                                       (when-let [value (:value target)] [:target value])
                                       (when-let [title (:title target)] [:title title]))
                     :else [:id target])]
    (get-in idx [:backlinks target-key] [])))

#?(:clj
   (defn read-edn-file [path]
     (edn/read-string (slurp path))))

#?(:clj
   (defn index-edn-files
     ([paths] (index-edn-files {} paths))
     ([system paths]
      (build-index system
                   (map (fn [path]
                          {:source path
                           :ast (read-edn-file path)})
                        paths)))))
