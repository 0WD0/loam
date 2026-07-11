(ns loam.docs.index
  "Logical-page-keyed global symbol index."
  (:require [clojure.string :as str]
            [loam.anchor :as anchor]
            [loam.ast :as ast]
            [loam.diagnostic :as diagnostic]
            [loam.docs.model :as model]
            [loam.route :as route]))

(defn normalize-title [title]
  (some-> title str str/trim (str/replace #"\s+" " ") str/lower-case))

(defn- document-map [documents]
  (into {} (map (juxt #(get-in % [:source :path]) identity) documents)))

(defn- entry-for [document page location]
  (let [node (:node location)
        p (ast/props node)
        root? (= (:path location) (:page/root-path page))
        anchor-id (when-not root? (anchor/docs-anchor-id node))]
    {:node-key (model/node-key (get-in document [:source :path]) (:path location))
     :node-type (:type node)
     :source (get-in document [:source :path])
     :source-span (diagnostic/source-span document node)
     :page-id (:page/id page)
     :page-route (:page/route page)
     :id (:ID p)
     :custom-id (:CUSTOM_ID p)
     :value (:value p)
     :title (ast/node-title node)
     :normalized-title (normalize-title (ast/node-title node))
     :anchor anchor-id
     :anchor-kind (when-not root? (anchor/docs-anchor-kind node))
     :explicit-anchor? (and (not root?) (anchor/explicit-docs-anchor? node))
     :href (if anchor-id
             (str (:page/route page) "#" anchor-id)
             (:page/route page))}))

(defn- page-root-entry [document page]
  (let [node (:page/root-ast page)
        p (ast/props node)]
    {:node-key (model/node-key (get-in document [:source :path]) (:page/root-path page))
     :node-type (:type node)
     :source (get-in document [:source :path])
     :source-span (diagnostic/source-span document node)
     :page-id (:page/id page)
     :page-route (:page/route page)
     :id (:ID p)
     :custom-id (:CUSTOM_ID p)
     :title (:page/title page)
     :normalized-title (normalize-title (:page/title page))
     :anchor nil
     :href (:page/route page)}))

(defn- collision-diagnostic [code message document node existing value]
  (diagnostic/error code message
                    {:phase :index
                     :document document
                     :node node
                     :data {:value value
                            :first-source-span (:source-span existing)
                            :first-page-id (:page-id existing)}}))

(defn- add-unique [state index-key value entry document node code message]
  (if-not value
    state
    (if-let [existing (get-in state [:index index-key value])]
      (update state :diagnostics conj
              (collision-diagnostic code message document node existing value))
      (assoc-in state [:index index-key value] entry))))

(defn- add-entry [state document node entry]
  (let [state (-> state
                  (assoc-in [:index :entries (:node-key entry)] entry)
                  (add-unique :ids (:id entry) entry document node
                              :duplicate-id "Duplicate Org ID")
                  (add-unique :custom-ids (:custom-id entry) entry document node
                              :duplicate-custom-id "Duplicate Org CUSTOM_ID"))
        state (if-let [anchor-id (:anchor entry)]
                (add-unique state :anchors [(:page-id entry) anchor-id]
                            entry document node :duplicate-anchor
                            "Duplicate rendered anchor in logical page")
                state)
        state (cond-> state
                (:value entry) (update-in [:index :targets (:value entry)] (fnil conj []) entry)
                (:normalized-title entry) (update-in [:index :headlines (:normalized-title entry)]
                                                     (fnil conj []) entry))]
    state))

(defn- add-page [state documents-by-source page]
  (let [source (get-in page [:page/source :path])
        document (get documents-by-source source)
        node (:page/root-ast page)
        id (:page/id page)
        route-value (:page/route page)
        route-key (route/normalized-route-key route-value)
        state (if-not id
                state
                (if-let [existing (get-in state [:index :pages id])]
                  (update state :diagnostics conj
                          (collision-diagnostic :duplicate-page-id
                                                "Duplicate logical page ID"
                                                document node existing id))
                  (assoc-in state [:index :pages id] page)))
        state (if-not route-key
                state
                (if-let [existing (get-in state [:index :routes route-key])]
                  (update state :diagnostics conj
                          (collision-diagnostic :duplicate-route
                                                "Duplicate or case-colliding logical page route"
                                                document node existing route-value))
                  (assoc-in state [:index :routes route-key] page)))]
    (-> state
        (update-in [:index :sources source] (fnil conj []) id)
        (add-entry document node (page-root-entry document page)))))

(defn- page-node-entries [document page partition]
  (let [source (get-in document [:source :path])]
    (->> (model/node-locations document)
         (filter #(model/owned? partition page source (:path %)))
         (remove #(= (:path %) (:page/root-path page)))
         (filter (fn [{:keys [node]}]
                   (or (= :headline (:type node))
                       (= :target (:type node))
                       (= :radio-target (:type node))
                       (some? (:ID (ast/props node)))
                       (some? (:CUSTOM_ID (ast/props node)))
                       (some? (:name (ast/props node)))
                       (some? (:NAME (ast/props node))))))
         (map (fn [location]
                {:node (:node location)
                 :entry (entry-for document page location)})))))

(defn build-index
  "Build a global index keyed by logical page ID, with source-to-pages modeled
  as a one-to-many vector. Duplicate identities/routes/anchors are errors."
  [{:keys [documents pages] :as partition} _opts]
  (let [documents-by-source (document-map documents)
        initial {:index {:pages {}
                         :routes {}
                         :sources {}
                         :ids {}
                         :custom-ids {}
                         :targets {}
                         :headlines {}
                         :anchors {}
                         :entries {}
                         :assets {}}
                 :diagnostics []}
        with-pages (reduce #(add-page %1 documents-by-source %2) initial pages)
        result (reduce
                (fn [state page]
                  (let [document (get documents-by-source (get-in page [:page/source :path]))]
                    (reduce (fn [state {:keys [node entry]}]
                              (add-entry state document node entry))
                            state
                            (page-node-entries document page partition))))
                with-pages
                pages)]
    (update result :index
            (fn [index]
              (-> index
                  (update :sources #(into {} (map (fn [[k v]] [k (vec v)]) %)))
                  (update :targets #(into {} (map (fn [[k v]] [k (vec v)]) %)))
                  (update :headlines #(into {} (map (fn [[k v]] [k (vec v)]) %))))))))

(defn entry-for-node [index source path]
  (get-in index [:entries (model/node-key source path)]))
