(ns loam.docs.model
  "Logical page discovery, ownership, and route modeling for Org documents."
  (:require [clojure.string :as str]
            [loam.ast :as ast]
            [loam.diagnostic :as diagnostic]
            [loam.route :as route]))

(def hidden-preamble-types
  #{:keyword :property-drawer :planning :comment :comment-block})

(defn indexed-children [node]
  (keep-indexed (fn [index child]
                  (when (ast/node? child) [index child]))
                (ast/children node)))

(defn node-locations
  "Depth-first locations for DOCUMENT. Paths contain indexes into :contents and
  therefore remain stable without mutating the source AST."
  [document]
  (letfn [(step [node path ancestors outline]
            (let [headline? (= :headline (:type node))
                  title (when headline? (ast/node-title node))
                  outline' (cond-> outline title (conj title))
                  here {:document document
                        :node node
                        :path path
                        :ancestors ancestors
                        :outline-path outline'}]
              (cons here
                    (mapcat (fn [[index child]]
                              (step child
                                    (conj path index)
                                    (conj ancestors path)
                                    outline'))
                            (indexed-children node)))))]
    (step (:ast document) [] [] [(ast/document-title (:ast document))])))

(defn node-key [source path]
  [source (vec path)])

(defn page-root?
  "A level-one headline or any headline with EXPORT_FILE_NAME is a page root."
  [node]
  (let [p (ast/props node)]
    (and (= :headline (:type node))
         (or (= 1 (:level p))
             (some? (:EXPORT_FILE_NAME p))))))

(defn- visible-node? [node]
  (cond
    (not (ast/node? node)) (not (str/blank? (str node)))
    (contains? hidden-preamble-types (:type node)) false
    (= :section (:type node)) (boolean (some visible-node? (ast/children node)))
    :else true))

(defn- preamble-paths [document root-paths]
  (let [roots (set root-paths)]
    (->> (node-locations document)
         (remove #(empty? (:path %)))
         (take-while #(not (contains? roots (:path %))))
         (filter #(visible-node? (:node %)))
         (map :path)
         vec)))

(defn- source-prefix [document opts]
  (or (get-in opts [:page-id-prefixes (get-in document [:source :path])])
      (:page-id-prefix opts)
      (ast/source-stem (get-in document [:source :path]))
      "document"))

(defn- page-identity [document node opts]
  (let [p (ast/props node)
        identity (or (:CUSTOM_ID p) (:ID p) (:EXPORT_FILE_NAME p))]
    (when identity
      (str (source-prefix document opts) ":" identity))))

(defn- page-path [node]
  (let [p (ast/props node)]
    (or (:EXPORT_FILE_NAME p) (:CUSTOM_ID p))))

(defn- route-result [document node opts path]
  (try
    {:route (route/docs-route {:base (or (:base opts) "/docs")
                               :version (or (:version opts) "dev")
                               :locale (:locale opts)
                               :page-path path})}
    (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) error
      {:diagnostic
       (diagnostic/error :unsafe-route
                         "Logical page route contains an unsafe component"
                         {:phase :partition
                          :document document
                          :node node
                          :data (select-keys (ex-data error) [:fields])})})))

(defn- page-source [document node]
  (let [span (diagnostic/source-span document node)]
    (cond-> {:path (get-in document [:source :path])}
      (:begin span) (assoc :begin (:begin span))
      (:end span) (assoc :end (:end span))
      (get-in span [:start :line]) (assoc :start-line (get-in span [:start :line]))
      (get-in span [:end-location :line]) (assoc :end-line (get-in span [:end-location :line])))))

(defn- headline-page [document location index opts]
  (let [node (:node location)
        p (ast/props node)
        id (page-identity document node opts)
        path (page-path node)
        route-result (when path (route-result document node opts path))
        diagnostics (cond-> []
                      (nil? id)
                      (conj (diagnostic/error
                             :missing-page-identity
                             "Page root requires CUSTOM_ID, ID, or EXPORT_FILE_NAME"
                             {:phase :partition :document document :node node}))
                      (nil? path)
                      (conj (diagnostic/error
                             :missing-page-path
                             "Page root requires EXPORT_FILE_NAME or CUSTOM_ID"
                             {:phase :partition :document document :node node}))
                      (:diagnostic route-result) (conj (:diagnostic route-result))
                      (and (string? path) (re-find #"(?i)\.[a-z0-9]+$" path))
                      (conj (diagnostic/error
                             :page-path-has-extension
                             "EXPORT_FILE_NAME/page path must not include a file extension"
                             {:phase :partition :document document :node node
                              :data {:page-path path}}))
                      (str/blank? (:DESCRIPTION p))
                      (conj (diagnostic/warning
                             :missing-page-description
                             "Logical page has no DESCRIPTION property"
                             {:phase :partition :document document :node node})))]
    {:page (cond-> {:page/id id
                    :page/path path
                    :page/version (or (:version opts) "dev")
                    :page/route (:route route-result)
                    :page/source (page-source document node)
                    :page/title-ast (:title p)
                    :page/root-ast node
                    :page/root-path (:path location)
                    :page/outline-path (:outline-path location)
                    :page/order [index]
                    :page/group (some-> path (str/split #"/") first)
                    :page/title (ast/node-title node)
                    :page/description (:DESCRIPTION p)
                    :page/headings []
                    :page/children []}
             (:locale opts) (assoc :page/locale (:locale opts)))
     :diagnostics diagnostics}))

(defn- landing-page [document opts]
  (let [ast (:ast document)
        p (ast/props ast)
        prefix (source-prefix document opts)
        path (or (get-in opts [:landing-paths (get-in document [:source :path])])
                 (:EXPORT_FILE_NAME p)
                 (ast/source-stem (get-in document [:source :path])))
        id (or (:ID p) (str prefix ":landing"))
        route-result (route-result document ast opts path)]
    {:page {:page/id id
            :page/path path
            :page/version (or (:version opts) "dev")
            :page/route (:route route-result)
            :page/source (page-source document ast)
            :page/title-ast nil
            :page/root-ast ast
            :page/root-path []
            :page/outline-path [(ast/document-title ast)]
            :page/order [-1]
            :page/group (some-> path (str/split #"/") first)
            :page/title (ast/document-title ast)
            :page/description (or (:DESCRIPTION p) "")
            :page/headings []
            :page/children []
            :page/landing? true}
     :diagnostics (cond-> []
                    (:diagnostic route-result) (conj (:diagnostic route-result)))}))

(defn- nearest-root-path [root-path-set ancestors]
  (last (filter root-path-set ancestors)))

(defn- attach-tree [pages]
  (let [path->id (into {} (map (juxt :page/root-path :page/id) pages))
        pages (mapv (fn [page]
                      (let [path (:page/root-path page)
                            ancestors (mapv #(subvec path 0 %) (range 0 (count path)))
                            parent-path (last (filter path->id ancestors))]
                        (assoc page :page/parent (get path->id parent-path))))
                    pages)
        children (reduce (fn [m page]
                           (if-let [parent (:page/parent page)]
                             (update m parent (fnil conj []) (:page/id page))
                             m))
                         {}
                         pages)]
    (mapv (fn [page]
            (assoc page :page/children (get children (:page/id page) [])))
          pages)))

(defn- add-prev-next [pages]
  (mapv (fn [index page]
          (cond-> page
            (pos? index) (assoc :page/previous (:page/id (nth pages (dec index))))
            (< index (dec (count pages))) (assoc :page/next (:page/id (nth pages (inc index))))))
        (range)
        pages))

(defn- ownership-for [document pages]
  (let [root->id (into {} (map (juxt :page/root-path :page/id) pages))
        landing-id (some #(when (:page/landing? %) (:page/id %)) pages)]
    (letfn [(step [owners node path owner]
              (let [owner (or (get root->id path) owner)
                    owners (if owner (assoc owners path owner) owners)]
                (reduce (fn [m [index child]]
                          (step m child (conj path index) owner))
                        owners
                        (indexed-children node))))]
      (step {} (:ast document) [] landing-id))))

(defn partition-document
  "Partition one normalized source document into logical pages."
  [document opts]
  (let [locations (vec (node-locations document))
        roots (vec (filter #(page-root? (:node %)) locations))
        built (mapv (fn [index location]
                      (headline-page document location index opts))
                    (range)
                    roots)
        root-paths (mapv :path roots)
        landing? (seq (preamble-paths document root-paths))
        landing (when landing? (landing-page document opts))
        pages (if landing
                (into [(:page landing)] (map :page built))
                (mapv :page built))
        pages (-> pages attach-tree add-prev-next)]
    {:pages pages
     :owners (ownership-for document pages)
     :diagnostics (vec (concat (mapcat :diagnostics built)
                               (:diagnostics landing)))}))

(defn partition-documents
  "Partition normalized documents once, preserving source order and ASTs."
  [documents opts]
  (let [results (mapv #(partition-document % opts) documents)]
    {:documents documents
     :pages (vec (mapcat :pages results))
     :owners (into {}
                   (map (fn [document result]
                          [(get-in document [:source :path]) (:owners result)])
                        documents
                        results))
     :diagnostics (vec (mapcat :diagnostics results))}))

(defn owner-id [partition source path]
  (get-in partition [:owners source (vec path)]))

(defn owned? [partition page source path]
  (= (:page/id page) (owner-id partition source path)))

(defn navigation-tree
  "Derive deterministic public navigation from the logical page-root tree.

  Pages are already in authored preorder.  Landing pages, when present, are
  roots whose children are the document's top-level page roots; nested page
  roots remain children of their nearest owning page root."
  [pages]
  (let [by-id (into {} (map (juxt :page/id identity) pages))]
    (letfn [(node [page]
              (cond-> {:id (:page/id page)
                       :title (:page/title page)
                       :path (:page/path page)
                       :route (:page/route page)
                       :order (vec (:page/order page))
                       :children (mapv (fn [id]
                                         (node (or (get by-id id)
                                                   (throw (ex-info
                                                           "Navigation child page is missing"
                                                           {:page-id (:page/id page)
                                                            :child-id id})))))
                                       (:page/children page))}
                (:page/landing? page) (assoc :landing true)))]
      (mapv node (remove :page/parent pages)))))
