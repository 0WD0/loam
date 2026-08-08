(ns loam.personal.model
  "Personal-site logical page profile built on Loam's strict ownership model."
  (:require [clojure.string :as str]
            [loam.ast :as ast]
            [loam.diagnostic :as diagnostic]
            [loam.docs.model :as docs.model]
            [loam.route :as route]))

(def kind-by-group
  {"notes" "note"
   "projects" "project"
   "writing" "writing"
   "posts" "post"
   "pages" "page"})

(def uuid-pattern
  #"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(defn- text-value [value]
  (when-not (nil? value)
    (let [value (cond
                  (keyword? value) (name value)
                  (symbol? value) (name value)
                  :else (str value))
          value (str/trim value)]
      (when-not (str/blank? value) value))))

(defn- boolean-value [value]
  (cond
    (boolean? value) value
    (nil? value) false
    :else (contains? #{"1" "t" "true" "yes" "on"}
                     (str/lower-case (str/trim (str value))))))

(defn- integer-value [value]
  (when-let [value (text-value value)]
    (try
      (Long/parseLong value)
      (catch NumberFormatException _ nil))))

(defn- split-tags [value]
  (cond
    (nil? value) []
    (and (map? value) (= :anonymous (:type value)))
    (keep text-value (:contents value))
    (sequential? value) (keep text-value value)
    :else (->> (str/split (str value) #"[,\s:]+")
               (keep text-value))))

(defn- keyword-properties [document-ast]
  (reduce (fn [properties node]
            (if (= :keyword (:type node))
              (let [{:keys [key value]} (ast/props node)]
                (if (and (string? key) (some? value))
                  (assoc properties (keyword (str/upper-case key)) value)
                  properties))
              properties))
          {}
          (ast/walk document-ast)))

(defn- document-properties [document-ast]
  ;; File-level property drawer values are authoritative when a keyword with the
  ;; same spelling also exists. Standard authoring metadata such as TITLE,
  ;; DESCRIPTION, and FILETAGS normally arrives through keyword nodes.
  (merge (keyword-properties document-ast)
         (ast/props document-ast)))

(defn- page-properties [page]
  (let [root (:page/root-ast page)]
    (if (= :org-data (:type root))
      (document-properties root)
      (ast/props root))))

(defn- page-tags [props]
  (->> (concat (split-tags (:tags props))
               (split-tags (:FILETAGS props))
               (split-tags (:LOAM_TAGS props)))
       distinct
       vec))

(defn- inferred-kind [page]
  (or (get kind-by-group (:page/group page))
      (when (:page/landing? page) "page")
      "note"))

(defn- enrich-page [page]
  (let [props (page-properties page)
        kind (or (some-> (:LOAM_KIND props) text-value str/lower-case)
                 (inferred-kind page))
        status (some-> (:LOAM_STATUS props) text-value str/lower-case)
        tags (page-tags props)
        published-at (or (text-value (:LOAM_PUBLISHED_AT props))
                         (text-value (:PUBLISHED_AT props)))
        updated-at (or (text-value (:LOAM_UPDATED_AT props))
                       (text-value (:UPDATED_AT props)))
        display-order (integer-value (:LOAM_ORDER props))]
    (cond-> (assoc page
                   :page/kind kind
                   :page/tags tags
                   :page/featured? (boolean-value (:LOAM_FEATURED props)))
      status (assoc :page/status status)
      published-at (assoc :page/published-at published-at)
      updated-at (assoc :page/updated-at updated-at)
      display-order (assoc :page/display-order display-order))))

(defn personal-route-builder [{:keys [opts path]}]
  (route/site-route {:base (or (:base opts) "/")
                     :page-path path}))

(defn- source-prefix [document opts]
  (or (get-in opts [:page-id-prefixes (get-in document [:source :path])])
      (:page-id-prefix opts)
      (ast/source-stem (get-in document [:source :path]))
      "document"))

(defn- page-source [document node]
  (let [span (diagnostic/source-span document node)]
    (cond-> {:path (get-in document [:source :path])}
      (:begin span) (assoc :begin (:begin span))
      (:end span) (assoc :end (:end span))
      (get-in span [:start :line]) (assoc :start-line (get-in span [:start :line]))
      (get-in span [:end-location :line]) (assoc :end-line (get-in span [:end-location :line])))))

(defn- document-page? [document]
  (let [props (ast/props (:ast document))]
    (and (text-value (:ID props))
         (text-value (:EXPORT_FILE_NAME props)))))

(defn- document-route-result [document path opts]
  (try
    {:route (personal-route-builder {:document document
                                     :node (:ast document)
                                     :opts opts
                                     :path path})}
    (catch clojure.lang.ExceptionInfo error
      {:diagnostic
       (diagnostic/error :unsafe-route
                         "Document page route contains an unsafe component"
                         {:phase :partition
                          :document document
                          :node (:ast document)
                          :data (select-keys (ex-data error) [:fields])})})))

(defn- partition-document-page [document opts]
  (let [root (:ast document)
        props (document-properties root)
        id (text-value (:ID props))
        path (text-value (:EXPORT_FILE_NAME props))
        title (ast/document-title root)
        description (text-value (:DESCRIPTION props))
        route-result (document-route-result document path opts)
        page-id (str (source-prefix document opts) ":" id)
        page {:page/id page-id
              :page/path path
              :page/version nil
              :page/route (:route route-result)
              :page/source (page-source document root)
              :page/title-ast nil
              :page/root-ast root
              :page/root-path []
              :page/outline-path [title]
              :page/order [-1]
              :page/group (some-> path (str/split #"/") first)
              :page/title title
              :page/description (or description "")
              :page/headings []
              :page/children []}
        owners (into {}
                     (map (fn [{:keys [path]}] [path page-id]))
                     (docs.model/node-locations document))
        diagnostics (cond-> []
                      (:diagnostic route-result) (conj (:diagnostic route-result))
                      (and path (re-find #"(?i)\.[a-z0-9]+$" path))
                      (conj (diagnostic/error
                             :page-path-has-extension
                             "EXPORT_FILE_NAME/page path must not include a file extension"
                             {:phase :partition :document document :node root
                              :data {:page-path path}}))
                      (str/blank? description)
                      (conj (diagnostic/warning
                             :missing-page-description
                             "Document page has no DESCRIPTION keyword"
                             {:phase :partition :document document :node root})))]
    {:pages [page]
     :owners owners
     :diagnostics diagnostics}))

(defn- partition-one [document opts]
  (if (document-page? document)
    (partition-document-page document opts)
    (docs.model/partition-document document opts)))

(defn- canonical-uuid? [value]
  (boolean (and (string? value) (re-matches uuid-pattern value))))

(defn- page-id-diagnostic [document page]
  (let [id (text-value (:ID (page-properties page)))]
    (cond
      (nil? id)
      (diagnostic/error :missing-personal-page-id
                        "Personal-site pages require an Org ID UUID"
                        {:phase :partition
                         :document document
                         :node (:page/root-ast page)})

      (not (canonical-uuid? id))
      (diagnostic/error :invalid-personal-page-id
                        "Personal-site page ID must be a canonical UUID"
                        {:phase :partition
                         :document document
                         :node (:page/root-ast page)
                         :data {:id id}})
      :else nil)))

(defn partition-documents
  "Partition normalized Org documents for an unversioned personal site.

  A document whose org-data root owns both ID and EXPORT_FILE_NAME is one
  document page. Other documents retain Loam's strict headline-page ownership
  model, which is useful for collections such as workbench.org. Every personal
  page must carry a canonical UUID in its Org ID property."
  [documents opts]
  (let [profile-opts (assoc opts
                            :route-builder personal-route-builder
                            :version nil)
        results (mapv #(partition-one % profile-opts) documents)
        pages (->> results (mapcat :pages) (mapv enrich-page))
        documents-by-source (into {} (map (juxt #(get-in % [:source :path]) identity) documents))
        id-diagnostics (keep (fn [page]
                               (let [document (get documents-by-source
                                                   (get-in page [:page/source :path]))]
                                 (page-id-diagnostic document page)))
                             pages)]
    {:documents documents
     :pages pages
     :owners (into {}
                   (map (fn [document result]
                          [(get-in document [:source :path]) (:owners result)])
                        documents
                        results))
     :diagnostics (vec (concat (mapcat :diagnostics results)
                               id-diagnostics))}))
