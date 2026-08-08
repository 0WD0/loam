(ns loam.docs.link
  "Global, ownership-aware link resolution for logical pages."
  (:require [clojure.string :as str]
            [loam.anchor :as anchor]
            [loam.ast :as ast]
            [loam.diagnostic :as diagnostic]
            [loam.docs.index :as docs.index]
            [loam.docs.model :as model]
            [loam.route :as route]))

(def external-schemes #{"http" "https" "mailto"})
(def internal-types #{nil "" "id" "custom-id" "fuzzy" "radio"})
(def image-extensions #{"png" "jpg" "jpeg" "gif" "webp" "svg" "avif"})

(defn- distinct-entries [entries]
  (:entries
   (reduce (fn [{:keys [seen] :as state} entry]
             (if (contains? seen (:node-key entry))
               state
               (-> state
                   (update :seen conj (:node-key entry))
                   (update :entries conj entry))))
           {:seen #{} :entries []}
           entries)))

(defn- candidate-summary [entry]
  (select-keys entry [:page-id :href :source :source-span :node-type :title :value]))

(defn- link-diagnostic [severity code message document node data]
  (diagnostic/diagnostic severity code message
                         {:phase :resolve
                          :document document
                          :node node
                          :data data}))

(defn- success [entry]
  {:entry entry
   :resolution {:kind :internal
                :href (:href entry)
                :target-page-id (:page-id entry)
                :target-node-key (:node-key entry)
                :target-anchor (:anchor entry)}})

(defn- choose [document node target candidates]
  (let [candidates (vec (distinct-entries candidates))]
    (cond
      (= 1 (count candidates)) (success (first candidates))
      (> (count candidates) 1)
      {:diagnostics [(link-diagnostic
                      :error :ambiguous-link
                      "Internal link matches multiple targets at the same precedence"
                      document node
                      {:target target
                       :candidates (mapv candidate-summary candidates)})]}
      :else nil)))

(defn- entries-in-page [entries page-id]
  (filter #(= page-id (:page-id %)) entries))

(defn- entries-in-source [entries source]
  (filter #(= source (:source %)) entries))

(defn- fuzzy-candidates [index value]
  (concat (get-in index [:targets value] [])
          (get-in index [:headlines (docs.index/normalize-title value)] [])))

(defn- resolve-fuzzy [index document page node value]
  (let [value (str/replace (or value "") #"^\*+" "")
        page-id (:page/id page)
        source (get-in page [:page/source :path])
        exact-anchor (get-in index [:anchors [page-id (anchor/fragment-id value)]])
        candidates (fuzzy-candidates index value)]
    (or (when exact-anchor (success exact-anchor))
        (choose document node value (entries-in-page candidates page-id))
        (choose document node value (entries-in-source candidates source))
        (choose document node value candidates)
        {:diagnostics [(link-diagnostic :error :unresolved-link
                                        "Internal fuzzy link has no target"
                                        document node {:target value})]})))

(defn- resolve-explicit [index document node kind value]
  (if-let [entry (get-in index [kind value])]
    (success entry)
    {:diagnostics [(link-diagnostic :error :unresolved-link
                                    "Explicit internal link has no target"
                                    document node
                                    {:target-kind kind :target value})]}))

(defn- parser-hint [props]
  (let [{:keys [resolved-id resolved-custom-id resolved-value resolved-type]}
        (:resolved props)]
    (cond
      resolved-id [:ids resolved-id]
      resolved-custom-id [:custom-ids resolved-custom-id]
      (and resolved-value (contains? #{:target :radio-target} resolved-type))
      [:target resolved-value]
      (and resolved-value (= :headline resolved-type)) [:fuzzy resolved-value]
      :else nil)))

(defn- resolve-internal [index document page node]
  (let [{:keys [type path] :as props} (ast/props node)
        [hint-kind hint-value] (parser-hint props)]
    (cond
      (= hint-kind :ids) (resolve-explicit index document node :ids hint-value)
      (= hint-kind :custom-ids) (resolve-explicit index document node :custom-ids hint-value)
      (= hint-kind :target)
      (let [candidates (get-in index [:targets hint-value] [])]
        (or (choose document node hint-value
                    (entries-in-page candidates (:page/id page)))
            (choose document node hint-value
                    (entries-in-source candidates (get-in page [:page/source :path])))
            (choose document node hint-value candidates)
            {:diagnostics [(link-diagnostic :error :unresolved-link
                                            "Resolved parser hint has no target"
                                            document node {:target hint-value})]}))
      (= hint-kind :fuzzy) (resolve-fuzzy index document page node hint-value)
      (= type "id") (resolve-explicit index document node :ids path)
      (= type "custom-id") (resolve-explicit index document node :custom-ids path)
      (and (= type "fuzzy") (str/starts-with? (or path "") "#"))
      (resolve-explicit index document node :custom-ids (subs path 1))
      (= type "radio")
      (let [candidates (get-in index [:targets path] [])]
        (or (choose document node path (entries-in-page candidates (:page/id page)))
            (choose document node path candidates)
            {:diagnostics [(link-diagnostic :error :unresolved-link
                                            "Radio link has no target"
                                            document node {:target path})]}))
      :else (resolve-fuzzy index document page node path))))

(defn- external-href [{:keys [type path raw-link]}]
  (let [prefix (str type ":")]
    (if (and (string? raw-link) (str/starts-with? raw-link prefix))
      raw-link
      (str prefix (or path "")))))

(defn- path-extension [path]
  (some->> path (re-find #"(?i)\.([a-z0-9]+)$") second str/lower-case))

(defn- split-file-selector [path]
  (let [[file selector] (str/split (or path "") #"::" 2)]
    [file selector]))

(defn- source-directory [source]
  (let [parts (str/split source #"/")]
    (str/join "/" (butlast parts))))

(defn- normalize-posix [path]
  (loop [parts (str/split path #"/")
         result []]
    (if-let [part (first parts)]
      (cond
        (or (empty? part) (= "." part)) (recur (rest parts) result)
        (= ".." part) (if (seq result)
                        (recur (rest parts) (pop result))
                        nil)
        :else (recur (rest parts) (conj result part)))
      (str/join "/" result))))

(defn- resolve-file-link [index document page node]
  (let [{:keys [path]} (ast/props node)
        [file selector] (split-file-selector path)
        source (get-in page [:page/source :path])
        relative (str/join "/" (remove str/blank? [(source-directory source) file]))
        target-source (normalize-posix relative)
        extension (path-extension file)]
    (cond
      (or (str/starts-with? (or file "") "/")
          (str/includes? (or file "") "\\"))
      {:diagnostics [(link-diagnostic :error :unsafe-file-link
                                      "File link must be repository-relative POSIX"
                                      document node {:target file})]}

      (or (nil? target-source) (not (route/safe-relative-path? target-source)))
      {:diagnostics [(link-diagnostic :error :unsafe-file-link
                                      "File link escapes the repository or is not a safe POSIX path"
                                      document node {:target file})]}

      (= extension "org")
      (let [page-ids (get-in index [:sources target-source] [])
            pages (keep #(get-in index [:pages %]) page-ids)]
        (cond
          selector
          (let [target (str/replace selector #"^\*+" "")
                candidates (filter #(= target-source (:source %))
                                   (fuzzy-candidates index target))]
            (or (choose document node selector candidates)
                {:diagnostics [(link-diagnostic :error :unresolved-link
                                                "Document file link selector has no target"
                                                document node {:target path})]}))
          (= 1 (count pages))
          (let [target-page (first pages)]
            {:resolution {:kind :internal
                          :href (:page/route target-page)
                          :target-page-id (:page/id target-page)}})
          (> (count pages) 1)
          {:diagnostics [(link-diagnostic :error :ambiguous-link
                                          "Multi-page Org file link requires a selector"
                                          document node
                                          {:target path :page-ids (vec page-ids)})]}
          :else
          {:diagnostics [(link-diagnostic :error :unresolved-link
                                          "Document file link has no indexed source"
                                          document node {:target path})]}))

      (contains? image-extensions extension)
      {:resolution {:kind :asset :href target-source :deferred? true}
       :diagnostics [(link-diagnostic :warning :deferred-asset
                                      "Image asset emission is deferred by the docs profile"
                                      document node {:target target-source})]}

      :else
      {:resolution {:kind :download :href target-source :deferred? true}
       :diagnostics [(link-diagnostic :warning :deferred-download
                                      "Download asset emission is deferred by the docs profile"
                                      document node {:target target-source})]})))

(defn- resolve-one [index document page location]
  (let [node (:node location)
        props (ast/props node)
        type (:type props)
        result (cond
                 (contains? external-schemes type)
                 {:resolution {:kind :external
                               :href (external-href props)
                               :attrs (when (contains? #{"http" "https"} type)
                                        {:rel "noopener noreferrer external"})}}

                 (= "file" type) (resolve-file-link index document page node)
                 (contains? internal-types type) (resolve-internal index document page node)
                 :else
                 {:diagnostics [(link-diagnostic :error :unsafe-link-scheme
                                                 "Link scheme is not allowed by the docs profile"
                                                 document node {:scheme type})]})
        target (:entry result)
        unstable (when (= :generated-title (:anchor-kind target))
                   (link-diagnostic :warning :unstable-referenced-anchor
                                    "Referenced headline has no stable CUSTOM_ID or ID"
                                    document node
                                    {:target-page-id (:page-id target)
                                     :target-anchor (:anchor target)}))]
    (cond-> (dissoc result :entry)
      unstable (update :diagnostics (fnil conj []) unstable))))

(defn- nearest-source-heading [index source page-id location]
  (some (fn [path]
          (let [entry (docs.index/entry-for-node index source path)]
            (when (and entry
                       (= :headline (:node-type entry))
                       (= page-id (:page-id entry)))
              entry)))
        (reverse (:ancestors location))))

(defn- source-context [index page location]
  (let [source (get-in page [:page/source :path])
        heading (nearest-source-heading index source (:page/id page) location)
        source-anchor (anchor/link-occurrence-id (:path location))]
    {:source-anchor source-anchor
     :source-href (anchor/with-fragment (:page/route page) source-anchor)
     :source-heading-title (:title heading)
     :source-heading-href (:href heading)
     :source-link-text (some-> (:node location) ast/text str/trim)}))

(defn resolve-links
  "Resolve every owned link against the global index. Ambiguous and unresolved
  internal links are hard errors; no candidate is selected by collection order."
  [{:keys [documents pages] :as partition} index _opts]
  (let [documents-by-source (into {} (map (juxt #(get-in % [:source :path]) identity)
                                           documents))
        results
        (for [page pages
              :let [source (get-in page [:page/source :path])
                    document (get documents-by-source source)]
              location (model/node-locations document)
              :when (and (= :link (:type (:node location)))
                         (model/owned? partition page source (:path location)))]
          (let [result (resolve-one index document page location)
                context (source-context index page location)
                resolution (cond-> (:resolution result)
                             (= :internal (get-in result [:resolution :kind]))
                             (assoc :source-anchor (:source-anchor context)))]
            (assoc result
                   :node-key (model/node-key source (:path location))
                   :page-id (:page/id page)
                   :source-context context
                   :resolution resolution)))]
    {:links (mapv (fn [result]
                    (merge {:node-key (:node-key result)
                            :page-id (:page-id result)
                            :resolution (:resolution result)}
                           (:source-context result)))
                  results)
     :resolutions (into {}
                        (keep (fn [{:keys [node-key resolution]}]
                                (when resolution [node-key resolution])))
                        results)
     :diagnostics (vec (mapcat :diagnostics results))
     :deferred? (boolean (some #(get-in % [:resolution :deferred?]) results))}))
