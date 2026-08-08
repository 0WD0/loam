(ns loam.compile
  "Pure logical-document compiler.

  `compile-documents` performs load -> validate -> normalize -> partition ->
  index -> resolve -> render -> emit without filesystem writes. Use
  `loam.emit.bundle/write-artifacts!` for atomic publication."
  (:refer-clojure :exclude [load])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [loam.ast :as ast]
            [loam.core :as core]
            [loam.diagnostic :as diagnostic]
            [loam.docs.index :as docs.index]
            [loam.docs.link :as docs.link]
            [loam.docs.model :as model]
            [loam.docs.render :as docs.render]
            [loam.json :as json]
            [loam.personal.model :as personal.model]
            [loam.route :as route])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def supported-envelope-versions #{1})
(def manifest-schema-version 1)
(def phase-order [:load :validate :normalize :partition :index :resolve :render :emit])

(defn sha256
  "Hex SHA-256 of UTF-8 TEXT."
  [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str text) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))

(defn- parse-input [input]
  (cond
    (string? input) {:envelope (edn/read-string input)}
    (and (map? input) (contains? input :envelope)) input
    (and (map? input) (contains? input :edn))
    (-> input
        (assoc :envelope (edn/read-string (:edn input)))
        (dissoc :edn))
    (map? input) {:envelope input}
    :else (throw (ex-info "Loam compiler input must be an envelope map or EDN string"
                          {:input-type (type input)}))))

(defn load-documents
  "Pure load phase. INPUTS may contain Envelope v1 maps, EDN strings, or
  `{:envelope envelope :source-content text}` wrappers."
  [inputs opts]
  (reduce
   (fn [state input]
     (try
       (let [{:keys [envelope source-content] :as loaded} (parse-input input)
             path (get-in envelope [:ox-edn/source :path])
             source-content (or source-content (get-in opts [:source-contents path]))]
         (update state :documents conj
                 (assoc loaded
                        :envelope envelope
                        :source-content source-content)))
       (catch Exception error
         (update state :diagnostics conj
                 (diagnostic/error :invalid-compiler-input
                                   "Compiler input could not be loaded as EDN Envelope v1"
                                   {:phase :load
                                    :node-type :org-data
                                    :source-span {}
                                    :data {:exception (str (class error))}})))))
   {:documents [] :diagnostics []}
   inputs))

(defn- required-map-diagnostic [envelope key]
  (when-not (map? (get envelope key))
    (diagnostic/error :invalid-envelope
                      (str "Envelope field " key " must be a map")
                      {:phase :validate :node-type :org-data :source-span {}})))

(defn- envelope-shape-diagnostics [envelope]
  (let [required [:ox-edn/schema-version
                  :ox-edn/exporter
                  :ox-edn/source
                  :ox-edn/document
                  :ox-edn/diagnostics]
        missing (remove #(contains? envelope %) required)
        version (:ox-edn/schema-version envelope)]
    (vec
     (concat
      (map (fn [key]
             (diagnostic/error :missing-envelope-field
                               (str "Envelope v1 is missing " key)
                               {:phase :validate
                                :node-type :org-data
                                :source-span {}
                                :data {:field key}}))
           missing)
      (when (and (contains? envelope :ox-edn/schema-version)
                 (not (contains? supported-envelope-versions version)))
        [(diagnostic/error :unsupported-envelope-version
                           "Envelope schema version is not supported"
                           {:phase :validate
                            :node-type :org-data
                            :source-span {}
                            :data {:schema-version version
                                   :supported (vec (sort supported-envelope-versions))}})])
      (keep #(required-map-diagnostic envelope %)
            [:ox-edn/exporter :ox-edn/source :ox-edn/document])
      (when-not (vector? (:ox-edn/diagnostics envelope))
        [(diagnostic/error :invalid-envelope-diagnostics
                           "Envelope diagnostics must be a vector"
                           {:phase :validate :node-type :org-data :source-span {}})])))))

(defn- public-source-span [path]
  (cond-> {} path (assoc :path path)))

(defn- source-diagnostics [{:keys [envelope source-content]} opts]
  (let [{:keys [path encoding] source-digest :sha256} (:ox-edn/source envelope)
        verify? (not= false (:verify-source? opts))]
    (cond-> []
      (not (route/safe-relative-path? path))
      (conj (diagnostic/error :unsafe-source-path
                              "Envelope source path must be repository-relative POSIX"
                              {:phase :validate
                               :node-type :org-data
                               :source-span (public-source-span path)
                               :data {:problems (route/relative-path-problems path)}}))
      (not (and (string? source-digest) (re-matches #"[0-9a-f]{64}" source-digest)))
      (conj (diagnostic/error :invalid-source-digest
                              "Envelope source sha256 must be 64 hexadecimal characters"
                              {:phase :validate :node-type :org-data
                               :source-span (public-source-span path)}))
      (not= "utf-8" (some-> encoding str/lower-case))
      (conj (diagnostic/error :unsupported-source-encoding
                              "Envelope source encoding must be utf-8"
                              {:phase :validate :node-type :org-data
                               :source-span (public-source-span path)
                               :data {:encoding encoding}}))
      (and verify? (nil? source-content))
      (conj (diagnostic/error :missing-source-content
                              "Source content is required to verify the envelope digest and spans"
                              {:phase :validate :node-type :org-data
                               :source-span (public-source-span path)}))
      (and verify? source-content (string? source-digest)
           (not= (str/lower-case source-digest) (sha256 source-content)))
      (conj (diagnostic/error :source-digest-mismatch
                              "Envelope source sha256 does not match source content"
                              {:phase :validate :node-type :org-data
                               :source-span (public-source-span path)})))))

(defn- exporter-diagnostics [envelope]
  (let [exporter (:ox-edn/exporter envelope)
        path (get-in envelope [:ox-edn/source :path])
        invalid (filter #(not (string? (get exporter %)))
                        [:name :version :emacs-version :org-version])]
    (mapv (fn [field]
            (diagnostic/error :invalid-exporter-metadata
                              "Envelope exporter metadata is incomplete"
                              {:phase :validate :node-type :org-data
                               :source-span (public-source-span path)
                               :data {:field field}}))
          (cond-> (vec invalid)
            (and (string? (:name exporter)) (not= "ox-edn" (:name exporter)))
            (conj :name)))))

(defn- node-position-diagnostics [loaded opts]
  (let [envelope (:envelope loaded)
        document (:ox-edn/document envelope)
        path (get-in envelope [:ox-edn/source :path])
        content (:source-content loaded)
        max-offset (when content (inc (count content)))
        require? (not= false (:require-source-spans? opts))]
    (if-not (and (ast/node? document) (= :org-data (:type document)))
      [(diagnostic/error :invalid-document-root
                         "Envelope document must be an ox-edn AST node"
                         {:phase :validate :node-type :org-data
                          :source-span (public-source-span path)})]
      (vec
       (mapcat
        (fn [node]
          (let [{:keys [begin end]} (ast/props node)
                span (cond-> {:path path} begin (assoc :begin begin) end (assoc :end end))]
            (cond
              (and require? (or (nil? begin) (nil? end)))
              [(diagnostic/error :missing-source-span
                                 "AST node requires begin and end offsets in strict mode"
                                 {:phase :validate :node node :node-type (:type node)
                                  :source-span span})]
              (or (and begin (not (integer? begin)))
                  (and end (not (integer? end)))
                  (and begin (not (pos? begin)))
                  (and end (not (pos? end)))
                  (and begin end (> begin end))
                  (and max-offset end (> end max-offset)))
              [(diagnostic/error :invalid-source-span
                                 "AST node source span is invalid or out of bounds"
                                 {:phase :validate :node node :node-type (:type node)
                                  :source-span span
                                  :data {:source-length (when content (count content))}})]
              :else [])))
        (ast/walk document))))))

(defn- producer-diagnostics [envelope]
  (let [path (get-in envelope [:ox-edn/source :path])]
    (mapv
     (fn [value]
       (let [span (:source-span value)]
         (if (and (map? value)
                  (contains? #{:info :warning :error} (:severity value))
                  (keyword? (:code value))
                  (string? (:message value))
                  (map? span)
                  (integer? (:begin span))
                  (integer? (:end span))
                  (<= 0 (:begin span) (:end span))
                  (keyword? (:node-type value)))
         (-> value
             (update :source-span #(merge {:path path} (or % {})))
             (assoc :phase :validate))
         (diagnostic/error :invalid-producer-diagnostic
                           "Envelope contains a malformed producer diagnostic"
                           {:phase :validate :node-type :org-data
                            :source-span (public-source-span path)}))))
     (or (:ox-edn/diagnostics envelope) []))))

(defn validate-document
  "Validate one loaded Envelope v1 wrapper."
  [loaded opts]
  (let [envelope (:envelope loaded)]
    (if-not (map? envelope)
      [(diagnostic/error :invalid-envelope
                         "Compiler input is not an Envelope v1 map"
                         {:phase :validate :node-type :org-data :source-span {}})]
      (let [shape (envelope-shape-diagnostics envelope)]
        (if (some diagnostic/error? shape)
          shape
          (vec (concat shape
                       (source-diagnostics loaded opts)
                       (exporter-diagnostics envelope)
                       (node-position-diagnostics loaded opts)
                       (producer-diagnostics envelope))))))))

(defn validate-documents [loaded opts system]
  (let [built-in (mapcat #(validate-document % opts) (:documents loaded))
        extension-diagnostics
        (mapcat (fn [validator]
                  (mapcat #(or (validator % opts) []) (:documents loaded)))
                (:validators system))]
    {:documents (:documents loaded)
     :diagnostics (vec (concat (:diagnostics loaded) built-in extension-diagnostics))}))

(defn normalize-document
  "Normalize envelope metadata without changing the source AST."
  [loaded]
  (let [envelope (:envelope loaded)
        source (:ox-edn/source envelope)
        content (:source-content loaded)]
    {:envelope envelope
     :source (select-keys source [:path :sha256 :encoding])
     :source-content content
     :line-starts (diagnostic/line-starts content)
     :ast (:ox-edn/document envelope)
     :exporter (:ox-edn/exporter envelope)}))

(defn normalize-documents [validated opts system]
  (let [documents (->> (:documents validated)
                       (map normalize-document)
                       (map (fn [document]
                              (reduce (fn [document transform]
                                        (transform document opts))
                                      document
                                      (:document-transforms system))))
                       (sort-by #(get-in % [:source :path]))
                       vec)]
    {:documents documents
     :diagnostics (:diagnostics validated)}))

(defn partition-pages [normalized opts system]
  (let [partitioner (or (:partitioner system) model/partition-documents)
        partition (partitioner (:documents normalized) opts)]
    (reduce (fn [partition post-partitioner]
              (post-partitioner partition opts))
            partition
            (:page-partitioners system))))

(defn index-pages [partition opts]
  (docs.index/build-index partition opts))

(defn resolve-page-links [partition index opts]
  (docs.link/resolve-links partition index opts))

(defn render-page-fragments [partition index resolutions opts system]
  (docs.render/render-pages partition index resolutions
                            (assoc opts :renderers (:renderers system))))

(defn- manifest-source [document]
  {:path (get-in document [:source :path])
   :sha256 (get-in document [:source :sha256])})

(defn- public-diagnostic [value]
  (cond-> {:severity (:severity value)
           :code (:code value)
           :message (:message value)
           :sourceSpan (:source-span value)
           :nodeType (:node-type value)}
    (:phase value) (assoc :phase (:phase value))
    (:data value) (assoc :data (:data value))))

(defn- page-content-file [page]
  (str "pages/" (str/replace (:page/path page) "/" "-") ".html"))

(defn- manifest-page [page rendered digest content-file relations]
  (let [source (:page/source page)
        page-id (:page/id page)]
    (cond-> {:id page-id
             :path (:page/path page)
             :route (:page/route page)
             :title (:page/title page)
             :description (or (:page/description page) "")
             :contentFile content-file
             :digest digest
             :source (cond-> {:path (:path source)}
                       (:start-line source) (assoc :startLine (:start-line source))
                       (:end-line source) (assoc :endLine (:end-line source)))
             :headings (:headings rendered)
             :order (vec (:page/order page))
             :childIds (vec (:page/children page))
             :outgoingLinks (vec (get-in relations [:outgoing page-id] []))
             :backlinks (vec (get-in relations [:backlinks page-id] []))}
      (:page/version page) (assoc :version (:page/version page))
      (:page/parent page) (assoc :parentId (:page/parent page))
      (:page/previous page) (assoc :previousId (:page/previous page))
      (:page/next page) (assoc :nextId (:page/next page))
      (:page/landing? page) (assoc :landing true)
      (:page/kind page) (assoc :kind (:page/kind page))
      (:page/status page) (assoc :status (:page/status page))
      (contains? page :page/tags) (assoc :tags (vec (:page/tags page)))
      (contains? page :page/featured?) (assoc :featured (boolean (:page/featured? page)))
      (:page/published-at page) (assoc :publishedAt (:page/published-at page))
      (:page/updated-at page) (assoc :updatedAt (:page/updated-at page))
      (:page/display-order page) (assoc :displayOrder (:page/display-order page)))))

(defn- href-route [href]
  (when (string? href)
    (first (str/split href #"#" 2))))

(defn- link-relations [index resolution-result]
  (let [edges (->> (:links resolution-result)
                   (keep (fn [{:keys [page-id resolution]}]
                           (when (= :internal (:kind resolution))
                             (let [target-route (href-route (:href resolution))
                                   target (get-in index [:routes (route/normalized-route-key target-route)])
                                   target-id (:page/id target)]
                               (when (and target-id (not= page-id target-id))
                                 {:from page-id :to target-id})))))
                   distinct
                   (sort-by (juxt :from :to))
                   vec)
        outgoing (reduce (fn [m {:keys [from to]}]
                           (update m from (fnil conj []) to))
                         {} edges)
        backlinks (reduce (fn [m {:keys [from to]}]
                            (update m to (fnil conj []) from))
                          {} edges)
        normalize (fn [m]
                    (into {} (map (fn [[k values]] [k (vec (sort (distinct values)))]) m)))]
    {:edges edges
     :outgoing (normalize outgoing)
     :backlinks (normalize backlinks)}))

(defn- search-index-model [pages]
  {:schemaVersion 1
   :pages (mapv (fn [page]
                  (select-keys page [:id :route :title :description :kind :status :tags
                                     :publishedAt :updatedAt :headings]))
                pages)})

(defn- graph-model [pages relations]
  {:schemaVersion 1
   :nodes (mapv (fn [page]
                  (select-keys page [:id :route :title :kind :status :tags]))
                pages)
   :edges (:edges relations)})

(defn- build-commit-id [opts]
  (get-in opts [:build :commitId]))

(defn- build-commit-diagnostics [opts]
  (let [build (:build opts)
        commit-id (build-commit-id opts)]
    (if (or (nil? build)
            (and (map? build)
                 (or (not (contains? build :commitId))
                     (and (string? commit-id) (not (str/blank? commit-id))))))
      []
      [(diagnostic/error :invalid-build-commit
                         "Manifest build.commitId must be a non-empty string when provided"
                         {:phase :emit :node-type :org-data :source-span {}
                          :data {:field :commitId}})])))

(defn- machine-path? [value]
  (and (string? value)
       (or (re-find #"(?:^|[\s\"'])/(?:home|Users|tmp|private/tmp)/" value)
           (re-find #"(?i)(?:^|[\s\"'])[a-z]:\\" value))))

(defn- machine-path-values [value path]
  (cond
    (machine-path? value) [{:path path :value value}]
    (map? value) (mapcat (fn [[k v]] (machine-path-values v (conj path k))) value)
    (sequential? value) (mapcat (fn [[index v]] (machine-path-values v (conj path index)))
                                (map-indexed vector value))
    :else []))

(defn emit-artifacts
  "Pure Manifest v1 + HTML fragment emitter. No filesystem writes occur here."
  [partition index resolution-result render-result diagnostics opts]
  (let [pages (:pages partition)
        rendered-by-id (into {} (map (juxt :page-id identity)
                                     (:rendered-pages render-result)))
        page-files (mapv (fn [page] [(:page/id page) (page-content-file page)]) pages)
        file-groups (group-by second page-files)
        collision-diagnostics
        (mapv (fn [[file entries]]
                (diagnostic/error :duplicate-content-file
                                  "Logical page paths collide after fragment filename normalization"
                                  {:phase :emit :node-type :org-data :source-span {}
                                   :data {:content-file file
                                          :page-ids (mapv first entries)}}))
              (filter (fn [[_ entries]] (> (count entries) 1)) file-groups))
        commit-diagnostics (build-commit-diagnostics opts)
        diagnostics (diagnostic/sort-diagnostics
                     (concat diagnostics collision-diagnostics
                             commit-diagnostics))]
    (if (some diagnostic/error? diagnostics)
      {:diagnostics diagnostics}
      (let [fragment-data
            (mapv (fn [[page-id content-file]]
                    (let [rendered (get rendered-by-id page-id)
                          content (:html rendered)]
                      {:page-id page-id
                       :content-file content-file
                       :html content
                       :digest (sha256 content)}))
                  page-files)
            fragment-by-id (into {} (map (juxt :page-id identity) fragment-data))
            unreleasable? (boolean
                           (or (:deferred? resolution-result)
                               (:deferred? render-result)
                               (some #(contains? #{:deferred-macro :deferred-node-type
                                                   :deferred-asset :deferred-download}
                                                 (:code %))
                                     diagnostics)))
            relations (link-relations index resolution-result)
            source-models (mapv manifest-source (:documents partition))
            page-models (mapv (fn [page]
                                (let [rendered (get rendered-by-id (:page/id page))
                                      fragment (get fragment-by-id (:page/id page))]
                                  (manifest-page page rendered (:digest fragment)
                                                 (:content-file fragment) relations)))
                              pages)
            search-index (search-index-model page-models)
            graph (graph-model page-models relations)
            content-hash (sha256 (str (pr-str source-models)
                                      "\n"
                                      (str/join "\n" (map (juxt :content-file :digest) fragment-data))))
            commit-id (build-commit-id opts)
            manifest {:schemaVersion manifest-schema-version
                      :sources source-models
                      :build (cond-> {:contentHash content-hash
                                      :unreleasable unreleasable?}
                               commit-id (assoc :commitId commit-id))
                      :pages page-models
                      :navigation (model/navigation-tree pages)
                      :redirects (vec (or (:redirects opts) []))
                      :diagnostics (mapv public-diagnostic diagnostics)}
            report {:schemaVersion 1
                    :status (if unreleasable? "unreleasable" "ok")
                    :unreleasable unreleasable?
                    :pageCount (count pages)
                    :sourceCount (count (:documents partition))
                    :coverage (:coverage render-result)
                    :diagnostics (mapv public-diagnostic diagnostics)}
            leaked (concat (machine-path-values manifest [])
                           (machine-path-values report [])
                           (mapcat (fn [{:keys [content-file html]}]
                                     (map #(assoc % :file content-file)
                                          (machine-path-values html [])))
                                   fragment-data))]
        (if (seq leaked)
          {:diagnostics
           (diagnostic/sort-diagnostics
            (conj diagnostics
                  (diagnostic/error :absolute-path-leak
                                    "Public docs artifacts contain a machine-absolute path"
                                    {:phase :emit :node-type :org-data :source-span {}
                                     :data {:locations (mapv #(dissoc % :value) leaked)}})))}
          {:manifest manifest
           :report report
           :search-index search-index
           :graph graph
           :fragments (into (sorted-map)
                            (map (juxt :content-file :html) fragment-data))
           :files (into (sorted-map)
                        (concat [["manifest.json" (str (json/render-canonical-json manifest) "\n")]
                                 ["build-report.json" (str (json/render-canonical-json report) "\n")]
                                 ["search-index.json" (str (json/render-canonical-json search-index) "\n")]
                                 ["graph.json" (str (json/render-canonical-json graph) "\n")]]
                                (map (juxt :content-file :html) fragment-data)))
           :diagnostics diagnostics
           :unreleasable? unreleasable?})))))

(defn- has-errors? [diagnostics]
  (boolean (some diagnostic/error? diagnostics)))

(defn- finish [state]
  (let [diagnostics (diagnostic/sort-diagnostics (:diagnostics state))]
    (assoc state
           :diagnostics diagnostics
           :status (if (has-errors? diagnostics) :error :ok))))

(defn- profile-partitioner [opts]
  (when (contains? #{:personal :loam/personal} (:profile opts))
    personal.model/partition-documents))

(defn compile-system
  "Create a compiler service system. The docs partitioner remains the default;
  profiles may replace the logical page model through the single-instance
  :partitioner target."
  [opts]
  (let [partitioner (or (:partitioner opts) (profile-partitioner opts))]
    (core/create-system
     (cond-> {:extensions (vec (:extensions opts))}
       partitioner (assoc :partitioner partitioner)))))

(defn compile-documents
  "Pure compiler entry. Returns `{:status :ok :artifacts ...}` or a structured
  error result; it never writes files and never emits partial artifacts."
  ([inputs] (compile-documents inputs {}))
  ([inputs opts]
   (let [system (or (:system opts) (compile-system opts))
         loaded (load-documents inputs opts)
         state {:phase-trace [:load]
                :diagnostics (:diagnostics loaded)}]
     (if (has-errors? (:diagnostics state))
       (finish state)
       (let [validated (validate-documents loaded opts system)
             state (assoc state :phase-trace [:load :validate]
                                :diagnostics (:diagnostics validated))]
         (if (has-errors? (:diagnostics state))
           (finish state)
           (let [normalized (normalize-documents validated opts system)
                 partition (partition-pages normalized opts system)
                 diagnostics (concat (:diagnostics normalized) (:diagnostics partition))
                 state (assoc state :phase-trace [:load :validate :normalize :partition]
                                    :documents (:documents normalized)
                                    :partition partition
                                    :pages (:pages partition)
                                    :diagnostics diagnostics)
                 indexed (index-pages partition opts)
                 diagnostics (concat diagnostics (:diagnostics indexed))
                 state (assoc state :phase-trace (conj (:phase-trace state) :index)
                                    :index (:index indexed)
                                    :diagnostics diagnostics)
                 resolved (resolve-page-links partition (:index indexed) opts)
                 diagnostics (concat diagnostics (:diagnostics resolved))
                 state (assoc state :phase-trace (conj (:phase-trace state) :resolve)
                                    :links (:links resolved)
                                    :resolutions (:resolutions resolved)
                                    :diagnostics diagnostics)]
             (if (has-errors? diagnostics)
               (finish state)
               (let [rendered (render-page-fragments partition (:index indexed)
                                                     (:resolutions resolved) opts system)
                     diagnostics (concat diagnostics (:diagnostics rendered))
                     rule-diagnostics (mapcat #(or (% (assoc state :render rendered) opts) [])
                                              (:diagnostic-rules system))
                     diagnostics (concat diagnostics rule-diagnostics)
                     state (assoc state :phase-trace (conj (:phase-trace state) :render)
                                        :render rendered
                                        :diagnostics diagnostics)]
                 (if (has-errors? diagnostics)
                   (finish state)
                   (let [emitted (emit-artifacts partition (:index indexed) resolved rendered
                                                 diagnostics opts)
                         emitted (reduce (fn [artifacts emitter]
                                           (emitter artifacts opts))
                                         emitted
                                         (:emitters system))
                         diagnostics (:diagnostics emitted)
                         state (assoc state :phase-trace (conj (:phase-trace state) :emit)
                                            :diagnostics diagnostics
                                            :artifacts (when-not (has-errors? diagnostics)
                                                         (dissoc emitted :diagnostics)))]
                     (finish state))))))))))))

(defn compile-documents!
  "Pure throwing variant for production callers. Structured diagnostics are in
  ex-data under :diagnostics."
  ([inputs] (compile-documents! inputs {}))
  ([inputs opts]
   (let [result (compile-documents inputs opts)]
     (diagnostic/throw-if-errors! (:diagnostics result))
     result)))
