(ns loam.docs.reference
  "Explicit reference entries extracted from authored Org description lists.

  Majutsu marks its generated reference pages with an INDEX property (`ky`,
  `fn`, or `vr`).  Their entries come only from description-list terms such as
  `Key: ...`, `Command: ...`, and `User Option: ...`; ordinary prose and
  monospace text are deliberately ignored."
  (:require [clojure.string :as str]
            [loam.anchor :as anchor]
            [loam.ast :as ast]
            [loam.diagnostic :as diagnostic]
            [loam.docs.model :as model]))

(def index-definitions
  {"ky" {:label "keystroke" :plural "keystroke entries"}
   "fn" {:label "function or command" :plural "function and command entries"}
   "vr" {:label "variable" :plural "variable and user-option entries"}})

(defn- normalized-text [value]
  (some-> value ast/text str/trim (str/replace #"\s+" " ") not-empty))

(defn description-term
  "Return the normalized authored description-list term for NODE, or nil."
  [node]
  (when (= :item (:type node))
    (some-> node ast/props :tag normalized-text)))

(defn- tag-parts [node]
  (when-let [tag (description-term node)]
    (when-let [[_ prefix term]
               (re-matches
                #"(?i)^(Key|Commands?|Functions?|Variables?|User Options?):\s*(.+)$"
                tag)]
      {:tag tag
       :prefix (str/lower-case prefix)
       :term (normalized-text term)})))

(defn semantic-item?
  "True when NODE has one of the explicit reference description terms."
  [node]
  (boolean (tag-parts node)))

(defn page-index-code
  "Return a supported INDEX code from PAGE's Org root, or nil."
  [page]
  (let [code (some-> page :page/root-ast ast/props :INDEX ast/value-name
                     str/trim str/lower-case)]
    (when (contains? index-definitions code) code)))

(defn- direct-description [node]
  (or (some (fn [child]
              (when (and (ast/node? child) (= :paragraph (:type child)))
                (normalized-text child)))
            (ast/children node))
      (normalized-text node)
      ""))

(defn- split-symbols [term]
  (->> (str/split term #"\s+/\s+")
       (map normalized-text)
       (remove nil?)
       vec))

(defn- command-symbol? [value]
  ;; Lower-case, hyphenated Lisp symbols distinguish command annotations from
  ;; human qualifiers such as `(Emacs)`, `(Evil)`, and `(Emacs/Evil)`.
  (boolean (re-matches #"[a-z][^\s/()]*-[^\s/()]+" value)))

(defn- key-command-groups [term]
  (->> (re-seq #"[(]([^()]*)[)]" term)
       (map second)
       (filter #(some command-symbol? (split-symbols %)))
       vec))

(defn- key-commands [term]
  (->> (key-command-groups term)
       (mapcat split-symbols)
       (filter command-symbol?)
       vec))

(defn- key-label [term]
  (let [without-commands
        (reduce (fn [value group]
                  (str/replace value (str "(" group ")") ""))
                term
                (key-command-groups term))]
    (or (some-> without-commands
                normalized-text
                (str/replace #"\s*/\s*$" "")
                str/trim
                not-empty)
        term)))

(defn- reference-specs [node]
  (when-let [{:keys [prefix term]} (tag-parts node)]
    (case prefix
      "key"
      (into [{:reference/index-code "ky"
              :reference/type :key
              :reference/term (key-label term)
              :reference/origin :description-term}]
            (map (fn [command]
                   {:reference/index-code "fn"
                    :reference/type :command
                    :reference/term command
                    :reference/origin :key-command})
                 (key-commands term)))

      ("command" "commands")
      (mapv (fn [command]
              {:reference/index-code "fn"
               :reference/type :command
               :reference/term command
               :reference/origin :description-term})
            (split-symbols term))

      ("function" "functions")
      (mapv (fn [function]
              {:reference/index-code "fn"
               :reference/type :function
               :reference/term function
               :reference/origin :description-term})
            (split-symbols term))

      ("variable" "variables")
      (mapv (fn [variable]
              {:reference/index-code "vr"
               :reference/type :variable
               :reference/term variable
               :reference/origin :description-term})
            (split-symbols term))

      ("user option" "user options")
      (mapv (fn [option]
              {:reference/index-code "vr"
               :reference/type :user-option
               :reference/term option
               :reference/origin :description-term})
            (split-symbols term))

      [])))

(defn- anchor-base [prefix value]
  (some-> (str prefix " " value) anchor/fragment-id str/lower-case))

(defn- unique-anchor [seen scope base]
  (let [key [scope base]
        occurrence (inc (get seen key 0))]
    [(assoc seen key occurrence)
     (if (= 1 occurrence) base (str base "-" occurrence))]))

(defn- source-candidates [{:keys [documents pages] :as partition}]
  (let [page-by-id (into {} (map (juxt :page/id identity) pages))]
    (mapcat
     (fn [document]
       (let [source (get-in document [:source :path])]
         (keep (fn [{:keys [node path]}]
                 (when-let [specs (seq (reference-specs node))]
                   (when-let [page (get page-by-id (model/owner-id partition source path))]
                     {:document document
                      :node node
                      :path path
                      :page page
                      :tag (description-term node)
                      :description (direct-description node)
                      :specs (vec specs)})))
               (model/node-locations document))))
     documents)))

(defn- with-source-anchors [candidates]
  (:items
   (reduce
    (fn [{:keys [seen items]} {:keys [page tag] :as candidate}]
      (let [node (:node candidate)
            explicit (anchor/docs-anchor-id node)
            base (or explicit (anchor-base "ref-source" tag))
            [seen generated] (if explicit
                               [seen explicit]
                               (unique-anchor seen (:page/id page) base))]
        {:seen seen
         :items (conj items (assoc candidate
                                   :source-anchor generated
                                   :source-anchor-explicit? (boolean explicit)))}))
    {:seen {} :items []}
    candidates)))

(defn- source-entry [{:keys [document node path page tag source-anchor
                             source-anchor-explicit? specs]}]
  (let [p (ast/props node)
        source (get-in document [:source :path])]
    {:node-key (model/node-key source path)
     :node-type :item
     :source source
     :source-span (diagnostic/source-span document node)
     :page-id (:page/id page)
     :page-route (:page/route page)
     :id (:ID p)
     :custom-id (:CUSTOM_ID p)
     :title tag
     :anchor source-anchor
     :anchor-kind (if source-anchor-explicit?
                    (anchor/docs-anchor-kind node)
                    :generated-reference)
     :explicit-anchor? source-anchor-explicit?
     :href (str (:page/route page) "#" source-anchor)
     :reference/source? true
     :reference/specs specs}))

(defn- expanded-references [item entry]
  (mapv (fn [spec]
          (merge
           (select-keys entry [:node-key :source :source-span :page-id :page-route
                               :href :anchor])
           {:reference/tag (:tag item)
            :reference/description (:description item)
            :reference/page-title (get-in item [:page :page/title])}
           spec))
        (:specs item)))

(defn- reference-sort-key [entry]
  [(some-> entry :reference/term str/lower-case)
   (:page-route entry)
   (or (get-in entry [:source-span :begin])
       #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER))
   (:source entry)
   (pr-str (:node-key entry))])

(defn- with-index-anchors [code entries]
  (:items
   (reduce
    (fn [{:keys [seen items]} entry]
      (let [base (anchor-base (str "ref-index-" code) (:reference/term entry))
            [seen generated] (unique-anchor seen code base)]
        {:seen seen
         :items (conj items (assoc entry :reference/index-anchor generated))}))
    {:seen {} :items []}
    (sort-by reference-sort-key entries))))

(defn collect
  "Collect explicit reference terms and their addressable source items.

  Returns `:node-entries` for the global anchor index and deterministic vectors
  under `:references`, keyed by the INDEX codes `ky`, `fn`, and `vr`."
  [partition]
  (let [items (with-source-anchors (source-candidates partition))
        node-entries (mapv (fn [item]
                             {:document (:document item)
                              :node (:node item)
                              :entry (source-entry item)})
                           items)
        references (mapcat (fn [item]
                             (expanded-references item (source-entry item)))
                           items)
        grouped (group-by :reference/index-code references)]
    {:node-entries node-entries
     :references (into (sorted-map)
                       (map (fn [code]
                              [code (vec (with-index-anchors code (get grouped code [])))])
                            (sort (keys index-definitions))))}))

(defn- reference-label-node [code term]
  (if (= "ky" code)
    [:kbd term]
    [:code term]))

(defn- reference-row [code entry]
  (let [term (:reference/term entry)
        description (:reference/description entry)]
    [[:dt {:id (:reference/index-anchor entry)
           :class "org-reference-term"
           :data-reference-identity term
           :data-reference-type (name (:reference/type entry))}
      [:a {:href (:href entry)} (reference-label-node code term)]]
     [:dd {:class "org-reference-description"}
      [:span (if (str/blank? description) "No description provided." description)]
      " "
      [:span {:class "org-reference-source"}
       "From "
       [:a {:href (:href entry)} (:reference/page-title entry)]]]]))

(defn index-section
  "Return generated semantic Hiccup for PAGE's INDEX property, or nil."
  [index page]
  (when-let [code (page-index-code page)]
    (let [entries (vec (get-in index [:references code] []))
          {:keys [label plural]} (get index-definitions code)]
      (into
       [:section {:class "org-reference-index"
                  :data-org-index code
                  :data-reference-count (count entries)}
        [:p {:class "org-reference-index-summary"}
         (str (count entries) " explicitly authored "
              (if (= 1 (count entries)) label plural) ".")]]
       (if (seq entries)
         [(into [:dl {:class "org-reference-list"}]
                (mapcat #(reference-row code %) entries))]
         [[:p {:class "org-reference-index-empty" :role "status"}
           (str "No explicitly authored " plural " were found.")]])))))
