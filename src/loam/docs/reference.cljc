(ns loam.docs.reference
  "Explicit reference entries extracted from authored Org description lists.

  Majutsu marks its generated reference pages with an INDEX property (`ky`,
  `fn`, or `vr`).  Their entries come only from description-list terms such as
  `Key: ...`, `Command: ...`, and `User Option: ...`; ordinary prose and
  monospace text are deliberately ignored.  A containing description list
  may use `#+attr_reference` to author binding context which cannot be
  represented faithfully in a compact description term."
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

(def reference-kinds
  #{:command-binding :transient-argument :key-binding})

(defn- attribute-values [value]
  (cond
    (string? value) [value]
    (ast/node? value) [(ast/text value)]
    (sequential? value) (mapcat attribute-values value)
    (nil? value) []
    :else [(str value)]))

(defn- reference-attribute-pairs [node]
  (let [props (some-> node ast/props)
        value (or (:attr_reference props)
                  (:ATTR_REFERENCE props))]
    (->> (attribute-values value)
         (mapcat #(re-seq #":([A-Za-z][A-Za-z0-9_-]*)\s+(?:\"([^\"]*)\"|'([^']*)'|([^\s]+))"
                          %))
         (map (fn [[_ key double-quoted single-quoted bare]]
                [(keyword (-> key str/lower-case (str/replace "_" "-")))
                 (or double-quoted single-quoted bare)])))))

(defn- editor-scopes [value]
  (->> (str/split (or value "") #"(?:\s*[,/]\s*|\s+)")
       (keep (fn [scope]
               (case (some-> scope normalized-text str/lower-case)
                 "emacs" "Emacs"
                 "evil" "Evil"
                 nil)))
       distinct
       vec))

(defn- reference-attributes [node]
  (let [attributes (into {} (reference-attribute-pairs node))
        kind (some-> (:kind attributes) str/lower-case keyword)
        interface (normalized-text (:interface attributes))
        mode (normalized-text (:mode attributes))
        prefix (normalized-text (:prefix attributes))
        state (normalized-text (:state attributes))
        scopes (editor-scopes (or (:scope attributes) (:scopes attributes)))]
    (cond-> {}
      (contains? reference-kinds kind) (assoc :kind kind)
      interface (assoc :interface interface)
      mode (assoc :mode mode)
      prefix (assoc :prefix prefix)
      state (assoc :state state)
      (seq scopes) (assoc :scopes scopes))))

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

(defn- key-parenthetical-groups [term]
  (->> (re-seq #"[(]([^()]*)[)]" term)
       (map second)
       vec))

(defn- key-command-groups [term]
  (->> (key-parenthetical-groups term)
       (filter #(some command-symbol? (split-symbols %)))
       vec))

(defn- key-commands [term]
  (->> (key-command-groups term)
       (mapcat split-symbols)
       (filter command-symbol?)
       vec))

(defn- key-scopes [term]
  (->> (key-parenthetical-groups term)
       (remove #(some command-symbol? (split-symbols %)))
       (mapcat #(str/split % #"\s*/\s*"))
       (keep (fn [scope]
               (case (some-> scope normalized-text str/lower-case)
                 "emacs" "Emacs"
                 "evil" "Evil"
                 nil)))
       distinct
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
      (let [commands (key-commands term)]
        (into [{:reference/index-code "ky"
                :reference/type :key
                :reference/term (key-label term)
                :reference/commands commands
                :reference/scopes (key-scopes term)
                :reference/origin :description-term}]
              (map (fn [command]
                     {:reference/index-code "fn"
                      :reference/type :command
                      :reference/term command
                      :reference/origin :key-command})
                   commands)))

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
  (anchor/ascii-slug (str prefix " " value)))

(defn- unique-anchor [seen scope base]
  (let [key [scope base]
        occurrence (inc (get seen key 0))]
    [(assoc seen key occurrence)
     (if (= 1 occurrence) base (str base "-" occurrence))]))

(defn- source-candidates [{:keys [documents pages] :as partition}]
  (let [page-by-id (into {} (map (juxt :page/id identity) pages))]
    (mapcat
     (fn [document]
       (let [source (get-in document [:source :path])
             locations (vec (model/node-locations document))
             nodes-by-path (into {} (map (juxt :path :node) locations))]
         (keep (fn [{:keys [node path ancestors outline-path]}]
                 (when-let [specs (seq (reference-specs node))]
                   (when-let [page (get page-by-id (model/owner-id partition source path))]
                     (let [page-outline (vec (:page/outline-path page))
                           outline (vec outline-path)
                           authored-attributes
                           (reduce (fn [attributes ancestor-path]
                                     (let [ancestor (get nodes-by-path ancestor-path)]
                                       (if (= :plain-list (:type ancestor))
                                         (merge attributes (reference-attributes ancestor))
                                         attributes)))
                                   {}
                                   ancestors)
                           nested (if (and (<= (count page-outline) (count outline))
                                           (= page-outline
                                              (subvec outline 0 (count page-outline))))
                                    (subvec outline (count page-outline))
                                    [])]
                       {:document document
                        :node node
                        :path path
                        :page page
                        :tag (description-term node)
                        :description (direct-description node)
                        :context-path (into [(:page/title page)] nested)
                        :reference-attributes authored-attributes
                        :specs (vec specs)}))))
               locations)))
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
     :href (anchor/with-fragment (:page/route page) source-anchor)
     :reference/source? true
     :reference/specs specs}))

(defn- expanded-references [item entry]
  (let [{:keys [kind interface mode prefix state scopes]}
        (:reference-attributes item)]
    (mapv (fn [spec]
            (merge
             (select-keys entry [:node-key :source :source-span :page-id :page-route
                                 :href :anchor])
             (cond-> {:reference/tag (:tag item)
                      :reference/description (:description item)
                      :reference/page-title (get-in item [:page :page/title])
                      :reference/context-path (:context-path item)}
               kind (assoc :reference/kind kind)
               interface (assoc :reference/interface interface)
               mode (assoc :reference/mode mode)
               prefix (assoc :reference/prefix prefix)
               state (assoc :reference/state state))
             (cond-> spec
               (seq scopes) (update :reference/scopes
                                    #(vec (distinct (concat (or % []) scopes)))))))
          (:specs item))))

(defn- reference-sort-key [entry]
  [(some-> entry :reference/term str/lower-case)
   (:reference/context-path entry)
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

(defn- transient-key-term? [term]
  (->> (str/split term #"\s+/\s+")
       (some #(str/starts-with? (str/trim %) "-"))
       boolean))

(defn- transient-context? [context-path]
  (boolean
   (some #(re-find #"(?i)\btransient\b" %) context-path)))

(defn- key-reference-kind [{:reference/keys [kind commands context-path term]}]
  (or kind
      (cond
        (seq commands) :command-binding
        (or (transient-key-term? term)
            (transient-context? context-path)) :transient-argument
        :else :key-binding)))

(def key-kind-labels
  {:command-binding "Command binding"
   :transient-argument "Transient argument"
   :key-binding "Key binding"})

(defn- context-fact [entry]
  (let [context (str/join " › " (:reference/context-path entry))]
    [:li {:class "org-reference-fact org-reference-fact-context"}
     [:span {:class "org-reference-fact-label"} "Context"]
     [:a {:href (:href entry)} context]]))

(defn- kind-fact [kind]
  (let [label (get key-kind-labels kind)]
    [:li {:class "org-reference-fact"}
     [:span {:class "org-reference-fact-label"} "Kind"]
     [:span label]]))

(defn- scope-fact [scopes]
  (let [value (str/join ", " scopes)]
    [:li {:class "org-reference-fact"}
     [:span {:class "org-reference-fact-label"} "Scope"]
     [:span value]]))

(defn- interface-fact [interface]
  [:li {:class "org-reference-fact org-reference-fact-interface"}
   [:span {:class "org-reference-fact-label"} "Interface"]
   [:span interface]])

(defn- mode-fact [mode]
  [:li {:class "org-reference-fact org-reference-fact-mode"}
   [:span {:class "org-reference-fact-label"} "Mode"]
   [:code mode]])

(defn- prefix-fact [prefix]
  [:li {:class "org-reference-fact org-reference-fact-prefix"}
   [:span {:class "org-reference-fact-label"} "Prefix"]
   [:kbd prefix]])

(defn- state-fact [state]
  [:li {:class "org-reference-fact org-reference-fact-state"}
   [:span {:class "org-reference-fact-label"} "State"]
   [:span state]])

(defn- command-fact [commands]
  (let [label (if (= 1 (count commands)) "Command" "Commands")
        value (str/join ", " commands)
        rendered (interpose
                  [:span {:class "org-reference-command-separator"
                          :aria-hidden true} ","]
                  (map #(vector :code %) commands))]
    [:li {:class "org-reference-fact org-reference-fact-command"
          :aria-label (str label ": " value)}
     [:span {:class "org-reference-fact-label"} label]
     (into [:span {:class "org-reference-fact-value org-reference-command-list"}]
           rendered)]))

(defn- key-reference-row [entry]
  (let [term (:reference/term entry)
        description (:reference/description entry)
        commands (:reference/commands entry)
        scopes (:reference/scopes entry)
        interface (:reference/interface entry)
        mode (:reference/mode entry)
        prefix (:reference/prefix entry)
        state (:reference/state entry)
        kind (key-reference-kind entry)
        context (str/join " › " (:reference/context-path entry))
        attrs (cond-> {:id (:reference/index-anchor entry)
                       :class "org-reference-term"
                       :data-reference-identity term
                       :data-reference-type (name (:reference/type entry))
                       :data-reference-context context
                       :data-reference-kind (name kind)}
                (seq commands) (assoc :data-reference-command
                                      (str/join " " commands))
                (seq scopes) (assoc :data-reference-scope
                                    (str/join " " scopes))
                interface (assoc :data-reference-interface interface)
                mode (assoc :data-reference-mode mode)
                prefix (assoc :data-reference-prefix prefix)
                state (assoc :data-reference-state state))
        identity-facts (cond-> [(kind-fact kind)]
                         interface (conj (interface-fact interface))
                         mode (conj (mode-fact mode))
                         (seq scopes) (conj (scope-fact scopes))
                         prefix (conj (prefix-fact prefix))
                         state (conj (state-fact state))
                         (seq commands) (conj (command-fact commands)))]
    [[:dt attrs
      [:a {:class "org-reference-key" :href (:href entry)}
       (reference-label-node "ky" term)]
      (into [:ul {:class "org-reference-facts org-reference-key-facts"
                  :aria-label "Binding identity"}]
            identity-facts)]
     [:dd {:class "org-reference-description"}
      [:span {:class "org-reference-description-text"}
       (if (str/blank? description) "No description provided." description)]
      [:ul {:class "org-reference-facts org-reference-context-facts"
            :aria-label "Binding context"}
       (context-fact entry)]]]))

(defn- reference-row [code entry]
  (if (= "ky" code)
    (key-reference-row entry)
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
         [:a {:href (:href entry)} (:reference/page-title entry)]]]])))

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
              (if (= 1 (count entries)) label plural) "."
              (when (= "ky" code)
                " Each entry includes its section context, binding kind, and any authored interface, mode, prefix, state, command, or editor scope."))]]
       (if (seq entries)
         [(into [:dl {:class "org-reference-list"}]
                (mapcat #(reference-row code %) entries))]
         [[:p {:class "org-reference-index-empty" :role "status"}
           (str "No explicitly authored " plural " were found.")]])))))
