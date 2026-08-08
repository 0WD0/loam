(ns loam.docs.render
  "Strict semantic Org renderer for logical documentation pages.

  Every accepted node type has an explicit render/hide/reject/defer
  disposition. There is deliberately no unknown-node children fallback."
  (:require [clojure.string :as str]
            [loam.anchor :as anchor]
            [loam.ast :as ast]
            [loam.diagnostic :as diagnostic]
            [loam.docs.model :as model]
            [loam.docs.reference :as reference]
            [loam.html :as html]
            [loam.svg :as svg]))

(def allowed-special-blocks
  #{"note" "tip" "warning" "danger" "experimental" "compatibility"})

(def default-dispositions
  {:org-data :render
   :section :render
   :anonymous :render
   :paragraph :render
   :headline :render
   :bold :render
   :italic :render
   :underline :render
   :strike-through :render
   :code :render
   :verbatim :render
   :subscript :render
   :superscript :render
   :link :render
   :target :render
   :radio-target :render
   :entity :render
   :line-break :render
   :plain-list :render
   :item :render
   :src-block :render
   :example-block :render
   :fixed-width :render
   :quote-block :render
   :verse-block :render
   :center-block :render
   :special-block :render
   :horizontal-rule :render
   :table :render
   :table-row :render
   :table-cell :render
   :footnote-definition :render
   :footnote-reference :render
   :timestamp :render
   :macro :render
   :property-drawer :hide
   :node-property :hide
   :drawer :hide
   :planning :hide
   :clock :hide
   :keyword :hide
   :comment :hide
   :comment-block :hide
   :export-block :reject
   :export-snippet :reject
   :latex-fragment :render
   :latex-environment :render
   :citation :defer
   :citation-reference :defer
   :statistics-cookie :defer})

(declare render-node render-children report!)

(defn- hiccup
  ([tag children] (into [tag] children))
  ([tag attrs children] (into [tag attrs] children)))

(defn- node-value [node]
  (let [p (ast/props node)]
    (or (:value p) (:raw-value p) (ast/text node) "")))

(defn- emacs-face-class [face]
  (str "ef-" (str/replace (str face) #"[^A-Za-z0-9_-]" "-")))

(defn- font-lock-fragments
  "Render zero-based half-open Emacs font-lock RUNS over SOURCE.

  Invalid or overlapping runs are ignored rather than being allowed to alter
  source text. ox-edn remains the authority that produces these offsets."
  [source font-lock]
  (let [length (count source)
        runs (sort-by :start (:runs font-lock))]
    (loop [cursor 0
           runs runs
           output []]
      (if-let [{:keys [start end faces]} (first runs)]
        (if (and (integer? start)
                 (integer? end)
                 (<= cursor start)
                 (< start end)
                 (<= end length)
                 (seq faces))
          (let [faces (mapv str faces)
                output (cond-> output
                         (< cursor start) (conj (subs source cursor start))
                         true (conj [:span
                                     {:class (str/join " " (map emacs-face-class faces))
                                      :data-emacs-faces (str/join " " faces)}
                                     (subs source start end)]))]
            (recur end (rest runs) output))
          (recur cursor (rest runs) output))
        (cond-> output
          (< cursor length) (conj (subs source cursor length)))))))

(defn- latex-display? [node]
  (let [value (str/triml (node-value node))]
    (or (= :latex-environment (:type node))
        (str/starts-with? value "$$")
        (str/starts-with? value "\\[")
        (str/starts-with? value "\\begin{"))))

(defn- latex-svg-style [preview]
  (let [width (:width preview)]
    (str (when (number? width)
           (str "width:" width "em;"))
         "height:auto;")))

(defn- latex-wrapper-style [preview display?]
  (let [depth (:depth preview)]
    (when (and (not display?) (number? depth) (pos? depth))
      (str "vertical-align:-" depth "em;"))))

(defn- latex-svg-id-prefix [preview]
  (let [digest (:sha256 preview)]
    (str "latex-" (subs (or digest "000000000000") 0 (min 12 (count (or digest "000000000000")))) "-")))

(defn- namespace-svg-ref [prefix value]
  (cond
    (and (string? value) (str/starts-with? value "#"))
    (str "#" prefix (subs value 1))

    (and (string? value) (re-matches #"url\(#[A-Za-z0-9_.:-]+\)" value))
    (str "url(#" prefix (subs value 5 (dec (count value))) ")")

    :else value))

(defn- namespace-latex-svg [hiccup preview]
  (let [prefix (latex-svg-id-prefix preview)]
    (letfn [(rewrite [node]
              (if-not (vector? node)
                node
                (let [[tag maybe-attrs & body] node
                      attrs? (map? maybe-attrs)
                      attrs (if attrs? maybe-attrs {})
                      children (if attrs? body (cons maybe-attrs body))
                      attrs (cond-> attrs
                              (:id attrs) (update :id #(str prefix %))
                              (:href attrs) (update :href #(namespace-svg-ref prefix %))
                              (:xlink:href attrs) (update :xlink:href #(namespace-svg-ref prefix %))
                              (:clip-path attrs) (update :clip-path #(namespace-svg-ref prefix %)))]
                  (into [tag attrs] (map rewrite children)))))]
      (rewrite hiccup))))

(defn- prepare-latex-svg [preview display?]
  (let [[tag attrs & children]
        (namespace-latex-svg (svg/parse-safe-svg (:svg preview)) preview)
        attrs (-> attrs
                  (dissoc :width :height)
                  (assoc :class "org-latex-svg"
                         :aria-hidden "true"
                         :focusable "false"
                         :style (latex-svg-style preview)))]
    (into [tag attrs] children)))

(defn- latex-renderer [ctx node _path]
  (let [preview (:ox-edn/latex-preview (ast/props node))
        display? (latex-display? node)
        source (node-value node)]
    (cond
      (not (and (map? preview)
                (= "svg" (ast/value-name (:format preview)))
                (string? (:svg preview))))
      (do
        (report! ctx
                 (diagnostic/error :missing-latex-preview
                                   "LaTeX node has no ox-edn SVG preview metadata"
                                   {:phase :render
                                    :document (:document ctx)
                                    :node node}))
        [])

      :else
      (try
        (let [svg (prepare-latex-svg preview display?)
              attrs (cond-> {:class (str "org-latex "
                                          (if display?
                                            "org-latex-display"
                                            "org-latex-inline"))
                              :role "img"
                              :aria-label source
                              :data-latex-source source
                              :data-latex-display (if display? "true" "false")
                              :title "Copying this formula yields its LaTeX source"}
                      (latex-wrapper-style preview display?)
                      (assoc :style (latex-wrapper-style preview display?))
                      (:sha256 preview)
                      (assoc :data-latex-sha256 (:sha256 preview)))]
          [(hiccup :span attrs [svg])])
        (catch #?(:clj Exception :cljs :default) error
          (report! ctx
                   (diagnostic/error :unsafe-latex-svg
                                     "Generated LaTeX SVG failed strict sanitization"
                                     {:phase :render
                                      :document (:document ctx)
                                      :node node
                                      :data {:reason #?(:clj (ex-message error)
                                                        :cljs (str error))
                                             :code #?(:clj (:code (ex-data error))
                                                      :cljs nil)}}))
          [])))))

(defn- src-block-renderer [_ctx node _path]
  (let [props (ast/props node)
        language (:language props)
        source (node-value node)
        font-lock (:ox-edn/font-lock props)
        mode (:mode font-lock)
        provider (:provider font-lock)
        attrs (cond-> {}
                language (assoc :class (str "language-" language))
                provider (assoc :data-highlight-provider (ast/value-name provider))
                mode (assoc :data-emacs-mode mode))
        contents (if (and (= "emacs-font-lock" (ast/value-name provider))
                          (seq (:runs font-lock)))
                   (font-lock-fragments source font-lock)
                   [source])]
    [[:figure {:class "org-src-block"}
      [:pre (into [:code attrs] contents)]]]))

(defn- render-children [ctx node path]
  (mapcat (fn [[index child]]
            (let [child-path (conj path index)]
              (cond
                (string? child) [child]
                (not (ast/node? child)) [(str child)]
                (model/owned? (:partition ctx) (:page ctx) (:source ctx) child-path)
                (render-node ctx child child-path)
                :else [])))
          (map-indexed vector (ast/children node))))

(defn- report! [ctx value]
  (swap! (:diagnostics ctx) conj value)
  nil)

(defn- rendered-link [ctx node path]
  (let [resolution (get (:resolutions ctx) (model/node-key (:source ctx) path))
        label (let [children (seq (render-children ctx node path))]
                (or children [(or (:raw-link (ast/props node))
                                  (:path (ast/props node)) "")]))]
    (if-not resolution
      (do
        (report! ctx (diagnostic/error :missing-link-resolution
                                       "Link reached rendering without a resolver result"
                                       {:phase :render
                                        :document (:document ctx)
                                        :node node}))
        [])
      (let [{:keys [kind href attrs source-anchor]} resolution
            marker (when source-anchor
                     [:span {:id source-anchor :data-org-link-occurrence true}])]
        (case kind
          :asset [[:figure.org-image
                   [:img {:src href :alt (str/trim (ast/text (ast/children node)))}]]]
          :download [(hiccup :a (merge {:href href :download true} attrs) label)]
          (cond-> []
            marker (conj marker)
            true (conj (hiccup :a (merge {:href href} attrs) label))))))))

(defn- heading-depth [ctx node]
  (let [root-level (or (-> (:page ctx) :page/root-ast ast/props :level) 0)
        level (or (:level (ast/props node)) (inc root-level))]
    (max 2 (inc (- level root-level)))))

(def search-hidden-types
  #{:property-drawer :node-property :drawer :planning :clock :keyword
    :comment :comment-block :export-block :export-snippet})

(def search-value-types
  #{:code :verbatim :radio-target :macro
    :src-block :example-block :fixed-width :verse-block
    :latex-fragment :latex-environment})

(def search-container-types
  #{:org-data :section :headline :plain-list :item :quote-block :center-block
    :special-block :table :table-row :table-cell :footnote-definition})

(declare search-node-text)

(defn- normalize-search-text [value]
  (-> (or value "")
      (str/replace #"\s+" " ")
      str/trim))

(defn- search-children-text [node separator]
  (->> (ast/children node)
       (map search-node-text)
       (remove str/blank?)
       (str/join separator)))

(defn- search-node-text [node]
  (cond
    (nil? node) ""
    (string? node) node
    (not (ast/node? node)) (str node)
    (contains? search-hidden-types (:type node)) ""
    (= :headline (:type node)) ""
    (= :line-break (:type node)) "\n"
    (= :horizontal-rule (:type node)) "\n"
    (= :entity (:type node)) (or (:utf-8 (ast/props node))
                                  (:name (ast/props node))
                                  "")
    (= :link (:type node)) (let [label (search-children-text node "")]
                             (if (str/blank? label)
                               (or (:raw-link (ast/props node))
                                   (:path (ast/props node))
                                   "")
                               label))
    (= :timestamp (:type node)) (or (:raw-value (ast/props node))
                                     (:value (ast/props node))
                                     "")
    (= :footnote-reference (:type node)) (or (:label (ast/props node)) "*")
    (contains? search-value-types (:type node)) (node-value node)
    (= :paragraph (:type node)) (search-children-text node "")
    (contains? search-container-types (:type node)) (search-children-text node "\n")
    :else (search-children-text node "")))

(defn- local-search-text [node]
  (->> (ast/children node)
       (remove #(and (ast/node? %) (= :headline (:type %))))
       (map search-node-text)
       (remove str/blank?)
       (str/join "\n")
       normalize-search-text))

(declare render-secondary)

(defn- render-secondary-children [node]
  (mapcat render-secondary (ast/children node)))

(defn- render-secondary [value]
  (cond
    (nil? value) []
    (string? value) [value]
    (sequential? value) (mapcat render-secondary value)
    (not (ast/node? value)) [(str value)]
    :else
    (let [rendered
          (case (:type value)
            :anonymous (render-secondary-children value)
            :bold [(hiccup :strong (render-secondary-children value))]
            :italic [(hiccup :em (render-secondary-children value))]
            :underline [(hiccup :span {:class "org-underline"}
                                (render-secondary-children value))]
            :strike-through [(hiccup :del (render-secondary-children value))]
            :code [[:code (node-value value)]]
            :verbatim [[:code {:class "org-verbatim"} (node-value value)]]
            :subscript [(hiccup :sub (render-secondary-children value))]
            :superscript [(hiccup :sup (render-secondary-children value))]
            :entity [(or (:utf-8 (ast/props value)) (:name (ast/props value)) "")]
            :line-break [" "]
            :link (render-secondary-children value)
            [(ast/text value)])]
      (if-let [spacing (ast/post-blank-text value)]
        (conj (vec rendered) spacing)
        rendered))))

(defn- headline-renderer [ctx node path]
  (let [entry (get-in (:index ctx) [:entries (model/node-key (:source ctx) path)])
        depth (heading-depth ctx node)
        tag (keyword (str "h" (min 6 depth)))
        heading-attrs (cond-> {:id (:anchor entry)
                               :class "org-heading"}
                        (> depth 6) (assoc :aria-level depth))
        section-attrs (cond-> {:class "org-section"
                               :data-org-type "headline"}
                        (and (:development? ctx)
                             (get-in entry [:source-span :start :line]))
                        (assoc :data-source-line (get-in entry [:source-span :start :line])))
        title (or (seq (render-secondary (get-in node [:properties :title])))
                  [(or (ast/node-title node) "")])]
    [(into [:section section-attrs
            [tag heading-attrs
             [:a {:class "org-heading-anchor"
                  :href (anchor/with-fragment "" (:anchor entry))}
              title]]]
           (render-children ctx node path))]))

(defn- list-tag [node]
  (case (ast/value-name (:type (ast/props node)))
    "ordered" :ol
    "descriptive" :dl
    :ul))

(defn- keymap-attrs [node]
  (when-let [scopes (seq (reference/keymap-scopes node))]
    {:data-keymap-scope (str/join " " scopes)}))

(defn- reference-keymap-attrs [entry]
  (when-let [scopes (seq (:reference/scopes entry))]
    {:data-keymap-scope (str/join " " scopes)}))

(defn- prepend-inline-marker
  "Insert MARKER at the start of the first rendered paragraph in CONTENTS.

  Org checkboxes belong to the item marker, not to a separate block. Keeping
  the marker inside the first paragraph preserves that inline relationship in
  HTML. If an unusual item has no leading paragraph, fall back to a direct
  child so no semantic marker is lost."
  [contents marker]
  (if-not marker
    contents
    (if-let [first-node (first contents)]
      (if (and (vector? first-node) (= :p (first first-node)))
        (let [attrs? (map? (second first-node))
              prefix (if attrs?
                       [(first first-node) (second first-node) marker]
                       [(first first-node) marker])
              body (if attrs? (drop 2 first-node) (rest first-node))]
          (into [(into prefix body)] (rest contents)))
        (into [marker] contents))
      [marker])))

(defn- item-renderer [ctx node path]
  (let [p (ast/props node)
        entry (get-in (:index ctx) [:entries (model/node-key (:source ctx) path)])
        checkbox (case (ast/value-name (:checkbox p))
                   "on" [:input {:type "checkbox" :checked true :disabled true}]
                   "off" [:input {:type "checkbox" :disabled true}]
                   "trans" [:input {:type "checkbox"
                                    :class "org-checkbox-mixed"
                                    :data-indeterminate true
                                    :aria-checked "mixed"
                                    :disabled true}]
                   nil)
        contents (prepend-inline-marker (render-children ctx node path) checkbox)
        scoped-attrs (reference-keymap-attrs entry)]
    (if-let [tag (:tag p)]
      [(if (:reference/source? entry)
         (into [:dt (merge {:id (:anchor entry)
                            :class "org-reference-source-term"
                            :data-reference-source true}
                           scoped-attrs)]
               (render-secondary tag))
         (into [:dt (or scoped-attrs {})] (render-secondary tag)))
       (into [:dd (or scoped-attrs {})] contents)]
      [(into [:li] contents)])))

(defn- special-block-renderer [ctx node path]
  (let [kind (some-> (or (:block-type (ast/props node))
                         (:type (ast/props node))
                         (:name (ast/props node)))
                     ast/value-name
                     str/lower-case)]
    (if (contains? allowed-special-blocks kind)
      [(hiccup :aside (merge {:class (str "org-callout org-callout-" kind)
                              :data-org-block kind
                              :role (when (contains? #{"warning" "danger"} kind) "note")}
                             (keymap-attrs node))
               (render-children ctx node path))]
      (do
        (report! ctx
                 (diagnostic/error :rejected-special-block
                                   "Special block type is not in the docs allowlist"
                                   {:phase :render
                                    :document (:document ctx)
                                    :node node
                                    :data {:block-type kind}}))
        []))))

(defn- macro-renderer [ctx node _path]
  (let [p (ast/props node)
        name (some-> (or (:key p) (:name p)) ast/value-name str/lower-case)
        arguments (or (some-> (:args p) ast/text not-empty)
                      (:arguments p)
                      (:raw-value p)
                      (:value p)
                      "")]
    (if (= "kbd" name)
      [[:kbd arguments]]
      (do
        (report! ctx
                 (diagnostic/warning :deferred-macro
                                     "Macro is explicit but not implemented by the docs renderer"
                                     {:phase :render
                                      :document (:document ctx)
                                      :node node
                                      :data {:macro name}}))
        [[:span {:class "org-deferred" :data-org-deferred "macro"} arguments]]))))

(defn- table-renderer [ctx node path]
  (let [rows (vec (ast/children node))
        rule-index (first (keep-indexed
                           (fn [index row]
                             (when (and (ast/node? row)
                                        (= :table-row (:type row))
                                        (= "rule" (ast/value-name (:type (ast/props row)))))
                               index))
                           rows))
        render-rows (fn [start end header?]
                      (mapcat
                       (fn [index]
                         (let [row (nth rows index)]
                           (if (and (ast/node? row) (= :table-row (:type row)))
                             (render-node (assoc ctx :table-cell-tag (if header? :th :td))
                                          row (conj path index))
                             [])))
                       (range start end)))
        header (when (and rule-index (pos? rule-index))
                 (vec (render-rows 0 rule-index true)))
        body-start (if rule-index (inc rule-index) 0)
        body (vec (render-rows body-start (count rows) false))
        sections (cond-> []
                   (seq header) (conj (hiccup :thead header))
                   (seq body) (conj (hiccup :tbody body)))]
    [[:div {:class "org-table-scroll"}
      (hiccup :table sections)]]))

(defn default-renderers []
  {:org-data (fn [ctx node path] (render-children ctx node path))
   :section (fn [ctx node path] (render-children ctx node path))
   :anonymous (fn [ctx node path] (render-children ctx node path))
   :paragraph (fn [ctx node path] [(hiccup :p (merge {:class "org-paragraph"}
                                                     (keymap-attrs node))
                                              (render-children ctx node path))])
   :headline headline-renderer
   :bold (fn [ctx node path] [(hiccup :strong (render-children ctx node path))])
   :italic (fn [ctx node path] [(hiccup :em (render-children ctx node path))])
   :underline (fn [ctx node path] [(hiccup :span {:class "org-underline"}
                                               (render-children ctx node path))])
   :strike-through (fn [ctx node path] [(hiccup :del (render-children ctx node path))])
   :code (fn [_ node _] [[:code (node-value node)]])
   :verbatim (fn [_ node _] [[:code {:class "org-verbatim"} (node-value node)]])
   :subscript (fn [ctx node path] [(hiccup :sub (render-children ctx node path))])
   :superscript (fn [ctx node path] [(hiccup :sup (render-children ctx node path))])
   :link rendered-link
   :target (fn [ctx _ path]
             (let [entry (get-in (:index ctx) [:entries (model/node-key (:source ctx) path)])]
               [[:span {:id (:anchor entry) :data-org-target true}]]))
   :radio-target (fn [ctx node path]
                   (let [entry (get-in (:index ctx) [:entries (model/node-key (:source ctx) path)])]
                     [[:span {:id (:anchor entry) :data-org-radio-target true}
                       (or (:value (ast/props node)) "")]]))
   :entity (fn [_ node _] [(or (:utf-8 (ast/props node)) (:name (ast/props node)) "")])
   :line-break (fn [_ _ _] [[:br]])
   :plain-list (fn [ctx node path] [(hiccup (list-tag node) (or (keymap-attrs node) {})
                                           (render-children ctx node path))])
   :item item-renderer
   :latex-fragment latex-renderer
   :latex-environment latex-renderer
   :src-block src-block-renderer
   :example-block (fn [_ node _] [[:pre {:class "org-example"} (node-value node)]])
   :fixed-width (fn [_ node _] [[:pre {:class "org-fixed-width"} (node-value node)]])
   :quote-block (fn [ctx node path] [(hiccup :blockquote (render-children ctx node path))])
   :verse-block (fn [_ node _] [[:pre {:class "org-verse"} (node-value node)]])
   :center-block (fn [ctx node path] [(hiccup :div {:class "org-center"}
                                                 (render-children ctx node path))])
   :special-block special-block-renderer
   :horizontal-rule (fn [_ _ _] [[:hr]])
   :table table-renderer
   :table-row (fn [ctx node path]
                (if (= "rule" (ast/value-name (:type (ast/props node))))
                  []
                  [(hiccup :tr (render-children ctx node path))]))
   :table-cell (fn [ctx node path]
                 (let [tag (or (:table-cell-tag ctx) :td)
                       attrs (if (= :th tag) {:scope "col"} {})]
                   [(hiccup tag attrs (render-children ctx node path))]))
   :footnote-definition (fn [ctx node path]
                          [(hiccup :aside {:class "org-footnote"
                                          :data-footnote (:label (ast/props node))}
                                   (render-children ctx node path))])
   :footnote-reference (fn [_ node _]
                         [[:sup {:class "org-footnote-ref"}
                           (or (:label (ast/props node)) "*")]])
   :timestamp (fn [_ node _] [[:time (node-value node)]])
   :macro macro-renderer})

(defn- coverage-for-page [partition document page dispositions]
  (let [source (get-in page [:page/source :path])]
    (reduce
     (fn [report location]
       (if (model/owned? partition page source (:path location))
         (let [type (:type (:node location))
               disposition (get dispositions type :unknown)]
           (-> report
               (update-in [:node-types type] (fnil inc 0))
               (update-in [:dispositions disposition] (fnil inc 0))
               (update :locations conj (assoc location :disposition disposition))))
         report))
     {:node-types {} :dispositions {} :locations []}
     (model/node-locations document))))

(defn- disposition-diagnostics [coverage document]
  (mapcat
   (fn [{:keys [node disposition]}]
     (case disposition
       :unknown [(diagnostic/error :unknown-node-type
                                   (str "No docs renderer disposition exists for node type "
                                        (:type node))
                                   {:phase :render
                                    :document document
                                    :node node
                                    :data {:node-type (:type node)}})]
       :reject [(diagnostic/error :rejected-node-type
                                  "Node type is explicitly rejected by the docs profile"
                                  {:phase :render :document document :node node})]
       :defer [(diagnostic/warning :deferred-node-type
                                   "Node type is explicitly deferred and makes the artifact unreleasable"
                                   {:phase :render :document document :node node})]
       []))
   (:locations coverage)))

(defn- render-node [ctx node path]
  (let [type (:type node)
        disposition (get (:dispositions ctx) type :unknown)
        rendered (case disposition
                   :render (if-let [renderer (get (:renderers ctx) type)]
                             (renderer ctx node path)
                             (do
                               (report! ctx (diagnostic/error :missing-renderer
                                                              "Render disposition has no renderer function"
                                                              {:phase :render
                                                               :document (:document ctx)
                                                               :node node}))
                               []))
                   :hide []
                   :defer [[:span {:class "org-deferred" :data-org-deferred (name type)}]]
                   ;; Unknown/reject diagnostics are emitted by the coverage pass.
                   [])
        spacing (ast/post-blank-text node)]
    (if spacing
      (conj (vec rendered) spacing)
      rendered)))

(defn render-page
  [partition index resolutions document page opts]
  (let [dispositions (merge default-dispositions (:dispositions opts))
        renderers (merge (default-renderers) (:renderers opts))
        coverage (coverage-for-page partition document page dispositions)
        coverage-diagnostics (vec (disposition-diagnostics coverage document))
        collector (atom coverage-diagnostics)
        source (get-in page [:page/source :path])
        ctx {:page page
             :document document
             :index index
             :partition partition
             :resolutions resolutions
             :source source
             :renderers renderers
             :dispositions dispositions
             :diagnostics collector
             :profile (or (:profile opts) :loam/docs)
             :development? (:development? opts)}
        root (:page/root-ast page)
        root-path (:page/root-path page)
        reference-section (reference/index-section index page)
        body (vec (render-children ctx root root-path))
        body (cond-> body
               reference-section (conj reference-section))
        article (into [:article {:class "org-document"
                                 :data-page-id (:page/id page)}]
                      body)
        diagnostics @collector
        heading-locations (->> (:locations coverage)
                               (filter #(and (= :headline (:type (:node %)))
                                             (not= (:path %) root-path)))
                               vec)
        page-outline-depth (count (:page/outline-path page))
        headings (mapv (fn [{:keys [node path outline-path]}]
                         (let [entry (get-in index [:entries (model/node-key source path)])]
                           {:depth (heading-depth ctx node)
                            :slug (:anchor entry)
                            :text (ast/node-title node)
                            :outlinePath (vec (drop page-outline-depth outline-path))}))
                       heading-locations)
        search-sections (mapv (fn [{:keys [node path]}]
                                (let [entry (get-in index [:entries (model/node-key source path)])]
                                  {:depth (heading-depth ctx node)
                                   :slug (:anchor entry)
                                   :title (ast/node-title node)
                                   :text (local-search-text node)}))
                              heading-locations)]
    {:page-id (:page/id page)
     :html (when-not (some diagnostic/error? diagnostics)
             (html/render-canonical-html article))
     :headings headings
     :search {:text (local-search-text root)
              :sections search-sections}
     :coverage (dissoc coverage :locations)
     :diagnostics diagnostics
     :deferred? (pos? (get-in coverage [:dispositions :defer] 0))}))

(defn render-pages
  "Render pages as HTML fragments. Unknown/reject nodes fail explicitly; defer
  nodes are visible diagnostics and mark the result unreleasable."
  [{:keys [documents pages] :as partition} index resolutions opts]
  (let [documents-by-source (into {} (map (juxt #(get-in % [:source :path]) identity)
                                           documents))
        rendered (mapv (fn [page]
                         (render-page partition index resolutions
                                      (get documents-by-source (get-in page [:page/source :path]))
                                      page opts))
                       pages)]
    {:rendered-pages rendered
     :diagnostics (vec (mapcat :diagnostics rendered))
     :deferred? (boolean (some :deferred? rendered))
     :coverage {:pages (into {} (map (juxt :page-id :coverage) rendered))
                                :totals (reduce (fn [totals page]
                                  (-> totals
                                      (update :node-types #(merge-with + % (get-in page [:coverage :node-types])))
                                      (update :dispositions #(merge-with + % (get-in page [:coverage :dispositions])))))
                                {:node-types {} :dispositions {}}
                                rendered)}}))
