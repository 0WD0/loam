(ns loam.docs.render
  "Strict semantic Org renderer for logical documentation pages.

  Every accepted node type has an explicit render/hide/reject/defer
  disposition. There is deliberately no unknown-node children fallback."
  (:require [clojure.string :as str]
            [loam.ast :as ast]
            [loam.diagnostic :as diagnostic]
            [loam.docs.model :as model]
            [loam.docs.reference :as reference]
            [loam.html :as html]))

(def allowed-special-blocks
  #{"note" "tip" "warning" "danger" "experimental" "compatibility"})

(def inline-node-types
  #{:bold :citation :citation-reference :code :entity :export-snippet
    :footnote-reference :italic :latex-fragment :line-break :link :macro
    :radio-target :statistics-cookie :strike-through :subscript :superscript
    :target :timestamp :underline :verbatim})

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
   :latex-fragment :defer
   :citation :defer
   :citation-reference :defer
   :statistics-cookie :defer})

(declare render-node render-children)

(defn- hiccup
  ([tag children] (into [tag] children))
  ([tag attrs children] (into [tag attrs] children)))

(defn- node-value [node]
  (let [p (ast/props node)]
    (or (:value p) (:raw-value p) (ast/text node) "")))

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
      (let [{:keys [kind href attrs]} resolution]
        (case kind
          :asset [[:figure.org-image
                   [:img {:src href :alt (str/trim (ast/text (ast/children node)))}]]]
          :download [(hiccup :a (merge {:href href :download true} attrs) label)]
          [(hiccup :a (merge {:href href} attrs) label)])))))

(defn- heading-depth [ctx node]
  (let [root-level (or (-> (:page ctx) :page/root-ast ast/props :level) 0)
        level (or (:level (ast/props node)) (inc root-level))]
    (max 2 (inc (- level root-level)))))

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
        title (or (ast/node-title node) "")]
    [(into [:section section-attrs
            [tag heading-attrs
             [:a {:class "org-heading-anchor"
                  :href (str "#" (:anchor entry))}
              title]]]
           (render-children ctx node path))]))

(defn- list-tag [node]
  (case (ast/value-name (:type (ast/props node)))
    "ordered" :ol
    "descriptive" :dl
    :ul))

(defn- item-renderer [ctx node path]
  (let [p (ast/props node)
        entry (get-in (:index ctx) [:entries (model/node-key (:source ctx) path)])
        checkbox (case (ast/value-name (:checkbox p))
                   "on" [:input {:type "checkbox" :checked true :disabled true}]
                   "off" [:input {:type "checkbox" :disabled true}]
                   "trans" [:input {:type "checkbox" :data-indeterminate true :disabled true}]
                   nil)
        children (cond-> [] checkbox (conj checkbox))
        contents (render-children ctx node path)]
    (if-let [tag (:tag p)]
      [(if (:reference/source? entry)
         [:dt {:id (:anchor entry)
               :class "org-reference-source-term"
               :data-reference-source true}
          (ast/text tag)]
         [:dt (ast/text tag)])
       (into [:dd] (concat children contents))]
      [(into [:li] (concat children contents))])))

(defn- special-block-renderer [ctx node path]
  (let [kind (some-> (or (:block-type (ast/props node))
                         (:type (ast/props node))
                         (:name (ast/props node)))
                     ast/value-name
                     str/lower-case)]
    (if (contains? allowed-special-blocks kind)
      [(hiccup :aside {:class (str "org-callout org-callout-" kind)
                       :data-org-block kind
                       :role (when (contains? #{"warning" "danger"} kind) "note")}
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
        value (or (:value p) (:arguments p) (:raw-value p) "")]
    (if (= "kbd" name)
      [[:kbd value]]
      (do
        (report! ctx
                 (diagnostic/warning :deferred-macro
                                     "Macro is explicit but not implemented by the docs renderer"
                                     {:phase :render
                                      :document (:document ctx)
                                      :node node
                                      :data {:macro name}}))
        [[:span {:class "org-deferred" :data-org-deferred "macro"} value]]))))

(defn default-renderers []
  {:org-data (fn [ctx node path] (render-children ctx node path))
   :section (fn [ctx node path] (render-children ctx node path))
   :anonymous (fn [ctx node path] (render-children ctx node path))
   :paragraph (fn [ctx node path] [(hiccup :p {:class "org-paragraph"}
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
   :plain-list (fn [ctx node path] [(hiccup (list-tag node) (render-children ctx node path))])
   :item item-renderer
   :src-block (fn [_ node _]
                (let [language (:language (ast/props node))]
                  [[:figure {:class "org-src-block"}
                    [:pre [:code (cond-> {} language (assoc :class (str "language-" language)))
                           (node-value node)]]]]))
   :example-block (fn [_ node _] [[:pre {:class "org-example"} (node-value node)]])
   :fixed-width (fn [_ node _] [[:pre {:class "org-fixed-width"} (node-value node)]])
   :quote-block (fn [ctx node path] [(hiccup :blockquote (render-children ctx node path))])
   :verse-block (fn [_ node _] [[:pre {:class "org-verse"} (node-value node)]])
   :center-block (fn [ctx node path] [(hiccup :div {:class "org-center"}
                                                 (render-children ctx node path))])
   :special-block special-block-renderer
   :horizontal-rule (fn [_ _ _] [[:hr]])
   :table (fn [ctx node path]
            [[:div {:class "org-table-scroll"}
              (hiccup :table (render-children ctx node path))]])
   :table-row (fn [ctx node path]
                (if (= "rule" (ast/value-name (:type (ast/props node))))
                  []
                  [(hiccup :tr (render-children ctx node path))]))
   :table-cell (fn [ctx node path] [(hiccup :td (render-children ctx node path))])
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
        post-blank (get-in node [:properties :post-blank])]
    (if (and (contains? inline-node-types type)
             (integer? post-blank)
             (pos? post-blank))
      (conj (vec rendered) (apply str (repeat post-blank " ")))
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
        headings (->> (:locations coverage)
                      (filter #(and (= :headline (:type (:node %)))
                                    (not= (:path %) root-path)))
                      (mapv (fn [{:keys [node path]}]
                              (let [entry (get-in index [:entries (model/node-key source path)])]
                                {:depth (heading-depth ctx node)
                                 :slug (:anchor entry)
                                 :text (ast/node-title node)}))))]
    {:page-id (:page/id page)
     :html (when-not (some diagnostic/error? diagnostics)
             (html/render-canonical-html article))
     :headings headings
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
