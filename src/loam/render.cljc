(ns loam.render
  "Registry-based AST to Hiccup renderer."
  (:require [clojure.string :as str]
            [loam.ast :as ast]))

(declare render-node render-inline render-block)

(def default-inline-types
  #{:anonymous :bold :italic :underline :strike-through :code :verbatim
    :subscript :superscript :link :target :radio-target :entity
    :latex-fragment :export-snippet :macro :line-break :statistics-cookie
    :citation :citation-reference})

(defn hiccup
  ([tag children]
   (into [tag] children))
  ([tag attrs children]
   (into [tag attrs] children)))

(defn render-children [ctx node]
  (mapcat #(render-node ctx %) (ast/children node)))

(defn text-content [node]
  (apply str (map #(if (string? %) % (or (-> % ast/props :value) "")) (ast/children node))))

(defn compact [xs]
  (remove nil? xs))

(defn target-id [node]
  (let [p (ast/props node)]
    (or (:ID p)
        (:CUSTOM_ID p)
        (:value p)
        (:raw-value p))))

(defn- resolved-fragment [link-props]
  (let [{:keys [resolved-id resolved-custom-id resolved-value]} (:resolved link-props)]
    (or resolved-id resolved-custom-id resolved-value)))

(defn- fragment-href [fragment]
  (some-> fragment str (str/replace #"\s+" "-") (->> (str "#"))))

(defn default-link-href [link-props]
  (let [{:keys [type path raw-link]} link-props]
    (or (fragment-href (resolved-fragment link-props))
        (case type
          ("http" "https" "ftp") (str type ":" path)
          "file" path
          "attachment" raw-link
          ("id" "custom-id" "fuzzy" "radio") (fragment-href path)
          raw-link))))

(defn link-href [ctx link-props]
  (if-let [resolve-link (:resolve-link ctx)]
    (or (resolve-link link-props)
        (default-link-href link-props))
    (default-link-href link-props)))

(defn render-link-label [ctx node]
  (let [rendered (seq (render-children ctx node))]
    (or rendered
        [(or (:raw-link (ast/props node)) (:path (ast/props node)) "")])))

(defn render-list-item [ctx node]
  (let [checkbox (:checkbox (ast/props node))
        checkbox-node (case (ast/value-name checkbox)
                        "on" [:input {:type "checkbox" :checked true :disabled true}]
                        "off" [:input {:type "checkbox" :disabled true}]
                        "trans" [:input {:type "checkbox" :data-indeterminate true :disabled true}]
                        nil)]
    [(hiccup :li (compact (cons checkbox-node (render-children ctx node))))]))

(defn list-tag [node]
  (case (ast/value-name (:type (ast/props node)))
    "ordered" :ol
    "descriptive" :dl
    :ul))

(defn headline-tag [level]
  (keyword (str "h" (min 6 (max 1 (or level 1))))))

(defn headline-attrs [node]
  (let [id (target-id node)
        tags (:tags (ast/props node))]
    (cond-> {}
      id (assoc :id id)
      (seq tags) (assoc :data-tags (str/join " " tags)))))

(defn unknown-renderer [ctx node]
  (render-children ctx node))

(defn default-renderers []
  {:anonymous (fn [ctx node] (render-children ctx node))
   :bold (fn [ctx node] [(hiccup :strong (render-children ctx node))])
   :italic (fn [ctx node] [(hiccup :em (render-children ctx node))])
   :underline (fn [ctx node] [(hiccup :span.underline (render-children ctx node))])
   :strike-through (fn [ctx node] [(hiccup :del (render-children ctx node))])
   :code (fn [_ node] [[:code (text-content node)]])
   :verbatim (fn [_ node] [[:code.verbatim (text-content node)]])
   :subscript (fn [ctx node] [(hiccup :sub (render-children ctx node))])
   :superscript (fn [ctx node] [(hiccup :sup (render-children ctx node))])
   :link (fn [ctx node] [(hiccup :a {:href (link-href ctx (ast/props node))}
                                (render-link-label ctx node))])
   :target (fn [_ node] [[:span {:id (target-id node) :data-org-target true}]])
   :radio-target (fn [_ node] [[:span {:id (target-id node) :data-org-radio-target true}
                                (or (-> node ast/props :value) "")]])
   :entity (fn [_ node] [(or (-> node ast/props :utf-8) (-> node ast/props :name) "")])
   :latex-fragment (fn [_ node] [[:span.math (or (-> node ast/props :value) "")]])
   :export-snippet (fn [_ _] [])
   :macro (fn [_ node] [(or (-> node ast/props :raw-value) "")])
   :line-break (fn [_ _] [[:br]])
   :statistics-cookie (fn [_ node] [(or (-> node ast/props :value) "")])
   :citation (fn [ctx node] [(hiccup :span.citation (render-children ctx node))])
   :citation-reference (fn [_ node] [(or (-> node ast/props :key) "")])
   :org-data (fn [ctx node] (render-children ctx node))
   :section (fn [ctx node] (render-children ctx node))
   :paragraph (fn [ctx node] [(hiccup :p (render-children ctx node))])
   :headline (fn [ctx node]
               (let [p (ast/props node)
                     title (:title p)
                     title-content (if title
                                     (render-inline ctx title)
                                     [(:raw-value p)])]
                 [(into [:section (headline-attrs node)
                         (hiccup (headline-tag (:level p)) title-content)]
                        (render-children ctx node))]))
   :plain-list (fn [ctx node] [(hiccup (list-tag node) (render-children ctx node))])
   :item render-list-item
   :src-block (fn [_ node] [[:pre [:code {:class (some-> node ast/props :language)}
                              (or (-> node ast/props :value) "")]]])
   :example-block (fn [_ node] [[:pre.example (or (-> node ast/props :value) "")]])
   :quote-block (fn [ctx node] [(hiccup :blockquote (render-children ctx node))])
   :verse-block (fn [_ node] [[:pre.verse (or (-> node ast/props :value) (text-content node))]])
   :center-block (fn [ctx node] [(hiccup :div.center (render-children ctx node))])
   :special-block (fn [ctx node] [(hiccup :div.special-block (render-children ctx node))])
   :export-block (fn [_ _] [])
   :comment (fn [_ _] [])
   :comment-block (fn [_ _] [])
   :property-drawer (fn [_ _] [])
   :drawer (fn [_ _] [])
   :planning (fn [_ _] [])
   :clock (fn [_ _] [])
   :keyword (fn [_ _] [])
   :horizontal-rule (fn [_ _] [[:hr]])
   :fixed-width (fn [_ node] [[:pre.fixed-width (or (-> node ast/props :value) "")]])
   :table (fn [ctx node] [[:table (hiccup :tbody (render-children ctx node))]])
   :table-row (fn [ctx node] [(hiccup :tr (render-children ctx node))])
   :table-cell (fn [ctx node] [(hiccup :td (render-children ctx node))])
   :footnote-definition (fn [ctx node] [(hiccup :aside.footnote (render-children ctx node))])
   :footnote-reference (fn [_ node] [[:sup.footnote-ref (or (-> node ast/props :label) "*")]])
   :timestamp (fn [_ node] [[:time (or (-> node ast/props :raw-value) (-> node ast/props :value) "")]])})

(defn render-inline [ctx node]
  (if (string? node)
    [node]
    (let [renderer (get (:renderers ctx) (:type node) unknown-renderer)]
      (renderer ctx node))))

(defn render-block [ctx node]
  (if (string? node)
    [node]
    (let [renderer (get (:renderers ctx) (:type node))]
      (if renderer
        (renderer ctx node)
        (if (contains? (:inline-types ctx) (:type node))
          (render-inline ctx node)
          (unknown-renderer ctx node))))))

(defn render-node
  ([node] (render-node {:renderers (default-renderers)
                        :inline-types default-inline-types} node))
  ([ctx node] (render-block ctx node)))

(defn render-document
  ([node] (render-document {} node))
  ([ctx node]
   (let [ctx (merge {:renderers (default-renderers)
                     :inline-types default-inline-types}
                    ctx)]
     (into [:div.loam-document] (render-node ctx node)))))

(def extension
  {:id :loam/render
   :renderers (default-renderers)
   :inline-types default-inline-types})
