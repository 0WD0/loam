(ns loam.anchor
  "Canonical HTML anchor generation for Org/Loam nodes.

  Org has several different names for addressable things: ID,
  CUSTOM_ID, headline titles, targets, radio targets, and named blocks.
  Loam keeps those semantic values in indexes, but renders them through
  one canonical HTML id / URL fragment pipeline so generated href values
  and rendered id attributes always match."
  (:require [clojure.string :as str]
            [loam.ast :as ast]))

(defn normalize-space
  "Trim S and collapse internal whitespace to a single ASCII space."
  [s]
  (some-> s str str/trim (str/replace #"\s+" " ")))

(defn fragment-id
  "Return a stable HTML id / URL fragment for a human or Org identifier.

  This intentionally keeps non-ASCII text so Chinese/Japanese/etc. Org
  headings and targets remain readable.  Whitespace and path-ish
  separators become `-`; characters that commonly break fragments or CSS
  selectors are removed/collapsed."
  [value]
  (some-> value
          normalize-space
          (str/replace #"\s+" "-")
          (str/replace #"[<>\"'`#?&/\\]+" "-")
          (str/replace #"-+" "-")
          (str/replace #"^-+" "")
          (str/replace #"-+$" "")
          not-empty))

(defn fragment-href
  "Return `#fragment` for VALUE after canonicalization, or nil."
  [value]
  (some-> value fragment-id (->> (str "#"))))

(defn anchor-kind
  "Return the semantic anchor kind for NODE.

  The kind is informational; `anchor-id` canonicalizes all kinds through
  the same HTML fragment pipeline while indexes still keep the raw Org
  ID/CUSTOM_ID/target values separately."
  [node]
  (let [p (ast/props node)]
    (cond
      (:ID p) :id
      (:CUSTOM_ID p) :custom-id
      (= :target (:type node)) :target
      (= :radio-target (:type node)) :radio-target
      (= :headline (:type node)) :headline
      (or (:name p) (:NAME p)) :name
      (:value p) :value
      (:raw-value p) :raw-value
      :else nil)))

(defn node-anchor-value
  "Return NODE's raw Org anchor value, before HTML canonicalization."
  [node]
  (let [p (ast/props node)]
    (cond
      (:ID p) (:ID p)
      (:CUSTOM_ID p) (:CUSTOM_ID p)
      (= :target (:type node)) (:value p)
      (= :radio-target (:type node)) (:value p)
      (= :headline (:type node)) (ast/node-title node)
      (or (:name p) (:NAME p)) (or (:name p) (:NAME p))
      (:value p) (:value p)
      (:raw-value p) (:raw-value p)
      :else nil)))

(defn anchor-id
  "Return the canonical rendered HTML id for an anchor VALUE.

  Arity 2 accepts a semantic KIND for callers that want to be explicit;
  currently all kinds share the same fragment normalization."
  ([value]
   (fragment-id value))
  ([_kind value]
   (fragment-id value)))

(defn node-anchor-id
  "Return NODE's canonical rendered HTML id, or nil."
  [node]
  (when-let [value (node-anchor-value node)]
    (anchor-id (anchor-kind node) value)))

(defn resolved-anchor-id
  "Return the canonical anchor id described by a link `:resolved` map."
  [{:keys [resolved-id resolved-custom-id resolved-value resolved-type]}]
  (cond
    resolved-id (anchor-id :id resolved-id)
    resolved-custom-id (anchor-id :custom-id resolved-custom-id)
    resolved-value (anchor-id resolved-type resolved-value)
    :else nil))

(defn link-anchor-id
  "Return the canonical local anchor id for internal LINK-PROPS.

  External/file links return nil.  Resolved Org export metadata wins over
  raw link paths because it tells us whether a fuzzy link targeted a
  headline, target, or radio target."
  [link-props]
  (or (some-> link-props :resolved resolved-anchor-id)
      (let [{:keys [type path]} link-props]
        (case type
          "id" (anchor-id :id path)
          "custom-id" (anchor-id :custom-id path)
          "fuzzy" (anchor-id :fuzzy path)
          "radio" (anchor-id :radio-target path)
          nil))))

(defn local-href
  "Return a local `#...` href for internal LINK-PROPS, or nil."
  [link-props]
  (some-> link-props link-anchor-id (->> (str "#"))))

(defn canonical-title-id
  "Canonical generated anchor for a headline title in the docs compiler.

  Explicit Org identifiers retain their authored case; title-only anchors are
  lower-case slugs so their generated form is predictable."
  [title]
  (some-> title
          fragment-id
          str/lower-case
          (str/replace #"[()\[\]{}:;,!@$%^*+=|~]+" "-")
          (str/replace #"-+" "-")
          (str/replace #"^-+|-+$" "")
          not-empty))

(defn docs-anchor-kind
  "Anchor precedence for logical docs pages: CUSTOM_ID, ID, explicit target,
  then a generated canonical headline slug."
  [node]
  (let [p (ast/props node)]
    (cond
      (:CUSTOM_ID p) :custom-id
      (:ID p) :id
      (= :target (:type node)) :target
      (= :radio-target (:type node)) :radio-target
      (= :headline (:type node)) :generated-title
      (or (:name p) (:NAME p)) :name
      :else nil)))

(defn docs-anchor-id
  "Return the logical docs anchor for NODE using stable-anchor precedence."
  [node]
  (let [p (ast/props node)
        kind (docs-anchor-kind node)]
    (case kind
      :custom-id (anchor-id :custom-id (:CUSTOM_ID p))
      :id (anchor-id :id (:ID p))
      :target (anchor-id :target (:value p))
      :radio-target (anchor-id :radio-target (:value p))
      :generated-title (canonical-title-id (ast/node-title node))
      :name (anchor-id :name (or (:name p) (:NAME p)))
      nil)))

(defn explicit-docs-anchor?
  "True when NODE's docs anchor was explicitly authored rather than generated
  from a headline title."
  [node]
  (contains? #{:custom-id :id :target :radio-target :name}
             (docs-anchor-kind node)))
