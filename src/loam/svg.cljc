(ns loam.svg
  "Strict parser for the small, static SVG subset emitted by dvisvgm.

  This deliberately does not provide a general raw-HTML or raw-SVG escape
  hatch. Accepted XML is converted to ordinary Loam Hiccup before rendering."
  (:require [clojure.string :as str]))

(def allowed-tags
  #{"svg" "defs" "g" "path" "use" "rect" "line" "polyline" "polygon"
    "circle" "ellipse" "clipPath" "symbol"})

(def allowed-attrs
  #{"version" "xmlns" "xmlns:xlink" "id" "viewBox" "width" "height"
    "x" "y" "x1" "y1" "x2" "y2" "cx" "cy" "r" "rx" "ry"
    "d" "points" "transform" "fill" "stroke" "stroke-width"
    "stroke-linecap" "stroke-linejoin" "stroke-miterlimit" "opacity"
    "fill-opacity" "stroke-opacity" "fill-rule" "clip-rule" "clip-path"
    "href" "xlink:href" "preserveAspectRatio"})

(def attr-re
  #"(?s)([A-Za-z_:][A-Za-z0-9_.:-]*)\s*=\s*(?:'([^']*)'|\"([^\"]*)\")")

(defn- unsafe-value? [name value]
  (or (re-find #"[<>]" value)
      (and (= name "xmlns") (not= value "http://www.w3.org/2000/svg"))
      (and (= name "xmlns:xlink") (not= value "http://www.w3.org/1999/xlink"))
      (and (not (contains? #{"xmlns" "xmlns:xlink"} name))
           (re-find #"(?i)javascript:|data:|file:|https?:" value))
      (and (contains? #{"href" "xlink:href"} name)
           (not (str/starts-with? value "#")))
      (and (= "clip-path" name)
           (not (re-matches #"url\(#[A-Za-z0-9_.:-]+\)" value)))
      (and (not= "clip-path" name)
           (re-find #"(?i)url\(" value))))

(defn- parse-attrs [text]
  (let [matches (re-seq attr-re text)
        remainder (reduce (fn [remaining [full & _]]
                            (str/replace-first remaining full ""))
                          text matches)]
    (when-not (str/blank? remainder)
      (throw (ex-info "Unsupported SVG attribute syntax"
                      {:code :unsafe-svg-attribute-syntax
                       :text (str/trim remainder)})))
    (reduce
     (fn [attrs [_ name single-quoted double-quoted]]
       (let [value (or single-quoted double-quoted "")]
         (when (or (str/starts-with? (str/lower-case name) "on")
                   (not (contains? allowed-attrs name))
                   (unsafe-value? name value))
           (throw (ex-info "Unsafe or unsupported SVG attribute"
                           {:code :unsafe-svg-attribute
                            :attribute name})))
         (when (contains? attrs (keyword name))
           (throw (ex-info "Duplicate SVG attribute"
                           {:code :duplicate-svg-attribute
                            :attribute name})))
         (assoc attrs (keyword name) value)))
     {}
     matches)))

(defn- opening-tag [token]
  (when-let [[_ name body slash] (re-matches #"(?s)<([A-Za-z][A-Za-z0-9_.:-]*)(.*?)(/?)>" token)]
    {:name name
     :attrs (parse-attrs body)
     :self-closing? (= slash "/")}))

(defn- closing-tag [token]
  (some-> (re-matches #"</([A-Za-z][A-Za-z0-9_.:-]*)\s*>" token) second))

(defn- node->hiccup [{:keys [tag attrs children]}]
  (into [(keyword tag) attrs]
        (map #(if (map? %) (node->hiccup %) %) children)))

(defn- attach-node [{:keys [stack root] :as state} node]
  (if-let [parent (peek stack)]
    (assoc state :stack (conj (pop stack) (update parent :children conj node)))
    (do
      (when root
        (throw (ex-info "SVG must have one root element"
                        {:code :multiple-svg-roots})))
      (assoc state :root node))))

(defn parse-safe-svg
  "Parse SVG-TEXT into safe Hiccup or throw ExceptionInfo.

  Only the static geometry subset used by dvisvgm is accepted. External
  resources, executable content, CSS, event attributes, entities, and doctypes
  are rejected."
  [svg-text]
  (when-not (string? svg-text)
    (throw (ex-info "SVG preview must be text" {:code :invalid-svg})))
  (when (re-find #"(?is)<!DOCTYPE|<!ENTITY|<script\b|<style\b|<foreignObject\b|<image\b|<iframe\b|<object\b|<embed\b" svg-text)
    (throw (ex-info "Executable or external SVG content is forbidden"
                    {:code :unsafe-svg-content})))
  (let [clean (-> svg-text
                  (str/replace #"(?is)^\s*<\?xml[^>]*\?>" "")
                  (str/replace #"(?is)<!--.*?-->" "")
                  str/trim)
        tokens (re-seq #"(?s)<[^>]+>|[^<]+" clean)]
    (when-not (= clean (apply str tokens))
      (throw (ex-info "SVG tokenization failed" {:code :invalid-svg})))
    (let [{:keys [stack root]}
          (reduce
           (fn [{:keys [stack] :as state} token]
             (cond
               (str/starts-with? token "</")
               (let [name (closing-tag token)
                     node (peek stack)]
                 (when-not (and name node (= name (:tag node)))
                   (throw (ex-info "Mismatched SVG closing tag"
                                   {:code :invalid-svg-nesting :tag name})))
                 (attach-node (assoc state :stack (pop stack)) node))

               (str/starts-with? token "<")
               (let [{:keys [name attrs self-closing?] :as parsed} (opening-tag token)]
                 (when-not parsed
                   (throw (ex-info "Unsupported SVG tag syntax"
                                   {:code :invalid-svg-tag})))
                 (when-not (contains? allowed-tags name)
                   (throw (ex-info "Unsupported SVG element"
                                   {:code :unsafe-svg-element :element name})))
                 (let [node {:tag name :attrs attrs :children []}]
                   (if self-closing?
                     (attach-node state node)
                     (update state :stack conj node))))

               (str/blank? token) state

               :else
               (if-let [node (peek stack)]
                 (assoc state :stack
                        (conj (pop stack) (update node :children conj token)))
                 (throw (ex-info "Text outside SVG root is forbidden"
                                 {:code :invalid-svg-text})))))
           {:stack [] :root nil}
           tokens)]
      (when (seq stack)
        (throw (ex-info "Unclosed SVG element" {:code :invalid-svg-nesting})))
      (when-not (and root (= "svg" (:tag root)))
        (throw (ex-info "SVG preview must have an svg root"
                        {:code :invalid-svg-root})))
      (node->hiccup root))))
