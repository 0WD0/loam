(ns loam.html
  "Small Hiccup to HTML renderer."
  (:require [clojure.string :as str]))

(def void-tags
  #{"area" "base" "br" "col" "embed" "hr" "img" "input" "link"
    "meta" "param" "source" "track" "wbr"})

(defn escape-html [x]
  (-> (str x)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(defn- parse-tag [tag]
  (let [raw (name tag)
        [_ tag-name id classes] (re-matches #"([^.#]+)(?:#([^.#]+))?(?:\.(.+))?" raw)]
    {:tag tag-name
     :id id
     :classes (some-> classes (str/split #"\."))}))

(defn- merge-tag-attrs [tag attrs]
  (let [{:keys [id classes]} (parse-tag tag)
        class-attr (:class attrs)
        classes (cond-> classes
                  class-attr (concat (if (sequential? class-attr)
                                       class-attr
                                       (str/split (str class-attr) #"\s+"))))]
    (cond-> (dissoc attrs :class)
      id (assoc :id (or (:id attrs) id))
      (seq classes) (assoc :class (str/join " " classes)))))

(defn- render-attr [[k v]]
  (cond
    (or (nil? v) (= false v)) nil
    (= true v) (str " " (name k))
    :else (str " " (name k) "=\"" (escape-html v) "\"")))

(declare render-html)
(declare render-canonical-html)

(defn- render-element [[tag & body]]
  (let [{tag-name :tag} (parse-tag tag)
        [attrs children] (if (map? (first body))
                           [(first body) (rest body)]
                           [{} body])
        attrs (merge-tag-attrs tag attrs)
        open (str "<" tag-name (apply str (keep render-attr attrs)) ">")]
    (if (void-tags tag-name)
      open
      (str open (apply str (map render-html children)) "</" tag-name ">"))))

(defn render-html [x]
  (cond
    (nil? x) ""
    (string? x) (escape-html x)
    (keyword? x) (escape-html (name x))
    (number? x) (str x)
    (vector? x) (render-element x)
    (seq? x) (apply str (map render-html x))
    :else (escape-html x)))

(defn- render-canonical-element [[tag & body]]
  (let [{tag-name :tag} (parse-tag tag)
        [attrs children] (if (map? (first body))
                           [(first body) (rest body)]
                           [{} body])
        attrs (merge-tag-attrs tag attrs)
        ordered-attrs (sort-by (comp name key) attrs)
        open (str "<" tag-name (apply str (keep render-attr ordered-attrs)) ">")]
    (if (void-tags tag-name)
      open
      (str open (apply str (map render-canonical-html children)) "</" tag-name ">"))))

(defn render-canonical-html
  "Deterministic Hiccup renderer used for content-addressed compiler fragments.
  Attribute order is lexical; legacy `render-html` keeps insertion behavior."
  [x]
  (cond
    (nil? x) ""
    (string? x) (escape-html x)
    (keyword? x) (escape-html (name x))
    (number? x) (str x)
    (vector? x) (render-canonical-element x)
    (seq? x) (apply str (map render-canonical-html x))
    :else (escape-html x)))

(defn document [hiccup]
  (str "<!doctype html>\n" (render-html hiccup) "\n"))
