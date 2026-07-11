(ns loam.json
  "Tiny JSON encoder for Loam static artifacts."
  (:require [clojure.string :as str]))

(defn escape-json [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\b" "\\b")
      (str/replace "\f" "\\f")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")))

(declare render-json)
(declare render-canonical-json)

(defn- render-map-entry [[k v]]
  (str (render-json (name k)) ":" (render-json v)))

(defn render-json [x]
  (cond
    (nil? x) "null"
    (true? x) "true"
    (false? x) "false"
    (number? x) (str x)
    (string? x) (str "\"" (escape-json x) "\"")
    (keyword? x) (render-json (name x))
    (symbol? x) (render-json (name x))
    (map? x) (str "{" (str/join "," (map render-map-entry x)) "}")
    (sequential? x) (str "[" (str/join "," (map render-json x)) "]")
    :else (render-json (str x))))

(defn- canonical-map-entry [[k v]]
  (str (render-canonical-json (name k)) ":" (render-canonical-json v)))

(defn render-canonical-json
  "Render deterministic JSON by ordering every map lexicographically by its
  serialized key. This is used for public compiler artifacts; the legacy
  `render-json` function retains its existing insertion-order behavior."
  [x]
  (cond
    (nil? x) "null"
    (true? x) "true"
    (false? x) "false"
    (number? x) (str x)
    (string? x) (str "\"" (escape-json x) "\"")
    (keyword? x) (render-canonical-json (name x))
    (symbol? x) (render-canonical-json (name x))
    (map? x) (str "{"
                  (str/join "," (map canonical-map-entry
                                     (sort-by (comp name key) x)))
                  "}")
    (sequential? x) (str "[" (str/join "," (map render-canonical-json x)) "]")
    :else (render-canonical-json (str x))))
