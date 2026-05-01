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
