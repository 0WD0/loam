(ns loam.ast
  "Utilities for ox-edn style AST maps."
  (:require [clojure.string :as str]))

(defn node? [x]
  (and (map? x) (keyword? (:type x))))

(defn props [node]
  (:properties node))

(defn children [node]
  (:contents node))

(defn walk
  "Depth-first lazy sequence of AST nodes."
  [root]
  (filter node? (tree-seq node? children root)))

(defn value-name [value]
  (cond
    (keyword? value) (name value)
    (symbol? value) (name value)
    (string? value) value
    :else nil))

(defn text
  "Extract display text from strings, AST nodes, or nested sequences."
  [x]
  (cond
    (nil? x) ""
    (string? x) x
    (node? x) (case (:type x)
                (:target :radio-target) (or (-> x props :value) "")
                :line-break "\n"
                (apply str (map text (children x))))
    (sequential? x) (apply str (map text x))
    :else (str x)))

(defn source-stem [source]
  (some-> source
          str
          (str/replace #"\\\\" "/")
          (str/split #"/")
          last
          (str/replace #"\.[^.]*$" "")))

(defn document-title [ast]
  (or (:TITLE (props ast))
      (some (fn [node]
              (when (and (= :keyword (:type node))
                         (= "TITLE" (-> node props :key)))
                (-> node props :value)))
            (walk ast))
      (some-> ast props :path source-stem)))

(defn document-id [ast]
  (:ID (props ast)))

(defn node-title [node]
  (let [p (props node)]
    (or (some-> p :title text not-empty)
        (:raw-value p)
        (:value p)
        (:name p))))

(defn node-anchor [node]
  (let [p (props node)]
    (or (:ID p) (:CUSTOM_ID p) (:value p) (:name p) (:raw-value p))))
