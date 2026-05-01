(ns loam.highlight.core
  "Shared helpers for Loam code highlighting providers."
  (:require [clojure.string :as str]
            [loam.ast :as ast]))

(defn normalize-language [lang]
  (let [lang (some-> lang str str/lower-case (str/replace #"^src-" ""))]
    (case lang
      nil "text"
      "" "text"
      "elisp" "emacs-lisp"
      "emacs-lisp" "emacs-lisp"
      "sh" "bash"
      "shell" "bash"
      "zsh" "bash"
      lang)))

(defn- class-parts [value]
  (cond
    (nil? value) []
    (string? value) [value]
    (keyword? value) [(name value)]
    (sequential? value) (mapcat class-parts value)
    :else [(str value)]))

(defn class-list [& classes]
  (->> (mapcat class-parts classes)
       (remove str/blank?)
       (str/join " ")))

(defn render-src-block
  "Render an Org src-block for a client-side highlighting provider."
  [{:keys [pre-class code-attrs pre-attrs]}]
  (fn [_ctx node]
    (let [props (ast/props node)
          lang (normalize-language (:language props))
          code (or (:value props) "")]
      [[:pre (merge {:class (class-list "loam-src" pre-class)
                     :data-language lang}
                    pre-attrs)
        [:code (merge {:class (str "language-" lang)
                       :data-language lang}
                      code-attrs)
         code]]])))
