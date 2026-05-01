(ns loam.defaults
  "Default Loam system composition."
  (:require [loam.core :as core]
            [loam.graph :as graph]
            [loam.highlight.treesitter.wasm :as treesitter.wasm]
            [loam.render :as render]
            [loam.search :as search]
            [loam.theme.default :as theme.default]))

(def default-extensions
  [render/extension
   (treesitter.wasm/extension)
   search/extension
   graph/extension
   theme.default/extension])

(defn create-system
  ([] (create-system {}))
  ([opts]
   (core/create-system
    (update opts :extensions #(vec (concat default-extensions %))))))
