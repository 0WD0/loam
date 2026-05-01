(ns loam.defaults
  "Default Loam system composition."
  (:require [loam.core :as core]
            [loam.render :as render]
            [loam.search :as search]
            [loam.theme.default :as theme.default]))

(def default-extensions
  [render/extension
   search/extension
   theme.default/extension])

(defn create-system
  ([] (create-system {}))
  ([opts]
   (core/create-system
    (update opts :extensions #(vec (concat default-extensions %))))))
