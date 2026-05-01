(ns loam.search
  "Search index extension."
  (:require [loam.ast :as ast]))

(defn document-search-entry [document]
  {:source (:source document)
   :id (:id document)
   :title (:title document)
   :url (:url document)
   :text (ast/text (:ast document))})

(defn search-indexer [idx documents]
  (assoc idx :search/documents (mapv document-search-entry documents)))

(def extension
  {:id :loam/search
   :extends {:indexers [search-indexer]}})
