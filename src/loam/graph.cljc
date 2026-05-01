(ns loam.graph
  "Graph data extension for Loam."
  (:require [loam.index :as index]))

(defn page-node-id [entry]
  (or (:page-id entry) (:id entry) (:page-url entry) (:href entry)))

(defn page-node [page]
  {:id (page-node-id page)
   :title (:page-title page)
   :url (:page-url page)
   :source (:source page)
   :source-dir (:source-dir page)})

(defn link-edge [idx link]
  (when-let [target (some->> (:target-key link) (index/lookup-target idx))]
    (let [source-id (or (:source-id link) (:source-url link))
          target-id (page-node-id target)]
      (when (and source-id target-id (not= source-id target-id))
        {:source source-id
         :target target-id
         :source-url (:source-url link)
         :target-url (:page-url target)
         :text (:text link)
         :type (get-in link [:link :type])}))))

(defn graph-data [idx]
  {:nodes (->> (:pages idx)
               vals
               (map page-node)
               (remove #(nil? (:id %)))
               (sort-by :title)
               vec)
   :edges (->> (:links idx)
               (keep #(link-edge idx %))
               distinct
               vec)})

(defn graph-indexer [idx _documents]
  (assoc idx :graph (graph-data idx)))

(def extension
  {:id :loam/graph
   :indexers [graph-indexer]})
