(ns loam.toc
  "Per-page table of contents extension."
  (:require [loam.ast :as ast]
            [loam.index :as index]))

(defn toc-item [document node]
  (let [entry (index/node-entry document node)
        props (ast/props node)]
    (cond-> {:level (or (:level props) 1)
             :title (:title entry)
             :href (:href entry)
             :anchor-id (:anchor-id entry)}
      (:id entry) (assoc :id (:id entry))
      (:custom-id entry) (assoc :custom-id (:custom-id entry)))))

(defn nest-items [items]
  (letfn [(step [items min-level]
            (loop [items items
                   result []]
              (if-let [item (first items)]
                (if (< (:level item) min-level)
                  [result items]
                  (let [[children rest-items] (step (rest items) (inc (:level item)))]
                    (recur rest-items (conj result (assoc item :children children)))))
                [result nil])))]
    (first (step items 1))))

(defn document-toc [document]
  (->> (:ast document)
       ast/walk
       (filter #(= :headline (:type %)))
       (map #(toc-item document %))
       nest-items))

(defn toc-indexer [idx documents]
  (assoc idx :toc
         (into {}
               (map (fn [document]
                      [(:source document) (document-toc document)]))
               documents)))

(def extension
  {:id :loam/toc
   :extends {:indexers [toc-indexer]}})
