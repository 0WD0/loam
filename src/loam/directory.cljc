(ns loam.directory
  "Directory index extension for Loam pages.")

(def default-root "notes")

(defn page-directory [page]
  (or (:source-dir page) default-root))

(defn directory-entry [[path pages]]
  (let [pages (vec (sort-by :page-title pages))]
    {:path path
     :title path
     :page-count (count pages)
     :pages pages}))

(defn directory-index [pages]
  (->> pages
       vals
       (group-by page-directory)
       (map directory-entry)
       (sort-by (fn [{:keys [path]}] [(if (= path default-root) 0 1) path]))
       vec))

(defn directory-indexer [idx _documents]
  (assoc idx :directories (directory-index (:pages idx))))

(def extension
  {:id :loam/directory
   :extends {:indexers [directory-indexer]}})
