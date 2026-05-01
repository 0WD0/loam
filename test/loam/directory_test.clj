(ns loam.directory-test
  (:require [clojure.test :refer [deftest is]]
            [loam.directory :as directory]
            [loam.fixtures :as fixtures]
            [loam.index :as index]))

(deftest builds-directory-index-from-page-source-dirs
  (let [documents [(assoc fixtures/doc-a
                          :source "/tmp/edn/a.edn"
                          :source-rel "a.edn"
                          :source-dir "notes")
                   (assoc fixtures/doc-b
                          :source "/tmp/edn/daily/b.edn"
                          :source-rel "daily/b.edn"
                          :source-dir "daily")]
        idx (index/build-index {:indexers [directory/directory-indexer]} documents)
        directories (zipmap (map :path (:directories idx)) (:directories idx))]
    (is (= ["notes" "daily"] (map :path (:directories idx))))
    (is (= ["Page A"] (map :page-title (get-in directories ["notes" :pages]))))
    (is (= ["Page B"] (map :page-title (get-in directories ["daily" :pages]))))
    (is (= {"notes" 1 "daily" 1}
           (update-vals directories :page-count)))))
