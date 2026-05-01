(ns loam.graph-test
  (:require [clojure.test :refer [deftest is]]
            [loam.defaults :as defaults]
            [loam.fixtures :as fixtures]
            [loam.graph :as graph]
            [loam.index :as index]))

(deftest builds-page-graph
  (let [idx (index/build-index (defaults/create-system) [fixtures/doc-a fixtures/doc-b])
        data (:graph idx)]
    (is (= #{"page-a" "page-b"} (set (map :id (:nodes data)))))
    (is (= [{:source "page-a"
             :target "page-b"
             :source-url "/notes/a"
             :target-url "/notes/b"
             :text "Page B"
             :type "id"}
            {:source "page-a"
             :target "page-b"
             :source-url "/notes/a"
             :target-url "/notes/b"
             :text "radio"
             :type "fuzzy"}]
           (:edges data)))
    (is (= data (graph/graph-data idx)))))
