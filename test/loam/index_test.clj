(ns loam.index-test
  (:require [clojure.test :refer [deftest is]]
            [loam.defaults :as defaults]
            [loam.fixtures :as fixtures]
            [loam.index :as index]))

(deftest builds-navigation-index
  (let [idx (index/build-index (defaults/create-system) [fixtures/doc-a fixtures/doc-b])]
    (is (= "/notes/a" (get-in idx [:ids "page-a" :href])))
    (is (= "/notes/b" (get-in idx [:ids "page-b" :href])))
    (is (= "/notes/b#custom-b" (get-in idx [:custom-ids "custom-b" :href])))
    (is (= "/notes/b#Radio-B" (-> idx :targets (get "Radio B") first :href)))
    (is (= 2 (count (:links idx))))
    (is (= 2 (count (:search/documents idx))))))

(deftest resolves-links-and-backlinks
  (let [idx (index/build-index (defaults/create-system) [fixtures/doc-a fixtures/doc-b])]
    (is (= "/notes/b" (index/resolve-link idx {:type "id" :path "page-b"})))
    (is (= "/notes/b#Radio-B"
           (index/resolve-link idx {:type "fuzzy"
                                    :path "Radio B"
                                    :resolved {:resolved-type :radio-target
                                               :resolved-value "Radio B"}})))
    (is (= ["Page B"] (map :text (index/backlinks-for idx "page-b"))))
    (is (= ["radio"] (map :text (index/backlinks-for idx [:target "Radio B"]))))))

(deftest allows-custom-indexer-extension
  (let [ext {:id :test/indexer
             :extends {:indexers [(fn [idx documents]
                                    (assoc idx :document-count (count documents)))]}}
        idx (index/build-index (defaults/create-system {:extensions [ext]})
                               [fixtures/doc-a fixtures/doc-b])]
    (is (= 2 (:document-count idx)))))
