(ns loam.toc-test
  (:require [clojure.test :refer [deftest is]]
            [loam.index :as index]
            [loam.toc :as toc]))

(def toc-doc
  {:source "/tmp/edn/toc.edn"
   :source-rel "toc.edn"
   :source-dir "notes"
   :ast {:type :org-data
         :properties {:ID "page-toc"
                      :EXPORT_FILE_NAME "toc"}
         :contents [{:type :headline
                     :properties {:level 1
                                  :raw-value "Intro"
                                  :ID "intro"}
                     :contents [{:type :headline
                                 :properties {:level 2
                                              :raw-value "Install"
                                              :CUSTOM_ID "install"}
                                 :contents [{:type :headline
                                             :properties {:level 3
                                                          :raw-value "NPM"
                                                          :ID "npm"}}]}]}
                    {:type :headline
                     :properties {:level 1
                                  :raw-value "Usage"
                                  :ID "usage"}}]}})

(deftest builds-nested-page-table-of-contents
  (let [idx (index/build-index {:indexers [toc/toc-indexer]} [toc-doc])]
    (is (= [{:level 1
             :title "Intro"
             :href "/notes/toc#intro"
             :anchor-id "intro"
             :id "intro"
             :children [{:level 2
                         :title "Install"
                         :href "/notes/toc#install"
                         :anchor-id "install"
                         :custom-id "install"
                         :children [{:level 3
                                     :title "NPM"
                                     :href "/notes/toc#npm"
                                     :anchor-id "npm"
                                     :id "npm"
                                     :children []}]}]}
            {:level 1
             :title "Usage"
             :href "/notes/toc#usage"
             :anchor-id "usage"
             :id "usage"
             :children []}]
           (get-in idx [:toc "/tmp/edn/toc.edn"])))))
