(ns loam.anchor-test
  (:require [clojure.test :refer [deftest is]]
            [loam.anchor :as anchor]))

(deftest canonicalizes-fragments
  (is (= "Radio-B" (anchor/fragment-id "Radio B")))
  (is (= "中文-标题" (anchor/fragment-id "中文 标题")))
  (is (= "a-b-c" (anchor/fragment-id " a / b ? c ")))
  (is (nil? (anchor/fragment-id "   "))))

(deftest derives-node-anchor-ids
  (is (= "uuid-1"
         (anchor/node-anchor-id {:type :headline
                                 :properties {:ID "uuid-1" :raw-value "Title"}})))
  (is (= "custom-id"
         (anchor/node-anchor-id {:type :headline
                                 :properties {:CUSTOM_ID "custom id" :raw-value "Title"}})))
  (is (= "Radio-B"
         (anchor/node-anchor-id {:type :radio-target
                                 :properties {:value "Radio B"}}))))

(deftest derives-link-local-hrefs
  (is (= "#uuid-1" (anchor/local-href {:type "id" :path "uuid-1"})))
  (is (= "#Radio-B"
         (anchor/local-href {:type "fuzzy"
                             :path "Radio B"
                             :resolved {:resolved-type :radio-target
                                        :resolved-value "Radio B"}})))
  (is (nil? (anchor/local-href {:type "https" :path "example.com"}))))

(deftest docs-anchors-use-stability-precedence
  (is (= "custom"
         (anchor/docs-anchor-id {:type :headline
                                 :properties {:CUSTOM_ID "custom"
                                              :ID "id"
                                              :raw-value "Title"}})))
  (is (= "generated-title"
         (anchor/docs-anchor-id {:type :headline
                                 :properties {:raw-value "Generated Title"}})))
  (is (anchor/explicit-docs-anchor?
       {:type :target :properties {:value "Explicit target"}}))
  (is (not (anchor/explicit-docs-anchor?
            {:type :headline :properties {:raw-value "Generated"}}))))
