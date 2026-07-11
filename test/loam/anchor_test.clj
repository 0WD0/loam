(ns loam.anchor-test
  (:require [clojure.test :refer [deftest is]]
            [loam.anchor :as anchor]))

(deftest canonicalizes-fragments
  (is (= "Radio-B" (anchor/fragment-id "Radio B")))
  (is (= "中文-标题" (anchor/fragment-id "中文 标题")))
  (is (= "a-b-c" (anchor/fragment-id " a / b ? c ")))
  (is (nil? (anchor/fragment-id "   "))))

(deftest emits-ascii-slugs-and-percent-encoded-hrefs
  (is (= "key-x3a-x25-x5b-x5b-x60-x2a-u4e2d-u6587"
         (anchor/ascii-slug "Key: % [[ ` * 中文")))
  (is (re-matches #"[a-z0-9]+(?:-[a-z0-9]+)*"
                  (anchor/ascii-slug "Key: % [[ ` * 中文")))
  (is (= "%25%20%5B%5B%60%2A%3A%28%E4%B8%AD%E6%96%87%29"
         (anchor/percent-encode-fragment "% [[`*:(中文)")))
  (is (= "/docs/#%E6%98%BE%E5%BC%8F%25anchor"
         (anchor/with-fragment "/docs/" "显式%anchor")))
  (is (= "显式%anchor"
         (anchor/docs-anchor-id
          {:type :headline :properties {:CUSTOM_ID "显式%anchor"}}))))

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
  (is (= "#%E6%98%BE%E5%BC%8F%25anchor"
         (anchor/local-href {:type "id" :path "显式%anchor"})))
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
  (is (= "face-policy-face"
         (anchor/canonical-title-id "Face Policy (:face)")))
  (is (= "understanding-revisions"
         (anchor/canonical-title-id "Understanding --revisions")))
  (is (= "u4e2d-u6587-u6807-u9898"
         (anchor/canonical-title-id "中文 标题")))
  (is (anchor/explicit-docs-anchor?
       {:type :target :properties {:value "Explicit target"}}))
  (is (not (anchor/explicit-docs-anchor?
            {:type :headline :properties {:raw-value "Generated"}}))))
