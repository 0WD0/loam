(ns loam.treesitter-wasm-test
  (:require [clojure.test :refer [deftest is]]
            [loam.highlight.treesitter.wasm :as ts-wasm]
            [loam.json :as json]
            [loam.render :as render]))

(deftest renders-src-block-for-client-side-treesitter
  (let [system {:renderers (:renderers (ts-wasm/extension))
                :inline-types #{}}
        node {:type :src-block
              :properties {:language "cljs"
                           :value "(defn foo [] :ok)"}}]
    (is (= [:div.loam-document
            [:pre.loam-src.loam-ts-src {:data-language "cljs"}
             [:code {:class "language-cljs"
                     :data-loam-treesitter true
                     :data-language "cljs"}
              "(defn foo [] :ok)"]]]
           (render/render-document system node)))))

(deftest extension-emits-assets-head-and-manifest
  (let [ext (ts-wasm/extension {:languages {:clojure {:aliases ["clj"]
                                                      :wasm "/x/clj.wasm"
                                                      :query "/x/highlights.scm"}}})
        manifest (json/render-json {:runtime "/assets/tree-sitter/tree-sitter.js"
                                    :runtime-wasm "/assets/tree-sitter/tree-sitter.wasm"
                                    :languages {:clojure {:aliases ["clj"]
                                                          :wasm "/x/clj.wasm"
                                                          :query "/x/highlights.scm"}}})]
    (is (= :loam.highlight/treesitter-wasm (:id ext)))
    (is (contains? (:assets ext) "assets/loam-treesitter.css"))
    (is (not (contains? (:assets ext) "assets/loam-treesitter.js")))
    (is (= manifest (get-in ext [:assets "assets/loam-treesitter.json"])))
    (is (nil? (:asset-files ext)))
    (is (= [[:link {:rel "stylesheet" :href "../../assets/loam-treesitter.css"}]
            [:script {:src "../../assets/treesitter.js"
                      :defer true
                      :data-config "../../assets/loam-treesitter.json"
                      :data-runtime "../../assets/tree-sitter/tree-sitter.js"
                      :data-runtime-wasm "../../assets/tree-sitter/tree-sitter.wasm"}]]
           ((first (:head ext)) {} "/notes/a")))))
