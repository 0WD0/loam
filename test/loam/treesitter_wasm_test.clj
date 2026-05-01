(ns loam.treesitter-wasm-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
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
            [:pre {:class "loam-src loam-ts-src"
                   :data-language "cljs"}
             [:code {:class "language-cljs"
                     :data-language "cljs"
                     :data-loam-treesitter true}
              "(defn foo [] :ok)"]]]
           (render/render-document system node)))))

(deftest extension-emits-assets-head-and-manifest
  (let [ext (ts-wasm/extension {:languages {:clojure {:aliases ["clj"]
                                                      :wasm "/x/clj.wasm"
                                                      :query "/x/highlights.scm"}}})
        manifest (json/render-json {:runtime-wasm "/assets/tree-sitter/tree-sitter.wasm"
                                    :languages {:clojure {:aliases ["clj"]
                                                          :wasm "/x/clj.wasm"
                                                          :query "/x/highlights.scm"}}})]
    (is (= :loam.highlight/treesitter-wasm (:id ext)))
    (is (contains? (:assets ext) "assets/loam-treesitter.css"))
    (is (not (contains? (:assets ext) "assets/loam-treesitter.js")))
    (is (= manifest (get-in ext [:assets "assets/loam-treesitter.json"])))
    (is (contains? (:assets ext) "assets/tree-sitter/loam-runtime.js"))
    (is (str/includes? (get-in ext [:assets "assets/tree-sitter/loam-runtime.js"])
                       "import * as runtime from \"./tree-sitter.js\";"))
    (is (nil? (:asset-files ext)))
    (is (= [[:link {:rel "stylesheet" :href "../../assets/loam-treesitter.css"}]
            [:script {:src "../../assets/tree-sitter/loam-runtime.js"
                      :type "module"}]
            [:script {:src "../../assets/loam-client.js"
                      :defer true
                      :data-config "../../assets/loam-treesitter.json"
                      :data-runtime-global "LoamTreeSitter"
                      :data-runtime-wasm "../../assets/tree-sitter/tree-sitter.wasm"}]]
           ((first (:head ext)) {} "/notes/a")))))
