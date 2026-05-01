(ns loam.shiki-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [loam.highlight.shiki :as shiki]
            [loam.render :as render]))

(deftest renders-src-block-for-client-side-shiki
  (let [system {:renderers (:renderers (shiki/extension))
                :inline-types #{}}
        node {:type :src-block
              :properties {:language "cljs"
                           :value "(defn foo [] :ok)"}}]
    (is (= [:div.loam-document
            [:pre {:class "loam-src loam-shiki-src"
                   :data-language "cljs"}
             [:code {:class "language-cljs"
                     :data-language "cljs"
                     :data-loam-shiki true}
              "(defn foo [] :ok)"]]]
           (render/render-document system node)))))

(deftest extension-emits-shiki-assets-and-head
  (let [ext (shiki/extension {:theme "github-light"
                              :languages ["clojure"]
                              :import "https://example.test/shiki.mjs"})]
    (is (= :loam.highlight/shiki (:id ext)))
    (is (contains? (:assets ext) "assets/loam-shiki.css"))
    (is (contains? (:assets ext) "assets/loam-shiki.js"))
    (is (str/includes? (get-in ext [:assets "assets/loam-shiki.js"])
                       "import { createHighlighter } from \"https://example.test/shiki.mjs\";"))
    (is (str/includes? (get-in ext [:assets "assets/loam-shiki.js"])
                       "const theme = \"github-light\";"))
    (is (= [[:link {:rel "stylesheet" :href "../../assets/loam-shiki.css"}]
            [:script {:src "../../assets/loam-shiki.js"
                      :type "module"}]]
           ((first (:head ext)) {} "/notes/a")))))
