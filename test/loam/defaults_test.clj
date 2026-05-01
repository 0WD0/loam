(ns loam.defaults-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [loam.defaults :as defaults]
            [loam.highlight.shiki :as shiki]
            [loam.render :as render]))

(def src-node
  {:type :src-block
   :properties {:language "cljs"
                :value "(defn foo [] :ok)"}})

(defn extension-ids [system]
  (set (map :id (:extensions system))))

(deftest default-system-uses-treesitter-highlighter
  (let [system (defaults/create-system)]
    (is (contains? (extension-ids system) :loam.highlight/treesitter-wasm))
    (is (not (contains? (extension-ids system) :loam.highlight/shiki)))
    (is (contains? (:assets system) "assets/loam-treesitter.css"))
    (is (contains? (:assets system) "assets/loam-treesitter.json"))
    (is (str/includes? (pr-str (render/render-document system src-node))
                       ":data-loam-treesitter true"))))

(deftest shiki-highlight-option-replaces-default-treesitter
  (let [system (defaults/create-system {:highlight {:provider :shiki
                                                    :theme "github-light"
                                                    :languages ["clojure"]}})]
    (is (not (contains? (extension-ids system) :loam.highlight/treesitter-wasm)))
    (is (contains? (extension-ids system) :loam.highlight/shiki))
    (is (not (contains? (:assets system) "assets/loam-treesitter.css")))
    (is (contains? (:assets system) "assets/loam-shiki.css"))
    (is (str/includes? (get-in system [:assets "assets/loam-shiki.js"])
                       "const theme = \"github-light\";"))
    (is (str/includes? (pr-str (render/render-document system src-node))
                       ":data-loam-shiki true"))))

(deftest user-highlight-extension-suppresses-default-treesitter
  (let [system (defaults/create-system {:extensions [(shiki/extension)]})]
    (is (not (contains? (extension-ids system) :loam.highlight/treesitter-wasm)))
    (is (contains? (extension-ids system) :loam.highlight/shiki))
    (is (not (contains? (:assets system) "assets/loam-treesitter.css")))
    (is (contains? (:assets system) "assets/loam-shiki.css"))))

(deftest highlight-none-disables-default-highlighter
  (let [system (defaults/create-system {:highlight :none})]
    (is (not (contains? (extension-ids system) :loam.highlight/treesitter-wasm)))
    (is (not (contains? (extension-ids system) :loam.highlight/shiki)))
    (is (not (contains? (:assets system) "assets/loam-treesitter.css")))
    (is (not (contains? (:assets system) "assets/loam-shiki.css")))))
