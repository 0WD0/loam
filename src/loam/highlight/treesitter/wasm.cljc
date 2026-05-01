(ns loam.highlight.treesitter.wasm
  "Client-side Tree-sitter/WASM syntax highlighting extension.

  This extension keeps Org src-block semantics in the EDN AST and only
  changes presentation: src blocks are rendered with data attributes, and a
  shadow-cljs-built browser asset loads web-tree-sitter plus language WASM
  and query files to turn code text into semantic spans."
  (:require [loam.head :as head]
            [loam.highlight.core :as highlight]
            [loam.json :as json]))

(def default-languages
  "Default language manifest.

  Matching language WASM/query files expected under
  `public/assets/tree-sitter`. The browser client is part of the generic
  `public/assets/loam-client.js` bundle built by `npm run client:release`.
  Use `npm run prepare-assets` to sync and compile defaults from node_modules.
  Pass custom :languages map to `extension` to add or replace languages."
  {:clojure {:aliases ["clj" "cljs" "cljc" "edn"]
             :wasm "/assets/tree-sitter/languages/tree-sitter-clojure.wasm"
             :query "/assets/tree-sitter/queries/clojure/highlights.scm"}
   :javascript {:aliases ["js" "jsx" "node"]
                :wasm "/assets/tree-sitter/languages/tree-sitter-javascript.wasm"
                :query "/assets/tree-sitter/queries/javascript/highlights.scm"}
   :typescript {:aliases ["ts"]
                :wasm "/assets/tree-sitter/languages/tree-sitter-typescript.wasm"
                :query "/assets/tree-sitter/queries/typescript/highlights.scm"}
   :python {:aliases ["py"]
            :wasm "/assets/tree-sitter/languages/tree-sitter-python.wasm"
            :query "/assets/tree-sitter/queries/python/highlights.scm"}})

(def render-src-block
  (highlight/render-src-block
   {:pre-class "loam-ts-src"
    :code-attrs {:data-loam-treesitter true}}))

(def css
  (str
   ".loam-ts-src{position:relative}.loam-ts-src code{white-space:pre}"
   ".loam-ts-src[data-loam-treesitter-status=loading]::after{content:'highlighting';position:absolute;top:.45rem;right:.6rem;color:#94a3b8;font-size:.75rem}"
   ".loam-ts-src[data-loam-treesitter-status=missing]::after,.loam-ts-src[data-loam-treesitter-status=error]::after{content:attr(data-loam-treesitter-message);position:absolute;top:.45rem;right:.6rem;color:#fca5a5;font-size:.75rem}"
   ".ts-comment{color:#7f848e;font-style:italic}.ts-keyword,.ts-keyword-return,.ts-keyword-function,.ts-keyword-conditional,.ts-keyword-repeat,.ts-keyword-operator{color:#c678dd}.ts-function,.ts-function-call,.ts-method,.ts-method-call{color:#61afef}.ts-variable{color:#e5c07b}.ts-variable-parameter,.ts-parameter{color:#d19a66}.ts-type,.ts-type-builtin{color:#56b6c2}.ts-string,.ts-string-special{color:#98c379}.ts-string-escape{color:#56b6c2}.ts-number,.ts-boolean,.ts-constant,.ts-constant-builtin{color:#d19a66}.ts-operator{color:#56b6c2}.ts-property,.ts-attribute{color:#e06c75}.ts-punctuation,.ts-punctuation-bracket,.ts-punctuation-delimiter,.ts-punctuation-special{color:#abb2bf}.ts-tag{color:#e06c75}"))

(defn manifest [opts]
  {:runtime-wasm (or (:runtime-wasm opts) "/assets/tree-sitter/tree-sitter.wasm")
   :languages (or (:languages opts) default-languages)})

(defn runtime-loader-js [opts]
  (let [runtime (or (:runtime opts) "./tree-sitter.js")
        runtime-global (or (:runtime-global opts) "LoamTreeSitter")]
    (str "import * as runtime from " (json/render-json runtime) ";\n"
         "globalThis[" (json/render-json runtime-global) "] = runtime;\n"
         "globalThis.dispatchEvent(new CustomEvent('loam:tree-sitter-runtime', {detail: runtime}));\n")))

(defn head-tags [opts]
  (fn [_ctx current-url]
    (let [runtime-global (or (:runtime-global opts) "LoamTreeSitter")]
      [(head/stylesheet current-url "/assets/loam-treesitter.css")
       (head/script current-url "/assets/tree-sitter/loam-runtime.js"
                    {:type "module"})
       (head/script current-url "/assets/loam-client.js"
                    {:defer true
                     :data-config (head/asset-url current-url "/assets/loam-treesitter.json")
                     :data-runtime-global runtime-global
                     :data-runtime-wasm (head/asset-url current-url (or (:runtime-wasm opts)
                                                                        "/assets/tree-sitter/tree-sitter.wasm"))})])))

(defn extension
  "Create the Tree-sitter/WASM highlighter extension.

  Options:
  - :languages map of language name -> {:aliases [...] :wasm URL :query URL}
  - :runtime ESM import specifier for web-tree-sitter JS module
  - :runtime-global global name published by runtime loader
  - :runtime-wasm URL to web-tree-sitter runtime WASM asset"
  ([] (extension {}))
  ([opts]
   {:id :loam.highlight/treesitter-wasm
    :loam.highlight/provider :treesitter
    :renderers {:src-block render-src-block}
    :head [(head-tags opts)]
    :assets {"assets/loam-treesitter.css" css
             "assets/loam-treesitter.json" (json/render-json (manifest opts))
             "assets/tree-sitter/loam-runtime.js" (runtime-loader-js opts)}}))
