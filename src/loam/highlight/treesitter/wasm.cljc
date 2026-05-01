(ns loam.highlight.treesitter.wasm
  "Client-side Tree-sitter/WASM syntax highlighting extension.

  This extension keeps Org src-block semantics in the EDN AST and only
  changes presentation: src blocks are rendered with data attributes, and a
  shadow-cljs-built browser asset loads web-tree-sitter plus language WASM
  and query files to turn code text into semantic spans."
  (:require [clojure.string :as str]
            [loam.ast :as ast]
            [loam.head :as head]
            [loam.json :as json]))

(def default-languages
  "Default language manifest.

  Matching language WASM/query files expected under
  `public/assets/tree-sitter`. The browser client is compiled to
  `public/assets/treesitter.js` with `npm run cljs:release`. Use
  `npm run prepare-assets` to sync and compile defaults from node_modules.
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

(defn normalize-language [lang]
  (let [lang (some-> lang str str/lower-case (str/replace #"^src-" ""))]
    (case lang
      nil "text"
      "" "text"
      "elisp" "emacs-lisp"
      "emacs-lisp" "emacs-lisp"
      "sh" "bash"
      "shell" "bash"
      "zsh" "bash"
      lang)))

(defn render-src-block [_ctx node]
  (let [p (ast/props node)
        lang (normalize-language (:language p))
        code (or (:value p) "")]
    [[:pre.loam-src.loam-ts-src {:data-language lang}
      [:code {:class (str "language-" lang)
              :data-loam-treesitter true
              :data-language lang}
       code]]]))

(def css
  (str
   ".loam-ts-src{position:relative}.loam-ts-src code{white-space:pre}"
   ".loam-ts-src[data-loam-treesitter-status=loading]::after{content:'highlighting';position:absolute;top:.45rem;right:.6rem;color:#94a3b8;font-size:.75rem}"
   ".loam-ts-src[data-loam-treesitter-status=missing]::after,.loam-ts-src[data-loam-treesitter-status=error]::after{content:attr(data-loam-treesitter-message);position:absolute;top:.45rem;right:.6rem;color:#fca5a5;font-size:.75rem}"
   ".ts-comment{color:#7f848e;font-style:italic}.ts-keyword,.ts-keyword-return,.ts-keyword-function,.ts-keyword-conditional,.ts-keyword-repeat,.ts-keyword-operator{color:#c678dd}.ts-function,.ts-function-call,.ts-method,.ts-method-call{color:#61afef}.ts-variable{color:#e5c07b}.ts-variable-parameter,.ts-parameter{color:#d19a66}.ts-type,.ts-type-builtin{color:#56b6c2}.ts-string,.ts-string-special{color:#98c379}.ts-string-escape{color:#56b6c2}.ts-number,.ts-boolean,.ts-constant,.ts-constant-builtin{color:#d19a66}.ts-operator{color:#56b6c2}.ts-property,.ts-attribute{color:#e06c75}.ts-punctuation,.ts-punctuation-bracket,.ts-punctuation-delimiter,.ts-punctuation-special{color:#abb2bf}.ts-tag{color:#e06c75}"))

(defn manifest [opts]
  {:runtime (or (:runtime opts) "/assets/tree-sitter/tree-sitter.js")
   :runtime-wasm (or (:runtime-wasm opts) "/assets/tree-sitter/tree-sitter.wasm")
   :languages (or (:languages opts) default-languages)})

(defn head-tags [opts]
  (fn [_ctx current-url]
    [(head/stylesheet current-url "/assets/loam-treesitter.css")
     (head/script current-url "/assets/treesitter.js"
                  {:defer true
                   :data-config (head/asset-url current-url "/assets/loam-treesitter.json")
                   :data-runtime (head/asset-url current-url (or (:runtime opts)
                                                                 "/assets/tree-sitter/tree-sitter.js"))
                   :data-runtime-wasm (head/asset-url current-url (or (:runtime-wasm opts)
                                                                      "/assets/tree-sitter/tree-sitter.wasm"))})]))

(defn extension
  "Create the Tree-sitter/WASM highlighter extension.

  Options:
  - :languages map of language name -> {:aliases [...] :wasm URL :query URL}
  - :runtime URL to web-tree-sitter JS module asset
  - :runtime-wasm URL to web-tree-sitter runtime WASM asset"
  ([] (extension {}))
  ([opts]
   {:id :loam.highlight/treesitter-wasm
    :renderers {:src-block render-src-block}
    :head [(head-tags opts)]
    :assets {"assets/loam-treesitter.css" css
             "assets/loam-treesitter.json" (json/render-json (manifest opts))}}))
