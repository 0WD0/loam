(ns loam.highlight.treesitter.wasm
  "Client-side Tree-sitter/WASM syntax highlighting extension.

  This extension keeps Org src-block semantics in the EDN AST and only
  changes presentation: src blocks are rendered with data attributes, and a
  small browser module loads web-tree-sitter plus language WASM files to
  turn code text into semantic spans."
  (:require [clojure.string :as str]
            [loam.ast :as ast]
            [loam.head :as head]
            [loam.json :as json]))

(def default-languages
  "Default language manifest.

  The referenced files are copied from npm packages listed in package.json.
  Pass a custom :languages map to `extension` to add or replace languages."
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

(def js
"(() => {
  const currentScript = document.currentScript;
  const configUrl = currentScript?.dataset?.config || '/assets/loam-treesitter.json';
  const runtimeUrl = currentScript?.dataset?.runtime || '/assets/tree-sitter/tree-sitter.js';
  const runtimeWasmUrl = currentScript?.dataset?.runtimeWasm || '/assets/tree-sitter/tree-sitter.wasm';

  const absoluteUrl = (url) => new URL(url, document.baseURI).toString();
  const assetUrl = (url) => url.startsWith('/') ? new URL(url, window.location.origin).toString() : absoluteUrl(url);

  const setStatus = (codeEl, status, message) => {
    const pre = codeEl.closest('pre');
    if (!pre) return;
    if (status) pre.dataset.loamTreesitterStatus = status;
    if (message) pre.dataset.loamTreesitterMessage = message;
  };

  const clearStatus = (codeEl) => {
    const pre = codeEl.closest('pre');
    if (!pre) return;
    delete pre.dataset.loamTreesitterStatus;
    delete pre.dataset.loamTreesitterMessage;
  };

  const escapeCapture = (capture) =>
    'ts-' + String(capture || '')
      .replace(/^@/, '')
      .toLowerCase()
      .replace(/[^a-z0-9_-]+/g, '-')
      .replace(/^-+|-+$/g, '');

  const languageName = (codeEl) =>
    codeEl.dataset.language || codeEl.closest('pre')?.dataset.language || 'text';

  const languageConfig = (manifest, lang) => {
    if (manifest.languages?.[lang]) return {...manifest.languages[lang], canonical: lang};
    for (const [name, conf] of Object.entries(manifest.languages || {})) {
      if ((conf.aliases || []).includes(lang)) return {...conf, canonical: name};
    }
    return null;
  };

  const importRuntime = async (url) => {
    const mod = await import(assetUrl(url));
    const Parser = mod.Parser || mod.default || mod;
    const Language = mod.Language || Parser.Language;
    const Query = mod.Query || Parser.Query;
    return {Parser, Language, Query};
  };

  const initRuntime = async (Parser) => {
    if (Parser.__loamInitialized) return;
    await Parser.init({
      locateFile(scriptName) {
        return assetUrl(scriptName.endsWith('.wasm') ? runtimeWasmUrl : scriptName);
      }
    });
    Parser.__loamInitialized = true;
  };

  const queryCaptures = (query, rootNode) => {
    if (typeof query.captures === 'function') return query.captures(rootNode);
    if (typeof query.matches === 'function') {
      const out = [];
      for (const match of query.matches(rootNode)) {
        for (const capture of match.captures || []) out.push(capture);
      }
      return out;
    }
    return [];
  };

  const nodeStart = (node) => node.startIndex ?? node.startPosition?.index ?? 0;
  const nodeEnd = (node) => node.endIndex ?? node.endPosition?.index ?? nodeStart(node);

  const captureName = (capture, query) => {
    if (capture.name) return capture.name;
    if (typeof query.captureNameForId === 'function' && capture.index != null) return query.captureNameForId(capture.index);
    if (typeof query.getCaptureNameForId === 'function' && capture.index != null) return query.getCaptureNameForId(capture.index);
    return String(capture.index ?? 'unknown');
  };

  const byteToJsIndexMap = (str) => {
    const encoder = new TextEncoder();
    const map = [];
    let byte = 0;
    for (let i = 0; i < str.length;) {
      map[byte] = i;
      const cp = str.codePointAt(i);
      const ch = String.fromCodePoint(cp);
      const len = encoder.encode(ch).length;
      for (let j = 1; j < len; j++) map[byte + j] = i;
      byte += len;
      i += ch.length;
    }
    map[byte] = str.length;
    return map;
  };

  const normalizeRanges = (captures, query, source) => {
    const ranges = captures.map((capture) => {
      const node = capture.node;
      return {start: nodeStart(node), end: nodeEnd(node), capture: captureName(capture, query)};
    }).filter((r) => r.end > r.start);

    ranges.sort((a, b) =>
      a.start - b.start || b.end - a.end || String(a.capture).localeCompare(String(b.capture)));

    const accepted = [];
    let cursor = 0;
    for (const range of ranges) {
      if (range.start < cursor) continue;
      accepted.push(range);
      cursor = range.end;
    }
    return accepted;
  };

  const renderRanges = (codeEl, source, ranges) => {
    const byteMap = byteToJsIndexMap(source);
    const frag = document.createDocumentFragment();
    let cursor = 0;

    const appendText = (startByte, endByte) => {
      if (endByte <= startByte) return;
      frag.appendChild(document.createTextNode(source.slice(byteMap[startByte], byteMap[endByte])));
    };

    for (const range of ranges) {
      appendText(cursor, range.start);
      const span = document.createElement('span');
      span.className = escapeCapture(range.capture);
      span.textContent = source.slice(byteMap[range.start], byteMap[range.end]);
      frag.appendChild(span);
      cursor = range.end;
    }
    appendText(cursor, byteMap.length - 1);

    codeEl.replaceChildren(frag);
    codeEl.dataset.loamTreesitterDone = 'true';
  };

  const loadManifest = async () => {
    const res = await fetch(assetUrl(configUrl));
    if (!res.ok) throw new Error(`failed to load ${configUrl}: ${res.status}`);
    return await res.json();
  };

  const main = async () => {
    const codeBlocks = [...document.querySelectorAll('code[data-loam-treesitter]')]
      .filter((el) => el.dataset.loamTreesitterDone !== 'true');
    if (!codeBlocks.length) return;

    const manifest = await loadManifest();
    const {Parser, Language, Query} = await importRuntime(manifest.runtime || runtimeUrl);
    await initRuntime(Parser);
    const highlighters = new Map();

    const getHighlighter = async (lang) => {
      const conf = languageConfig(manifest, lang);
      if (!conf) return null;
      const key = conf.canonical || lang;
      if (highlighters.has(key)) return await highlighters.get(key);
      const promise = (async () => {
        const language = await Language.load(assetUrl(conf.wasm));
        const queryText = await fetch(assetUrl(conf.query)).then((r) => {
          if (!r.ok) throw new Error(`failed to load ${conf.query}: ${r.status}`);
          return r.text();
        });
        const parser = new Parser();
        parser.setLanguage(language);
        const query = typeof language.query === 'function'
          ? language.query(queryText)
          : new Query(language, queryText);
        return {parser, query};
      })();
      highlighters.set(key, promise);
      return await promise;
    };

    for (const codeEl of codeBlocks) {
      const lang = languageName(codeEl);
      try {
        setStatus(codeEl, 'loading');
        const highlighter = await getHighlighter(lang);
        if (!highlighter) {
          setStatus(codeEl, 'missing', `no tree-sitter parser for ${lang}`);
          continue;
        }
        const source = codeEl.textContent || '';
        const tree = highlighter.parser.parse(source);
        const captures = queryCaptures(highlighter.query, tree.rootNode);
        const ranges = normalizeRanges(captures, highlighter.query, source);
        renderRanges(codeEl, source, ranges);
        clearStatus(codeEl);
      } catch (error) {
        console.error('[loam-treesitter]', error);
        setStatus(codeEl, 'error', 'tree-sitter error');
      }
    }
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', main, {once: true});
  } else {
    main();
  }
})();
")

(def npm-asset-files
  {"assets/tree-sitter/tree-sitter.js"
   "node_modules/web-tree-sitter/web-tree-sitter.js"
   "assets/tree-sitter/tree-sitter.wasm"
   "node_modules/web-tree-sitter/web-tree-sitter.wasm"

   "assets/tree-sitter/languages/tree-sitter-clojure.wasm"
   "node_modules/@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"
   "assets/tree-sitter/languages/tree-sitter-javascript.wasm"
   "node_modules/@vscode/tree-sitter-wasm/wasm/tree-sitter-javascript.wasm"
   "assets/tree-sitter/languages/tree-sitter-typescript.wasm"
   "node_modules/@vscode/tree-sitter-wasm/wasm/tree-sitter-typescript.wasm"
   "assets/tree-sitter/languages/tree-sitter-python.wasm"
   "node_modules/@vscode/tree-sitter-wasm/wasm/tree-sitter-python.wasm"

   "assets/tree-sitter/queries/clojure/highlights.scm"
   "node_modules/@yogthos/tree-sitter-clojure/queries/highlights.scm"
   "assets/tree-sitter/queries/javascript/highlights.scm"
   "node_modules/tree-sitter-javascript/queries/highlights.scm"
   "assets/tree-sitter/queries/typescript/highlights.scm"
   "node_modules/tree-sitter-typescript/queries/highlights.scm"
   "assets/tree-sitter/queries/python/highlights.scm"
   "node_modules/tree-sitter-python/queries/highlights.scm"

   "assets/tree-sitter/licenses/web-tree-sitter-MIT.txt"
   "node_modules/web-tree-sitter/LICENSE"
   "assets/tree-sitter/licenses/vscode-tree-sitter-wasm-MIT.txt"
   "node_modules/@vscode/tree-sitter-wasm/LICENSE"
   "assets/tree-sitter/licenses/tree-sitter-clojure.txt"
   "node_modules/@yogthos/tree-sitter-clojure/COPYING.txt"
   "assets/tree-sitter/licenses/tree-sitter-javascript-MIT.txt"
   "node_modules/tree-sitter-javascript/LICENSE"
   "assets/tree-sitter/licenses/tree-sitter-typescript-MIT.txt"
   "node_modules/tree-sitter-typescript/LICENSE"
   "assets/tree-sitter/licenses/tree-sitter-python-MIT.txt"
   "node_modules/tree-sitter-python/LICENSE"})

(defn manifest [opts]
  {:runtime (or (:runtime opts) "/assets/tree-sitter/tree-sitter.js")
   :runtime-wasm (or (:runtime-wasm opts) "/assets/tree-sitter/tree-sitter.wasm")
   :languages (or (:languages opts) default-languages)})

(defn head-tags [opts]
  (fn [_ctx current-url]
    [(head/stylesheet current-url "/assets/loam-treesitter.css")
     (head/script current-url "/assets/loam-treesitter.js"
                  {:type "module"
                   :defer true
                   :data-config (head/asset-url current-url "/assets/loam-treesitter.json")
                   :data-runtime (head/asset-url current-url (or (:runtime opts) "/assets/tree-sitter/tree-sitter.js"))
                   :data-runtime-wasm (head/asset-url current-url (or (:runtime-wasm opts) "/assets/tree-sitter/tree-sitter.wasm"))})]))

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
             "assets/loam-treesitter.js" js
             "assets/loam-treesitter.json" (json/render-json (manifest opts))}
    :asset-files (merge npm-asset-files (:asset-files opts))}))
