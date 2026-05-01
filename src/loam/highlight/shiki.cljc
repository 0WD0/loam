(ns loam.highlight.shiki
  "Client-side Shiki syntax highlighting extension."
  (:require [loam.head :as head]
            [loam.highlight.core :as highlight]
            [loam.json :as json]))

(def default-languages
  ["clojure" "javascript" "typescript" "python" "bash" "html" "css" "json" "markdown"])

(def default-theme "github-dark")

(def default-language-aliases
  {"cljs" "clojure"
   "cljc" "clojure"
   "edn" "clojure"
   "js" "javascript"
   "ts" "typescript"
   "py" "python"})

(def default-import "https://esm.sh/shiki@4.0.2")

(def css
  (str
   ".loam-shiki-src{position:relative}.loam-shiki-src code{white-space:pre}"
   ".loam-shiki-src[data-loam-shiki-status=loading]::after{content:'highlighting';position:absolute;top:.45rem;right:.6rem;color:#94a3b8;font-size:.75rem}"
   ".loam-shiki-src[data-loam-shiki-status=missing]::after,.loam-shiki-src[data-loam-shiki-status=error]::after{content:attr(data-loam-shiki-message);position:absolute;top:.45rem;right:.6rem;color:#fca5a5;font-size:.75rem}"))

(defn render-src-block [_opts]
  (highlight/render-src-block
   {:pre-class "loam-shiki-src"
    :code-attrs {:data-loam-shiki true}}))

(defn shiki-client-js [opts]
  (let [import-spec (or (:import opts) default-import)
        theme (or (:theme opts) default-theme)
        languages (or (:languages opts) default-languages)
        language-aliases (merge default-language-aliases (:language-aliases opts))]
    (str
     "import { createHighlighter } from " (json/render-json import-spec) ";\n"
     "const theme = " (json/render-json theme) ";\n"
     "const languages = " (json/render-json languages) ";\n"
     "const languageAliases = " (json/render-json language-aliases) ";\n"
     "const highlighterPromise = createHighlighter({ themes: [theme], langs: languages });\n"
     "const setStatus = (codeEl, status, message) => {\n"
     "  const pre = codeEl.closest('pre');\n"
     "  if (!pre) return;\n"
     "  if (status) pre.dataset.loamShikiStatus = status;\n"
     "  if (message) pre.dataset.loamShikiMessage = message;\n"
     "};\n"
     "const clearStatus = (codeEl) => {\n"
     "  const pre = codeEl.closest('pre');\n"
     "  if (!pre) return;\n"
     "  delete pre.dataset.loamShikiStatus;\n"
     "  delete pre.dataset.loamShikiMessage;\n"
     "};\n"
     "const languageName = (codeEl) => {\n"
     "  const lang = codeEl.dataset.language || codeEl.closest('pre')?.dataset.language || 'text';\n"
     "  return languageAliases[lang] || lang;\n"
     "};\n"
     "const highlightedPre = (html) => {\n"
     "  const template = document.createElement('template');\n"
     "  template.innerHTML = html.trim();\n"
     "  return template.content.querySelector('pre');\n"
     "};\n"
     "const highlightBlock = async (highlighter, codeEl) => {\n"
     "  const source = codeEl.textContent || '';\n"
     "  const lang = languageName(codeEl);\n"
     "  try {\n"
     "    setStatus(codeEl, 'loading');\n"
     "    if (!highlighter.getLoadedLanguages().includes(lang)) {\n"
     "      await highlighter.loadLanguage(lang);\n"
     "    }\n"
     "    const html = highlighter.codeToHtml(source, {lang, theme});\n"
     "    const nextPre = highlightedPre(html);\n"
     "    const currentPre = codeEl.closest('pre');\n"
     "    if (!nextPre || !currentPre) return;\n"
     "    nextPre.classList.add('loam-src', 'loam-shiki-src');\n"
     "    nextPre.dataset.language = lang;\n"
     "    nextPre.querySelector('code')?.setAttribute('data-language', lang);\n"
     "    nextPre.querySelector('code')?.setAttribute('data-loam-shiki-done', 'true');\n"
     "    currentPre.replaceWith(nextPre);\n"
     "  } catch (error) {\n"
     "    console.error('[loam-shiki]', error);\n"
     "    setStatus(codeEl, 'error', 'shiki error');\n"
     "  }\n"
     "};\n"
     "const main = async () => {\n"
     "  const blocks = [...document.querySelectorAll('code[data-loam-shiki]')]\n"
     "    .filter((el) => el.dataset.loamShikiDone !== 'true');\n"
     "  if (!blocks.length) return;\n"
     "  for (const block of blocks) setStatus(block, 'loading');\n"
     "  const highlighter = await highlighterPromise;\n"
     "  for (const block of blocks) await highlightBlock(highlighter, block);\n"
     "};\n"
     "if (document.readyState === 'loading') {\n"
     "  document.addEventListener('DOMContentLoaded', main, {once: true});\n"
     "} else {\n"
     "  main();\n"
     "}\n")))

(defn head-tags [_opts]
  (fn [_ctx current-url]
    [(head/stylesheet current-url "/assets/loam-shiki.css")
     (head/script current-url "/assets/loam-shiki.js"
                  {:type "module"})]))

(defn extension
  "Create the Shiki highlighter extension.

  Options:
  - :theme Shiki theme name, default github-dark
  - :languages vector of Shiki language names to preload
  - :language-aliases map of rendered language -> Shiki language
  - :import ESM import specifier for Shiki, default CDN package"
  ([] (extension {}))
  ([opts]
   {:id :loam.highlight/shiki
    :loam.highlight/provider :shiki
    :extends {:renderers {:src-block (render-src-block opts)}
              :head [(head-tags opts)]
              :assets {"assets/loam-shiki.css" css
                       "assets/loam-shiki.js" (shiki-client-js opts)}}}))
