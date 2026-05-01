(ns loam.highlight.treesitter.client
  "Browser-side Tree-sitter highlighter built with shadow-cljs."
  (:require [clojure.string :as str]
            [goog.object :as gobj]))

(def default-config-url "/assets/loam-treesitter.json")
(def default-runtime-global "LoamTreeSitter")
(def default-runtime-wasm-url "/assets/tree-sitter/tree-sitter.wasm")
(def runtime-ready-event "loam:tree-sitter-runtime")

(defn script-data [key]
  (some-> js/document
          .-currentScript
          .-dataset
          (gobj/get key)))

(def config-url
  (or (script-data "config") default-config-url))

(def runtime-global
  (or (script-data "runtimeGlobal") default-runtime-global))

(def runtime-wasm-data
  (script-data "runtimeWasm"))

(defn absolute-url [url]
  (str (js/URL. url (.-baseURI js/document))))

(defn asset-url [url]
  (if (str/starts-with? url "/")
    (str (js/URL. url (.. js/window -location -origin)))
    (absolute-url url)))

(defn set-status! [code-el status message]
  (when-let [pre (.closest code-el "pre")]
    (when status
      (.setAttribute pre "data-loam-treesitter-status" status))
    (when message
      (.setAttribute pre "data-loam-treesitter-message" message))))

(defn clear-status! [code-el]
  (when-let [pre (.closest code-el "pre")]
    (.removeAttribute pre "data-loam-treesitter-status")
    (.removeAttribute pre "data-loam-treesitter-message")))

(defn capture-class [capture]
  (str "ts-"
       (-> (or capture "")
           str
           (str/replace #"^@" "")
           str/lower-case
           (str/replace #"[^a-z0-9_-]+" "-")
           (str/replace #"^-+|-+$" ""))))

(defn language-name [code-el]
  (or (.getAttribute code-el "data-language")
      (some-> (.closest code-el "pre")
              (.getAttribute "data-language"))
      "text"))

(defn language-config [manifest lang]
  (if-let [conf (get-in manifest [:languages lang])]
    (assoc conf :canonical lang)
    (some (fn [[name conf]]
            (when (some #{lang} (:aliases conf))
              (assoc conf :canonical name)))
          (:languages manifest))))

(defn fetch-json [url]
  (-> (js/fetch (asset-url url))
      (.then (fn [response]
               (if (.-ok response)
                 (.json response)
                 (throw (js/Error. (str "failed to load " url ": " (.-status response)))))))
      (.then (fn [value]
               (js->clj value :keywordize-keys true)))))

(defn fetch-text [url]
  (-> (js/fetch (asset-url url))
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (throw (js/Error. (str "failed to load " url ": " (.-status response)))))))))

(defn runtime-value []
  (gobj/get js/globalThis runtime-global))

(defn wait-runtime []
  (if-let [runtime (runtime-value)]
    (.resolve js/Promise runtime)
    (js/Promise.
     (fn [resolve reject]
       (let [handler* (atom nil)
             timeout-id (.setTimeout js/window
                                     (fn []
                                       (when-let [handler @handler*]
                                         (.removeEventListener js/window runtime-ready-event handler))
                                       (reject (js/Error. "tree-sitter runtime not loaded")))
                                     10000)
             handler (fn [event]
                       (let [runtime (or (runtime-value)
                                         (gobj/get event "detail"))]
                         (when runtime
                           (.clearTimeout js/window timeout-id)
                           (when-let [handler @handler*]
                             (.removeEventListener js/window runtime-ready-event handler))
                           (resolve runtime))))]
         (reset! handler* handler)
         (.addEventListener js/window runtime-ready-event handler))))))

(defn runtime-api [runtime]
  (let [Parser (or (gobj/get runtime "Parser")
                   (gobj/get runtime "default")
                   runtime)
        Language (or (gobj/get runtime "Language")
                     (gobj/get Parser "Language"))
        Query (or (gobj/get runtime "Query")
                  (gobj/get Parser "Query"))]
    {:parser Parser
     :language Language
     :query Query}))

(defn init-runtime! [runtime runtime-wasm-url]
  (let [Parser (:parser runtime)]
    (if (gobj/get Parser "__loamInitialized")
      (.resolve js/Promise nil)
      (-> (.init Parser #js {:locateFile (fn [script-name]
                                           (asset-url
                                            (if (str/ends-with? script-name ".wasm")
                                              runtime-wasm-url
                                              script-name)))})
          (.then (fn [value]
                   (gobj/set Parser "__loamInitialized" true)
                   value))))))

(defn seq-from [value]
  (array-seq (.from js/Array (or value #js []))))

(defn query-captures [query root-node]
  (if-let [captures-fn (gobj/get query "captures")]
    (seq-from (.call captures-fn query root-node))
    (if-let [matches-fn (gobj/get query "matches")]
      (mapcat (fn [match]
                (seq-from (gobj/get match "captures")))
              (seq-from (.call matches-fn query root-node)))
      [])))

(defn node-start [node]
  (or (gobj/get node "startIndex")
      (some-> (gobj/get node "startPosition")
              (gobj/get "index"))
      0))

(defn node-end [node]
  (or (gobj/get node "endIndex")
      (some-> (gobj/get node "endPosition")
              (gobj/get "index"))
      (node-start node)))

(defn capture-name [capture query]
  (or (gobj/get capture "name")
      (when-let [index (gobj/get capture "index")]
        (if-let [capture-name-fn (or (gobj/get query "captureNameForId")
                                     (gobj/get query "getCaptureNameForId"))]
          (.call capture-name-fn query index)
          (str index)))
      "unknown"))

(defn normalize-ranges [captures query]
  (let [ranges (->> captures
                    (keep (fn [capture]
                            (let [node (gobj/get capture "node")
                                  start (node-start node)
                                  end (node-end node)]
                              (when (> end start)
                                {:start start
                                 :end end
                                 :capture (capture-name capture query)}))))
                    (sort-by (juxt :start (comp - :end) :capture)))]
    (loop [remaining ranges
           cursor 0
           accepted []]
      (if-let [range (first remaining)]
        (if (< (:start range) cursor)
          (recur (rest remaining) cursor accepted)
          (recur (rest remaining) (:end range) (conj accepted range)))
        accepted))))

(defn append-text! [fragment source start end]
  (when (< start end)
    (.appendChild fragment (.createTextNode js/document (.slice source start end)))))

(defn render-ranges! [code-el source ranges]
  (let [fragment (.createDocumentFragment js/document)]
    (loop [remaining ranges
           cursor 0]
      (if-let [range (first remaining)]
        (do
          (append-text! fragment source cursor (:start range))
          (let [span (.createElement js/document "span")]
            (set! (.-className span) (capture-class (:capture range)))
            (set! (.-textContent span) (.slice source (:start range) (:end range)))
            (.appendChild fragment span))
          (recur (rest remaining) (:end range)))
        (append-text! fragment source cursor (.-length source))))
    (.replaceChildren code-el fragment)
    (.setAttribute code-el "data-loam-treesitter-done" "true")))

(defn make-query [runtime language query-text]
  (let [query-fn (gobj/get language "query")]
    (if query-fn
      (.call query-fn language query-text)
      (let [Query (:query runtime)]
        (new Query language query-text)))))

(defn load-highlighter [runtime conf]
  (let [Parser (:parser runtime)
        Language (:language runtime)]
    (-> (.load Language (asset-url (:wasm conf)))
        (.then (fn [language]
                 (-> (fetch-text (:query conf))
                     (.then (fn [query-text]
                              (let [parser (new Parser)
                                    query (make-query runtime language query-text)]
                                (.setLanguage parser language)
                                {:parser parser :query query})))))))))

(defn get-highlighter [runtime manifest highlighters lang]
  (if-let [conf (language-config manifest lang)]
    (let [key (or (:canonical conf) lang)]
      (if (.has highlighters key)
        (.get highlighters key)
        (let [promise (load-highlighter runtime conf)]
          (.set highlighters key promise)
          promise)))
    (.resolve js/Promise nil)))

(defn highlight-block! [runtime manifest highlighters code-el]
  (let [lang (language-name code-el)]
    (set-status! code-el "loading" nil)
    (-> (get-highlighter runtime manifest highlighters lang)
        (.then (fn [highlighter]
                 (if-not highlighter
                   (set-status! code-el "missing" (str "no tree-sitter parser for " lang))
                   (let [source (or (.-textContent code-el) "")
                         tree (.parse (:parser highlighter) source)
                         captures (query-captures (:query highlighter) (gobj/get tree "rootNode"))
                         ranges (normalize-ranges captures (:query highlighter))]
                     (render-ranges! code-el source ranges)
                     (clear-status! code-el)))))
        (.catch (fn [error]
                  (.error js/console "[loam-treesitter]" error)
                  (set-status! code-el "error" "tree-sitter error"))))))

(defn code-blocks []
  (->> (seq-from (.querySelectorAll js/document "code[data-loam-treesitter]"))
       (remove #(= "true" (.getAttribute % "data-loam-treesitter-done")))))

(defn start! []
  (let [blocks (vec (code-blocks))]
    (when (seq blocks)
      (doseq [code-el blocks]
        (set-status! code-el "loading" nil))
      (-> (fetch-json config-url)
          (.then (fn [manifest]
                   (let [runtime-wasm-url (or runtime-wasm-data
                                              (:runtime-wasm manifest)
                                              default-runtime-wasm-url)]
                     (-> (wait-runtime)
                         (.then (fn [runtime]
                                  (let [runtime (runtime-api runtime)]
                                    (-> (init-runtime! runtime runtime-wasm-url)
                                        (.then (fn []
                                                 (let [highlighters (js/Map.)]
                                                   (reduce (fn [promise code-el]
                                                             (.then promise
                                                                    (fn [_]
                                                                      (highlight-block! runtime manifest highlighters code-el))))
                                                           (.resolve js/Promise nil)
                                                           blocks))))))))))))
          (.catch (fn [error]
                    (.error js/console "[loam-treesitter]" error)
                    (doseq [code-el blocks]
                      (set-status! code-el "error" "tree-sitter error"))))))))

(defn init []
  (if (= "loading" (.-readyState js/document))
    (.addEventListener js/document "DOMContentLoaded" start! #js {:once true})
    (start!)))

(init)
