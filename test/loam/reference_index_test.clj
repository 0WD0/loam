(ns loam.reference-index-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [loam.compile :as compile]
            [loam.docs-fixtures :as fixtures])
  (:import [java.net URI URLDecoder]
           [java.nio.charset StandardCharsets]))

(defn- index-page [title custom-id path code]
  (fixtures/headline 1 title
                     {:CUSTOM_ID custom-id
                      :EXPORT_FILE_NAME path
                      :DESCRIPTION (str title " description")
                      :INDEX code}
                     (fixtures/section)))

(defn- reference-input []
  (let [explicit-unicode
        (assoc-in
         (fixtures/description-item
          "Key: 中文 (majutsu-unicode)"
          (fixtures/paragraph "Use a Unicode key label."))
         [:properties :CUSTOM_ID]
         "显式%anchor")
        entries
        (fixtures/description-list
         (fixtures/description-item
          "Key: C-c C-c (majutsu-save)"
          (fixtures/paragraph "Save the current change."))
         (fixtures/description-item
          "Key: B (Emacs) / I (Evil) (majutsu-before)"
          (fixtures/paragraph "Insert before the selected revision."))
         (fixtures/description-item
          "Key: --"
          (fixtures/paragraph "Open the path selector."))
         (fixtures/description-item
          "Key: % (majutsu-percent)"
          (fixtures/paragraph "Use the percent key."))
         (fixtures/description-item
          "Key: [[ (majutsu-bracket)"
          (fixtures/paragraph "Use bracket keys."))
         (fixtures/description-item
          "Key: ` (majutsu-backtick)"
          (fixtures/paragraph "Use the backtick key."))
         explicit-unicode
         (fixtures/description-item
          "Command: majutsu-open / majutsu-close"
          (fixtures/paragraph
           "Target remote branch ("
           {:type :code :properties {:value "--remote-branch"} :contents []}
           ")."))
         (fixtures/description-item
          "User Option: majutsu-program"
          (fixtures/paragraph
           "Path to the "
           {:type :code :properties {:value "jj" :post-blank 1} :contents []}
           "binary (default: "
           {:type :verbatim :properties {:value "\"jj\""} :contents []}
           "). "
           {:type :bold :properties {:post-blank 1} :contents ["Bold"]}
           "and "
           {:type :italic :properties {} :contents ["italic <safe>"]}
           "."))
         (fixtures/description-item
          "Face: majutsu-warning"
          (fixtures/paragraph
           "Visual metadata and prose mentioning majutsu-not-an-entry are not symbols.")))
        repeated-key
        (fixtures/description-list
         (fixtures/description-item
          "Key: --"
          (fixtures/paragraph "Limit the log to matching files.")))
        contextual-key
        (assoc-in
         (fixtures/description-list
         (fixtures/description-item
           "Key: n / p"
           (fixtures/paragraph "Move between unresolved conflicts.")))
         [:properties :attr_reference]
         {:type :anonymous
          :properties {}
          :contents [":kind command-binding :interface \"Conflict buffer\" :mode majutsu-conflict-mode :prefix \"C-c ^\" :scope Emacs :state Normal"]})
        ast (fixtures/document
             "References"
             (fixtures/page "Workflow" "workflow" "guide/workflow"
                            (fixtures/section)
                            (fixtures/headline
                             2 "Diffing" {}
                             (fixtures/section)
                             (fixtures/headline
                              3 "Diff Transient" {}
                              (fixtures/section entries)))
                            (fixtures/headline
                             2 "Inspecting" {}
                             (fixtures/section)
                             (fixtures/headline
                              3 "Log Options Transient" {}
                              (fixtures/section repeated-key)))
                            (fixtures/headline
                             2 "Resolving" {}
                             (fixtures/section contextual-key)))
             (index-page "Keystroke Index" "keystroke-index"
                         "reference/keystrokes" "ky")
             (index-page "Function and Command Index" "function-command-index"
                         "reference/functions-commands" "fn")
             (index-page "Variable Index" "variable-index"
                         "reference/variables" "vr"))]
    (fixtures/envelope-input "docs/references.org" ast)))

(defn- html-ids [html]
  (mapv second (re-seq #"(?:^|\s)id=\"([^\"]+)\"" html)))

(defn- hrefs [html]
  (mapv second (re-seq #"(?:^|\s)href=\"([^\"]+)\"" html)))

(defn- decoded-fragment [href]
  (let [raw (.getRawFragment (URI. href))]
    (when raw
      (URLDecoder/decode raw (.name StandardCharsets/UTF_8)))))

(deftest builds-linked-reference-pages-from-explicit-description-terms
  (let [result (compile/compile-documents [(reference-input)] fixtures/compile-opts)
        references (get-in result [:index :references])
        source-html (get-in result [:artifacts :fragments "pages/guide-workflow.html"])
        key-html (get-in result [:artifacts :fragments "pages/reference-keystrokes.html"])
        function-html (get-in result [:artifacts :fragments
                                      "pages/reference-functions-commands.html"])
        variable-html (get-in result [:artifacts :fragments "pages/reference-variables.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= {"ky" 9 "fn" 8 "vr" 1}
           (into {} (map (fn [[code entries]] [code (count entries)]) references))))
    (is (str/includes? source-html
                       "id=\"ref-source-key-x3a-c-c-c-c-x28-majutsu-save-x29\""))
    (is (str/includes? key-html
                       "href=\"/docs/dev/guide/workflow/#ref-source-key-x3a-c-c-c-c-x28-majutsu-save-x29\""))
    (is (str/includes? key-html "<kbd>C-c C-c</kbd>"))
    (is (str/includes? key-html "data-reference-count=\"9\""))
    (is (str/includes? key-html
                       "data-reference-context=\"Workflow › Diffing › Diff Transient\""))
    (is (str/includes? key-html
                       "data-reference-context=\"Workflow › Inspecting › Log Options Transient\""))
    (is (= 2 (count (re-seq #"data-reference-identity=\"--\"" key-html))))
    (is (str/includes? key-html "data-reference-kind=\"command-binding\""))
    (is (str/includes? key-html "data-reference-kind=\"transient-argument\""))
    (is (str/includes? key-html "data-reference-command=\"majutsu-save\""))
    (is (str/includes? key-html "data-reference-scope=\"Emacs Evil\""))
    (is (str/includes? key-html "data-reference-interface=\"Conflict buffer\""))
    (is (str/includes? key-html "data-reference-mode=\"majutsu-conflict-mode\""))
    (is (str/includes? key-html "data-reference-prefix=\"C-c ^\""))
    (is (str/includes? key-html "data-reference-state=\"Normal\""))
    (is (str/includes? key-html
                       "class=\"org-reference-facts org-reference-key-facts\""))
    (is (str/includes? key-html
                       "class=\"org-reference-facts org-reference-context-facts\""))
    (is (str/includes? key-html "aria-label=\"Binding identity\""))
    (is (str/includes? key-html "aria-label=\"Command: majutsu-save\""))
    (is (str/includes? key-html "class=\"org-reference-fact-value org-reference-command-list\""))
    (is (str/includes? key-html ">Scope</span><span>Emacs, Evil</span>"))
    (is (str/includes? key-html ">Interface</span><span>Conflict buffer</span>"))
    (is (str/includes? key-html ">Mode</span><code>majutsu-conflict-mode</code>"))
    (is (str/includes? key-html ">Prefix</span><kbd>C-c ^</kbd>"))
    (is (str/includes? key-html ">State</span><span>Normal</span>"))
    (is (str/includes? source-html "id=\"显式%anchor\""))
    (is (str/includes?
         key-html
         "href=\"/docs/dev/guide/workflow/#%E6%98%BE%E5%BC%8F%25anchor\""))
    (is (str/includes? key-html "id=\"ref-index-ky-x25\""))
    (is (str/includes? key-html "id=\"ref-index-ky-x5b-x5b\""))
    (is (str/includes? key-html "id=\"ref-index-ky-x60\""))
    (is (str/includes? key-html "id=\"ref-index-ky-u4e2d-u6587\""))
    (doseq [symbol ["majutsu-save" "majutsu-open" "majutsu-close"]]
      (testing symbol
        (is (str/includes? function-html (str "<code>" symbol "</code>")))))
    (is (str/includes? function-html "Target remote branch (--remote-branch)."))
    (is (str/includes? variable-html "<code>majutsu-program</code>"))
    (is (str/includes?
         variable-html
         "Path to the jj binary (default: &quot;jj&quot;). Bold and italic &lt;safe&gt;."))
    (is (not (str/includes? variable-html "default: ).")))
    (doseq [html [source-html key-html function-html variable-html]
            :let [ids (html-ids html)]]
      (is (= (count ids) (count (set ids))))
      (doseq [id (filter #(str/starts-with? % "ref-") ids)]
        (is (re-matches #"[a-z0-9]+(?:-[a-z0-9]+)*" id) id))
      (doseq [href (hrefs html)]
        (is (not (re-find #"%(?![0-9A-Fa-f]{2})" href)) href)
        ;; java.net.URI plus UTF-8 decoding is the JVM equivalent of the
        ;; browser's URL/decodeURIComponent validation.
        (is (string? (or (decoded-fragment href) "")) href)))
    (let [source-id-set (set (html-ids source-html))]
      (doseq [href (filter #(str/includes? % "/guide/workflow/#")
                           (concat (hrefs key-html)
                                   (hrefs function-html)
                                   (hrefs variable-html)))]
        (is (contains? source-id-set (decoded-fragment href)) href)))
    (is (= (get-in result [:artifacts :files])
           (get-in (compile/compile-documents [(reference-input)] fixtures/compile-opts)
                   [:artifacts :files])))))

(deftest renders-a-clear-empty-state-for-an-authored-index-page
  (let [ast (fixtures/document
             "Empty References"
             (index-page "Variable Index" "variable-index" "reference/variables" "vr"))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/empty-references.org" ast)]
                fixtures/compile-opts)
        html (get-in result [:artifacts :fragments "pages/reference-variables.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (str/includes? html "data-reference-count=\"0\""))
    (is (str/includes? html "org-reference-index-empty"))
    (is (str/includes? html
                       "No explicitly authored variable and user-option entries were found."))))
