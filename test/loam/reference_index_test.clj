(ns loam.reference-index-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [loam.compile :as compile]
            [loam.docs-fixtures :as fixtures]))

(defn- index-page [title custom-id path code]
  (fixtures/headline 1 title
                     {:CUSTOM_ID custom-id
                      :EXPORT_FILE_NAME path
                      :DESCRIPTION (str title " description")
                      :INDEX code}
                     (fixtures/section)))

(defn- reference-input []
  (let [entries
        (fixtures/description-list
         (fixtures/description-item
          "Key: C-c C-c (majutsu-save)"
          (fixtures/paragraph "Save the current change."))
         (fixtures/description-item
          "Key: --"
          (fixtures/paragraph "Open the path selector."))
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
        ast (fixtures/document
             "References"
             (fixtures/page "Workflow" "workflow" "guide/workflow"
                            (fixtures/section entries))
             (index-page "Keystroke Index" "keystroke-index"
                         "reference/keystrokes" "ky")
             (index-page "Function and Command Index" "function-command-index"
                         "reference/functions-commands" "fn")
             (index-page "Variable Index" "variable-index"
                         "reference/variables" "vr"))]
    (fixtures/envelope-input "docs/references.org" ast)))

(deftest builds-linked-reference-pages-from-explicit-description-terms
  (let [result (compile/compile-documents [(reference-input)] fixtures/compile-opts)
        references (get-in result [:index :references])
        source-html (get-in result [:artifacts :fragments "pages/guide-workflow.html"])
        key-html (get-in result [:artifacts :fragments "pages/reference-keystrokes.html"])
        function-html (get-in result [:artifacts :fragments
                                      "pages/reference-functions-commands.html"])
        variable-html (get-in result [:artifacts :fragments "pages/reference-variables.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= {"ky" 2 "fn" 3 "vr" 1}
           (into {} (map (fn [[code entries]] [code (count entries)]) references))))
    (is (str/includes? source-html
                       "id=\"ref-source-key:-c-c-c-c-(majutsu-save)\""))
    (is (str/includes? key-html
                       "href=\"/docs/dev/guide/workflow/#ref-source-key:-c-c-c-c-(majutsu-save)\""))
    (is (str/includes? key-html "<kbd>C-c C-c</kbd>"))
    (is (str/includes? key-html "data-reference-count=\"2\""))
    (doseq [symbol ["majutsu-save" "majutsu-open" "majutsu-close"]]
      (testing symbol
        (is (str/includes? function-html (str "<code>" symbol "</code>")))))
    (is (str/includes? function-html "Target remote branch (--remote-branch)."))
    (is (str/includes? variable-html "<code>majutsu-program</code>"))
    (is (str/includes?
         variable-html
         "Path to the jj binary (default: &quot;jj&quot;). Bold and italic &lt;safe&gt;."))
    (is (not (str/includes? variable-html "default: ).")))
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
