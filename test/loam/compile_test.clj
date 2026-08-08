(ns loam.compile-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [loam.compile :as compile]
            [loam.diagnostic :as diagnostic]
            [loam.docs-fixtures :as fixtures]
            [loam.emit.starlight :as starlight]))

(defn canonical-input []
  (starlight/read-envelope-input "." "test/fixtures/consumer-envelope-v1.edn"))

(deftest consumes-the-byte-frozen-ox-edn-envelope-v1
  (let [envelope (edn/read-string (slurp "test/fixtures/consumer-envelope-v1.edn"))
        expected-fragment (edn/read-string (slurp "test/fixtures/golden/minimal-fragment.edn"))
        result (compile/compile-documents [(canonical-input)]
                                          {:build {:commitId fixtures/build-commit-id}})
        manifest (get-in result [:artifacts :manifest])
        manifest-json (get-in result [:artifacts :files "manifest.json"])]
    (is (= [:ox-edn/schema-version :ox-edn/exporter :ox-edn/source
            :ox-edn/document :ox-edn/diagnostics]
           (vec (keys envelope))))
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= compile/phase-order (:phase-trace result)))
    (is (= 1 (:schemaVersion manifest)))
    (is (= [{:path "test/fixtures/minimal.org"
             :sha256 "d9c591eec2cda2c5af741896200ff93c76ac43b2f306e2b3a51c783b73e7b613"}]
           (:sources manifest)))
    (is (not (contains? manifest :source)))
    (is (= "test-commit" (get-in manifest [:build :commitId])))
    (is (not (contains? (:build manifest) :vcs)))
    (is (= "/docs/dev/hello/" (get-in manifest [:pages 0 :route])))
    (is (= 3 (get-in manifest [:pages 0 :source :startLine])))
    (is (= expected-fragment
           (get-in result [:artifacts :fragments "pages/hello.html"])))
    (is (str/includes? manifest-json "\"sources\":["))
    (is (not (str/includes? manifest-json "Hello, ox-edn.")))
    (is (not (str/includes? (pr-str (:artifacts result)) "/home/")))))

(deftest compiler-output-is-byte-deterministic
  (let [opts {:build {:commitId fixtures/build-commit-id}}
        first-result (compile/compile-documents [(canonical-input)] opts)
        second-result (compile/compile-documents [(canonical-input)] opts)]
    (is (= :ok (:status first-result)))
    (is (= (get-in first-result [:artifacts :files])
           (get-in second-result [:artifacts :files])))
    (is (= (get-in first-result [:artifacts :manifest :build :contentHash])
           (get-in second-result [:artifacts :manifest :build :contentHash])))))

(deftest validates-envelope-schema-hash-diagnostics-and-optional-build-metadata
  (let [base (canonical-input)
        cases
        [[:unsupported-envelope-version
          (assoc-in base [:envelope :ox-edn/schema-version] 2)]
         [:unsafe-source-path
          (assoc-in base [:envelope :ox-edn/source :path] "/home/user/manual.org")]
         [:invalid-source-digest
          (assoc-in base [:envelope :ox-edn/source :sha256]
                    (str/upper-case (get-in base [:envelope :ox-edn/source :sha256])))]
         [:source-digest-mismatch
          (assoc base :source-content "changed")]
         [:invalid-producer-diagnostic
          (assoc-in base [:envelope :ox-edn/diagnostics]
                    [{:severity :warning :code "not-a-keyword" :message "bad"}])]]]
    (doseq [[code input] cases]
      (testing (name code)
        (let [result (compile/compile-documents
                      [input] {:build {:commitId fixtures/build-commit-id}})]
          (is (= :error (:status result)))
          (is (some #(= code (:code %)) (:diagnostics result)))
          (is (nil? (:artifacts result))))))
    (let [local-build (compile/compile-documents [(canonical-input)] {})]
      (is (= :ok (:status local-build)) (:diagnostics local-build))
      (is (not (contains? (get-in local-build [:artifacts :manifest :build])
                          :commitId))))
    (doseq [commit-id [nil "" 42]]
      (let [invalid-commit (compile/compile-documents
                            [(canonical-input)]
                            {:build {:commitId commit-id}})]
        (is (= :error (:status invalid-commit)))
        (is (some #(and (= :invalid-build-commit (:code %))
                        (= :commitId (get-in % [:data :field])))
                  (:diagnostics invalid-commit)))))))

(deftest strict-renderer-never-falls-back-to-children
  (doseq [[node code]
          [[{:type :unknown-semantic-node :properties {} :contents ["must not leak"]}
            :unknown-node-type]
           [{:type :export-block :properties {:value "<script>bad()</script>"} :contents []}
            :rejected-node-type]]]
    (testing (name code)
      (let [ast (fixtures/document "Strict"
                                   (fixtures/page "Strict" "strict" "guide/strict"
                                                  (fixtures/section node)))
            result (compile/compile-documents
                    [(fixtures/envelope-input "docs/strict.org" ast)]
                    fixtures/compile-opts)]
        (is (= :error (:status result)))
        (is (some #(= code (:code %)) (:diagnostics result)))
        (when (= :unknown-node-type code)
          (let [diagnostic (first (filter #(= code (:code %)) (:diagnostics result)))]
            (is (str/includes? (:message diagnostic) ":unknown-semantic-node"))
            (is (= :unknown-semantic-node (get-in diagnostic [:data :node-type])))))
        (is (nil? (:artifacts result)))))))

(deftest missing-latex-preview-is-a-render-error
  (let [latex {:type :latex-fragment :properties {:value "$x$"} :contents []}
        ast (fixtures/document "Missing LaTeX"
                               (fixtures/page "Missing LaTeX" "missing-latex" "guide/missing-latex"
                                              (fixtures/section (fixtures/paragraph latex))))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/missing-latex.org" ast)]
                fixtures/compile-opts)]
    (is (= :error (:status result)))
    (is (some #(= :missing-latex-preview (:code %)) (:diagnostics result)))
    (is (nil? (:artifacts result)))))

(deftest renders-sanitized-latex-svg-preview
  (let [svg "<?xml version='1.0'?><svg version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='10pt' height='5pt' viewBox='0 0 10 5'><defs><path id='g0' d='m0 0h1v1z'/></defs><g fill='currentColor'><use xlink:href='#g0'/></g></svg>"
        latex {:type :latex-fragment
               :properties {:value "$x$"
                            :ox-edn/latex-preview {:provider :org-latex-preview
                                                   :process 'dvisvgm
                                                   :format :svg
                                                   :sha256 "abc123"
                                                   :svg svg
                                                   :width 1.4
                                                   :height 0.8
                                                   :depth 0.1}}
               :contents []}
        ast (fixtures/document "LaTeX"
                               (fixtures/page "LaTeX" "latex" "guide/latex"
                                              (fixtures/section (fixtures/paragraph latex))))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/latex.org" ast)]
                fixtures/compile-opts)
        fragment (get-in result [:artifacts :fragments "pages/guide-latex.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (false? (get-in result [:artifacts :manifest :build :unreleasable])))
    (is (str/includes? fragment "class=\"org-latex org-latex-inline\""))
    (is (str/includes? fragment "class=\"org-latex-svg\""))
    (is (str/includes? fragment "fill=\"currentColor\""))
    (is (str/includes? fragment "id=\"latex-abc123-g0\""))
    (is (str/includes? fragment "xlink:href=\"#latex-abc123-g0\""))
    (is (not (str/includes? fragment "<?xml")))))

(deftest preserves-inline-post-blank-whitespace
  (let [verbatim {:type :verbatim
                  :properties {:value "jj" :post-blank 1}
                  :contents []}
        ast (fixtures/document "Whitespace"
                               (fixtures/page "Whitespace" "whitespace" "guide/whitespace"
                                              (fixtures/section
                                               (fixtures/paragraph
                                                "Use the " verbatim "CLI."))))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/whitespace.org" ast)]
                fixtures/compile-opts)
        fragment (get-in result [:artifacts :fragments "pages/guide-whitespace.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (str/includes? fragment
                       "Use the <code class=\"org-verbatim\">jj</code> CLI."))
    (is (not (str/includes? fragment "</code>CLI")))))

(deftest renders-kbd-macro-from-its-arguments
  (let [macro {:type :macro
               :properties {:key "kbd"
                            :args {:type :anonymous :properties {} :contents ["C-c C-c"]}
                            :value "{{{kbd(C-c C-c)}}}"}
               :contents []}
        ast (fixtures/document "Macro"
                               (fixtures/page "Macro" "macro" "guide/macro"
                                              (fixtures/section
                                               (fixtures/paragraph "Press " macro "."))))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/macro.org" ast)]
                fixtures/compile-opts)
        fragment (get-in result [:artifacts :fragments "pages/guide-macro.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (str/includes? fragment "<kbd>C-c C-c</kbd>"))
    (is (not (str/includes? fragment "{{{kbd")))))

(deftest renders-emacs-font-lock-runs-without-reinterpreting-source
  (let [source "(defun demo () \"hello\")\n"
        src {:type :src-block
             :properties {:language "emacs-lisp"
                          :value source
                          :ox-edn/font-lock
                          {:provider :emacs-font-lock
                           :mode "emacs-lisp-mode"
                           :runs [{:start 1
                                   :end 6
                                   :faces ["font-lock-keyword-face"]}
                                  {:start 7
                                   :end 11
                                   :faces ["font-lock-function-name-face"]}]}}
             :contents []}
        ast (fixtures/document "Font lock"
                               (fixtures/page "Font lock" "font-lock" "guide/font-lock"
                                              (fixtures/section src)))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/font-lock.org" ast)]
                fixtures/compile-opts)
        fragment (get-in result [:artifacts :fragments "pages/guide-font-lock.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (str/includes? fragment "data-highlight-provider=\"emacs-font-lock\""))
    (is (str/includes? fragment "data-emacs-mode=\"emacs-lisp-mode\""))
    (is (str/includes? fragment
                       "<span class=\"ef-font-lock-keyword-face\" data-emacs-faces=\"font-lock-keyword-face\">defun</span>"))
    (is (str/includes? fragment
                       "<span class=\"ef-font-lock-function-name-face\" data-emacs-faces=\"font-lock-function-name-face\">demo</span>"))
    (is (str/includes? fragment "&quot;hello&quot;"))))

(deftest renders-checkbox-inline-with-its-first-item-paragraph
  (let [checked {:type :item
                 :properties {:checkbox :on}
                 :contents [(fixtures/paragraph "Org remains source of truth")]}
        mixed {:type :item
               :properties {:checkbox :trans}
               :contents [(fixtures/paragraph "Coverage is still growing")]}
        list-node {:type :plain-list
                   :properties {:type :unordered}
                   :contents [checked mixed]}
        ast (fixtures/document "Checkboxes"
                               (fixtures/page "Checkboxes" "checkboxes" "guide/checkboxes"
                                              (fixtures/section list-node)))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/checkboxes.org" ast)]
                fixtures/compile-opts)
        fragment (get-in result [:artifacts :fragments "pages/guide-checkboxes.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (str/includes? fragment "<li><p class=\"org-paragraph\"><input"))
    (is (str/includes? fragment ">Org remains source of truth</p></li>"))
    (is (str/includes? fragment "checked disabled"))
    (is (str/includes? fragment
                       "aria-checked=\"mixed\""))
    (is (not (str/includes? fragment "</input><p")))))

(deftest renders-texinfo-compatible-keymap-scopes-as-html-data
  (let [vanilla (assoc (fixtures/paragraph "Use n and p.")
                       :properties {:attr_keymap ":scope Vanilla"})
        evil-list (assoc (fixtures/description-list
                          (fixtures/description-item "Key: C-j" (fixtures/paragraph "Move.")))
                         :properties {:type :descriptive
                                      :attr_keymap ":scope Evil"})
        ast (fixtures/document "Keymaps"
                               (fixtures/page "Keymaps" "keymaps" "guide/keymaps"
                                              (fixtures/section vanilla evil-list)))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/keymaps.org" ast)]
                fixtures/compile-opts)
        fragment (get-in result [:artifacts :fragments "pages/guide-keymaps.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (str/includes? fragment
                       "<p class=\"org-paragraph\" data-keymap-scope=\"Vanilla\">"))
    (is (str/includes? fragment
                       "<dl data-keymap-scope=\"Evil\">"))))

(deftest compiler-service-targets-compose-through-the-pipeline
  (let [extension
        {:id :test/compiler-phases
         :extends
         {:validators [(fn [_ _] [])]
          :document-transforms [(fn [document _] (assoc document :test/transformed true))]
          :page-partitioners [(fn [partition _] (assoc partition :test/partitioned true))]
          :diagnostic-rules [(fn [_ _]
                               [(diagnostic/info :test-rule "rule ran"
                                                 {:phase :render :node-type :org-data
                                                  :source-span {}})])]
          :emitters [(fn [artifacts _] (assoc artifacts :test/emitted true))]
          :renderers {:paragraph (fn [_ _ _] [[:p {:class "overridden"} "override"]])}}}
        opts (assoc fixtures/compile-opts :extensions [extension])
        result (compile/compile-documents [(fixtures/sixteen-page-input)] opts)]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (true? (get-in result [:documents 0 :test/transformed])))
    (is (true? (get-in result [:partition :test/partitioned])))
    (is (true? (get-in result [:artifacts :test/emitted])))
    (is (some #(= :test-rule (:code %)) (:diagnostics result)))
    (is (str/includes? (get-in result [:artifacts :fragments "pages/guide-introduction.html"])
                       "class=\"overridden\""))))

(deftest rejects-machine-paths-in-public-artifacts
  (let [ast (fixtures/document "Leak"
                               (fixtures/headline 1 "Leak"
                                                  {:CUSTOM_ID "leak"
                                                   :EXPORT_FILE_NAME "guide/leak"
                                                   :DESCRIPTION "local /home/alice/private"}
                                                  (fixtures/section (fixtures/paragraph "body"))))
        result (compile/compile-documents [(fixtures/envelope-input "docs/leak.org" ast)]
                                          fixtures/compile-opts)]
    (is (= :error (:status result)))
    (is (some #(= :absolute-path-leak (:code %)) (:diagnostics result)))
    (is (nil? (:artifacts result)))))


(deftest personal-profile-emits-unversioned-metadata-search-and-link-graph
  (let [project (fixtures/headline
                 1 "Majutsu"
                 {:ID "11111111-1111-4111-8111-111111111111"
                  :CUSTOM_ID "majutsu"
                  :EXPORT_FILE_NAME "projects/majutsu"
                  :DESCRIPTION "Jujutsu porcelain for Emacs."
                  :LOAM_KIND "project"
                  :LOAM_STATUS "active"
                  :LOAM_FEATURED "t"
                  :LOAM_ORDER "10"
                  :LOAM_PUBLISHED_AT "2026-08-08"
                  :tags {:type :anonymous :contents ["emacs" "jj"]}}
                 (fixtures/section
                  (fixtures/paragraph
                   "See "
                   (fixtures/link "custom-id" "wayland-ime" "the input note")
                   ".")))
        note (fixtures/headline
              1 "Wayland input methods"
              {:ID "22222222-2222-4222-8222-222222222222"
               :CUSTOM_ID "wayland-ime"
               :EXPORT_FILE_NAME "notes/wayland-input-methods"
               :DESCRIPTION "Notes about virtual keyboards and IMEs."
               :LOAM_KIND "note"
               :LOAM_STATUS "growing"
               :tags {:type :anonymous :contents ["wayland" "input"]}}
              (fixtures/section (fixtures/paragraph "Body.")))
        ast (fixtures/document "Personal" project note)
        result (compile/compile-documents
                [(fixtures/envelope-input "content/index.org" ast)]
                {:require-source-spans? false
                 :profile :loam/personal})
        artifacts (:artifacts result)
        pages (get-in artifacts [:manifest :pages])
        project-page (first pages)
        note-page (second pages)]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= "/projects/majutsu/" (:route project-page)))
    (is (= "/notes/wayland-input-methods/" (:route note-page)))
    (is (not (contains? project-page :version)))
    (is (= "project" (:kind project-page)))
    (is (= "active" (:status project-page)))
    (is (= ["emacs" "jj"] (:tags project-page)))
    (is (true? (:featured project-page)))
    (is (= 10 (:displayOrder project-page)))
    (is (= "2026-08-08" (:publishedAt project-page)))
    (is (= [(:id note-page)] (:outgoingLinks project-page)))
    (is (= [(:id project-page)] (:backlinks note-page)))
    (is (= [{:from (:id project-page) :to (:id note-page)}]
           (get-in artifacts [:graph :edges])))
    (is (= 2 (count (get-in artifacts [:search-index :pages]))))
    (is (contains? (:files artifacts) "search-index.json"))
    (is (contains? (:files artifacts) "graph.json"))))


(deftest personal-profile-supports-one-document-per-page
  (let [intro (fixtures/section
               {:type :keyword :properties {:key "TITLE" :value "Document post"} :contents []}
               {:type :keyword :properties {:key "DESCRIPTION" :value "One Org file is one page."} :contents []}
               {:type :keyword :properties {:key "FILETAGS" :value ":org:emacs:"} :contents []}
               (fixtures/paragraph "Intro."))
        section (fixtures/headline
                 1 "A real section"
                 {:CUSTOM_ID "real-section"}
                 (fixtures/section (fixtures/paragraph "Section body.")))
        ast {:type :org-data
             :properties {:ID "33333333-3333-4333-8333-333333333333"
                          :EXPORT_FILE_NAME "posts/document-post"
                          :LOAM_KIND "post"
                          :LOAM_STATUS "draft"
                          :LOAM_ORDER "7"}
             :contents [intro section]}
        result (compile/compile-documents
                [(fixtures/envelope-input "content/posts/document-post.org" ast)]
                {:require-source-spans? false
                 :profile :loam/personal})
        page (get-in result [:artifacts :manifest :pages 0])
        fragment (get-in result [:artifacts :fragments "pages/posts-document-post.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= 1 (count (get-in result [:artifacts :manifest :pages]))))
    (is (= "/posts/document-post/" (:route page)))
    (is (= "Document post" (:title page)))
    (is (= "One Org file is one page." (:description page)))
    (is (= "post" (:kind page)))
    (is (= "draft" (:status page)))
    (is (= ["org" "emacs"] (:tags page)))
    (is (= 7 (:displayOrder page)))
    (is (= [{:depth 2 :slug "real-section" :text "A real section"}]
           (:headings page)))
    (is (str/includes? fragment "Intro."))
    (is (str/includes? fragment "A real section"))))

(deftest personal-profile-requires-uuid-page-ids
  (let [ast (fixtures/document
             "Bad ID"
             (fixtures/headline
              1 "Bad ID"
              {:ID "not-a-uuid"
               :CUSTOM_ID "bad-id"
               :EXPORT_FILE_NAME "notes/bad-id"
               :DESCRIPTION "Invalid personal ID."}
              (fixtures/section (fixtures/paragraph "Body."))))
        result (compile/compile-documents
                [(fixtures/envelope-input "content/bad-id.org" ast)]
                {:require-source-spans? false
                 :profile :loam/personal})]
    (is (= :error (:status result)))
    (is (some #(= :invalid-personal-page-id (:code %)) (:diagnostics result)))
    (is (nil? (:artifacts result)))))
