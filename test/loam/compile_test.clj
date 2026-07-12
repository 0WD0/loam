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
                                          {:build {:vcs fixtures/build-vcs}})
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
    (is (= {:system "jj" :changeId "test-change" :commitId "test-commit"}
           (get-in manifest [:build :vcs])))
    (is (= "/docs/dev/hello/" (get-in manifest [:pages 0 :route])))
    (is (= 3 (get-in manifest [:pages 0 :source :startLine])))
    (is (= expected-fragment
           (get-in result [:artifacts :fragments "pages/hello.html"])))
    (is (str/includes? manifest-json "\"sources\":["))
    (is (not (str/includes? manifest-json "Hello, ox-edn.")))
    (is (not (str/includes? (pr-str (:artifacts result)) "/home/")))))

(deftest compiler-output-is-byte-deterministic
  (let [opts {:build {:vcs fixtures/build-vcs}}
        first-result (compile/compile-documents [(canonical-input)] opts)
        second-result (compile/compile-documents [(canonical-input)] opts)]
    (is (= :ok (:status first-result)))
    (is (= (get-in first-result [:artifacts :files])
           (get-in second-result [:artifacts :files])))
    (is (= (get-in first-result [:artifacts :manifest :build :contentHash])
           (get-in second-result [:artifacts :manifest :build :contentHash])))))

(deftest validates-envelope-schema-hash-diagnostics-and-build-metadata
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
        (let [result (compile/compile-documents [input] {:build {:vcs fixtures/build-vcs}})]
          (is (= :error (:status result)))
          (is (some #(= code (:code %)) (:diagnostics result)))
          (is (nil? (:artifacts result))))))
    (let [missing-vcs (compile/compile-documents [(canonical-input)] {})]
      (is (= :error (:status missing-vcs)))
      (is (some #(= :missing-build-vcs (:code %)) (:diagnostics missing-vcs))))))

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

(deftest deferred-disposition-is-visible-and-unreleasable
  (let [latex {:type :latex-fragment :properties {:value "$x$"} :contents []}
        ast (fixtures/document "Deferred"
                               (fixtures/page "Deferred" "deferred" "guide/deferred"
                                              (fixtures/section (fixtures/paragraph latex))))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/deferred.org" ast)]
                fixtures/compile-opts)
        fragment (get-in result [:artifacts :fragments "pages/guide-deferred.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (true? (get-in result [:artifacts :manifest :build :unreleasable])))
    (is (= "unreleasable" (get-in result [:artifacts :report :status])))
    (is (some #(= :deferred-node-type (:code %)) (:diagnostics result)))
    (is (str/includes? fragment "data-org-deferred=\"latex-fragment\""))
    (is (not (str/includes? fragment "$x$")))))

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
