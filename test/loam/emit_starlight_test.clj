(ns loam.emit-starlight-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [loam.compile :as compile]
            [loam.docs-fixtures :as fixtures]
            [loam.emit.starlight :as starlight]))

(defn temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "loam-starlight-test"
            (make-array java.nio.file.attribute.FileAttribute 0))))

(defn compiled-artifacts []
  (let [result (compile/compile-documents [(fixtures/sixteen-page-input)]
                                          fixtures/compile-opts)]
    (is (= :ok (:status result)) (:diagnostics result))
    (:artifacts result)))

(deftest atomically-replaces-clean-generations
  (let [root (temp-dir)
        output (io/file root "cache")
        artifacts (compiled-artifacts)
        first-summary (starlight/write-artifacts! output artifacts)
        first-manifest (slurp (io/file output "manifest.json"))]
    (is (= 16 (:pages first-summary)))
    (is (.isFile (io/file output "pages/guide-introduction.html")))
    (spit (io/file output "stale.html") "stale")
    (let [second-summary
          (starlight/write-artifacts!
           output
           {:manifest {:pages [] :build {:contentHash "empty"}}
            :unreleasable? false
            :files {"manifest.json" "{}\n"
                    "build-report.json" "{}\n"}})]
      (is (= 0 (:pages second-summary)))
      (is (= "{}\n" (slurp (io/file output "manifest.json"))))
      (is (not (.exists (io/file output "stale.html"))))
      (is (not (.exists (io/file output "pages/guide-introduction.html")))))
    (starlight/write-artifacts! output artifacts)
    (is (= first-manifest (slurp (io/file output "manifest.json"))))
    (is (empty? (filter #(re-find #"\.backup-|\.loam-" (.getName %))
                        (.listFiles root))))))

(deftest failed-generation-keeps-the-last-successful-output
  (let [root (temp-dir)
        output (io/file root "cache")
        artifacts {:manifest {:pages [] :build {:contentHash "old"}}
                   :files {"manifest.json" "old\n"}}]
    (starlight/write-artifacts! output artifacts)
    (try
      (starlight/write-artifacts!
       output
       {:manifest {:pages []}
        :files {"../escape" "bad"}})
      (is false "Expected unsafe artifact path failure")
      (catch clojure.lang.ExceptionInfo error
        (is (= :unsafe-artifact-path (:code (ex-data error))))))
    (is (= "old\n" (slurp (io/file output "manifest.json"))))
    (is (not (.exists (io/file root "escape"))))))

(deftest cli-compiles-real-envelope-input-as-a-clean-repeatable-generation
  (let [root (temp-dir)
        output (io/file root "docs-cache")
        opts {:repo-root "."
              :output-dir (.getPath output)
              :envelope-files ["test/fixtures/consumer-envelope-v1.edn"]
              :commit-id "test-commit"}
        first-summary (starlight/compile-cli! opts)
        first-manifest (slurp (io/file output "manifest.json"))]
    (is (= 1 (:pages first-summary)))
    (is (= "test-commit"
           (get-in first-summary [:result :artifacts :manifest :build :commitId])))
    (is (nil? (get-in first-summary [:result :artifacts :manifest :build :vcs])))
    (is (not (.contains first-manifest "changeId")))
    (is (.isFile (io/file output "pages/hello.html")))
    (spit (io/file output "stale.html") "stale")
    (let [second-summary (starlight/compile-cli! opts)]
      (is (= (:content-hash first-summary) (:content-hash second-summary)))
      (is (= first-manifest (slurp (io/file output "manifest.json"))))
      (is (not (.exists (io/file output "stale.html")))))))

(deftest cli-accepts-an-optional-commit-id
  (let [base-opts {:repo-root "."
                   :output-dir (.getPath (io/file (temp-dir) "docs-cache"))
                   :envelope-files ["test/fixtures/consumer-envelope-v1.edn"]}
        local-summary (starlight/compile-cli! base-opts)]
    (is (= 1 (:pages local-summary)))
    (is (not (contains? (get-in local-summary [:result :artifacts :manifest :build])
                        :commitId)))
    (is (= "source-commit"
           (:commit-id
            (starlight/parse-cli-args
             ["--repo-root" "."
              "--output-dir" "out"
              "--commit-id" "source-commit"
              "envelope.edn"]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unknown Loam docs compiler option"
                          (starlight/parse-cli-args
                           ["--vcs-commit-id" "legacy" "envelope.edn"])))))
