(ns loam.site-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [loam.fixtures :as fixtures]
            [loam.site :as site]))

(defn temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "loam-site-test"
            (make-array java.nio.file.attribute.FileAttribute 0))))

(defn write-edn! [dir name value]
  (let [file (io/file dir name)]
    (.mkdirs (.getParentFile file))
    (spit file (pr-str value))))

(deftest validates-required-build-options
  (try
    (site/build-site! {})
    (is false "Expected missing build option error")
    (catch clojure.lang.ExceptionInfo error
      (is (= "Missing required Loam build option" (ex-message error)))
      (is (= [:edn-dir :output-dir] (:missing (ex-data error)))))))

(deftest validates-edn-directory
  (try
    (site/build-site! {:edn-dir "/tmp/loam-missing-edn-dir"
                       :output-dir "/tmp/loam-missing-edn-out"})
    (is false "Expected missing EDN directory error")
    (catch clojure.lang.ExceptionInfo error
      (is (= "EDN directory does not exist or is not a directory" (ex-message error)))
      (is (= "/tmp/loam-missing-edn-dir" (:edn-dir (ex-data error)))))))

(deftest parses-bb-argument-separator
  (is (= {:edn-dir "build/edn"
          :output-dir "build/site"}
         (site/parse-args ["--" "--edn-dir" "build/edn" "--output-dir" "build/site"]))))

(deftest parses-config-file-and-cli-overrides
  (let [root (temp-dir)
        config-file (io/file root "loam.edn")]
    (spit config-file (pr-str {:edn-dir "edn"
                               :output-dir "site"
                               :public-dir "public"
                               :site-title "Configured Loam"
                               :highlight {:provider :shiki}}))
    (is (= {:edn-dir (.getPath (io/file root "edn"))
            :output-dir "override-site"
            :public-dir (.getPath (io/file root "public"))
            :site-title "Configured Loam"
            :highlight {:provider :shiki}}
           (site/parse-args ["--config" (.getPath config-file)
                             "--output-dir" "override-site"])))))

(deftest builds-static-site-from-edn-files
  (let [root (temp-dir)
        edn-dir (io/file root "edn")
        out-dir (io/file root "site")
        public-dir (io/file root "public")]
    (.mkdirs edn-dir)
    (.mkdirs (io/file public-dir "assets"))
    (spit (io/file public-dir "assets" "copied.txt") "copied asset")
    (write-edn! edn-dir "a.edn" fixtures/doc-a)
    (write-edn! edn-dir "daily/b.edn" fixtures/doc-b)
    (let [summary (site/build-site! {:edn-dir (.getPath edn-dir)
                                     :output-dir (.getPath out-dir)
                                     :public-dir (.getPath public-dir)
                                     :site-title "Test Loam"})
          home (slurp (io/file out-dir "index.html"))
          page-a (slurp (io/file out-dir "notes" "a" "index.html"))
          public-index (edn/read-string (slurp (io/file out-dir "index.edn")))
          graph (edn/read-string (slurp (io/file out-dir "graph.edn")))
          search-json (slurp (io/file out-dir "search-index.json"))]
      (is (= 2 (:pages summary)))
      (is (= 2 (:edn-files summary)))
      (is (.exists (io/file out-dir "assets" "site.css")))
      (is (.exists (io/file out-dir "assets" "search.js")))
      (is (= "copied asset" (slurp (io/file out-dir "assets" "copied.txt"))))
      (is (.exists (io/file out-dir "graph.json")))
      (is (str/includes? home "Test Loam"))
      (is (str/includes? home "data-loam-search"))
      (is (str/includes? home "daily <span"))
      (is (str/includes? page-a "href=\"/notes/b\""))
      (is (str/includes? page-a "page-toc"))
      (is (str/includes? page-a "href=\"/notes/a#heading-a\""))
      (is (str/includes? page-a "../../assets/site.css"))
      (is (str/includes? page-a "../../search-index.json"))
      (is (= "/notes/b" (get-in public-index [:ids "page-b" :href])))
      (is (= ["notes" "daily"] (map :path (:directories public-index))))
      (is (= "References" (get-in public-index [:toc (.getPath (io/file edn-dir "a.edn")) 0 :title])))
      (is (= 2 (count (:search/documents public-index))))
      (is (= 2 (count (:nodes graph))))
      (is (str/includes? search-json "Page A")))))
