(ns loam.docs-fixtures
  (:require [clojure.edn :as edn]
            [loam.compile :as compile]))

(def build-vcs
  {:system "jj"
   :changeId "test-change"
   :commitId "test-commit"})

(def compile-opts
  {:require-source-spans? false
   :build {:vcs build-vcs}})

(defn paragraph [& contents]
  {:type :paragraph :properties {} :contents (vec contents)})

(defn section [& contents]
  {:type :section :properties {} :contents (vec contents)})

(defn link
  ([type path label]
   (link type path label {}))
  ([type path label properties]
   {:type :link
    :properties (merge {:type type :path path :raw-link (str type ":" path)} properties)
    :contents [label]}))

(defn target [value]
  {:type :target :properties {:value value} :contents []})

(defn radio-target [value]
  {:type :radio-target :properties {:value value} :contents []})

(defn headline
  [level title properties & contents]
  {:type :headline
   :properties (merge {:level level
                       :raw-value title
                       :title {:type :anonymous :contents [title]}}
                      properties)
   :contents (vec contents)})

(defn page
  [title custom-id path & contents]
  (apply headline 1 title
         {:CUSTOM_ID custom-id
          :EXPORT_FILE_NAME path
          :DESCRIPTION (str title " description")}
         contents))

(defn document [title & contents]
  {:type :org-data
   :properties {:TITLE title}
   :contents (vec contents)})

(defn envelope-input
  ([path ast] (envelope-input path ast (str "fixture:" path "\n")))
  ([path ast source-content]
   {:source-content source-content
    :envelope
    {:ox-edn/schema-version 1
     :ox-edn/exporter {:name "ox-edn"
                       :version "0.3.0"
                       :emacs-version "29.1"
                       :org-version "9.6.15"}
     :ox-edn/source {:path path
                     :sha256 (compile/sha256 source-content)
                     :encoding "utf-8"}
     :ox-edn/document ast
     :ox-edn/diagnostics []}}))

(defn page-root-snapshot []
  (edn/read-string (slurp "test/fixtures/majutsu/page-roots-v1.edn")))

(defn sixteen-page-input []
  (let [roots (page-root-snapshot)
        ast (apply document "Majutsu"
                   (map (fn [{:keys [title custom-id export-file-name]}]
                          (page title custom-id export-file-name
                                (section (paragraph (str title " body")))))
                        roots))]
    (envelope-input "docs/majutsu.org" ast)))
