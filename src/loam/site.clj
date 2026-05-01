(ns loam.site
  "Static site builder for Loam."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [loam.core :as core]
            [loam.defaults :as defaults]
            [loam.html :as html]
            [loam.index :as index]
            [loam.route :as route]))

(defn edn-files [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) ".edn"))
       (sort-by #(.getPath %))))

(defn read-document [file]
  {:source (.getPath file)
   :ast (edn/read-string (slurp file))})

(defn ensure-dir! [path]
  (.mkdirs (io/file path)))

(defn write-file! [path content]
  (ensure-dir! (.getParentFile (io/file path)))
  (spit path content))

(defn public-index [idx]
  (select-keys idx [:pages :ids :custom-ids :titles :targets :links :backlinks :search/documents]))

(defn write-assets! [output-dir assets]
  (doseq [[path content] assets]
    (write-file! (.getPath (io/file output-dir path)) content)))

(defn write-index! [output-dir idx]
  (write-file! (.getPath (io/file output-dir "index.edn"))
               (with-out-str (pprint/pprint (public-index idx)))))

(defn build-site!
  "Build a static site.

  Required opts:
  - :edn-dir
  - :output-dir

  Optional opts:
  - :url-prefix default /notes/
  - :site-title default Loam
  - :extensions extension maps"
  [opts]
  (let [edn-dir (:edn-dir opts)
        output-dir (:output-dir opts)
        system (defaults/create-system (merge {:url-prefix "/notes/"} opts))
        files (edn-files edn-dir)
        documents (map read-document files)
        idx (index/build-index system documents)
        ctx (assoc system :index idx)
        home-layout (get-in system [:layouts :home])
        page-layout (get-in system [:layouts :page])]
    (ensure-dir! output-dir)
    (write-assets! output-dir (:assets system))
    (write-file! (.getPath (io/file output-dir "index.html"))
                 (html/document (home-layout ctx)))
    (doseq [document (:documents idx)]
      (write-file! (route/url->path output-dir (:url document))
                   (html/document (page-layout ctx document))))
    (write-index! output-dir idx)
    (core/emit-hook system :site/built {:system system
                                        :index idx
                                        :output-dir output-dir
                                        :edn-files (count files)
                                        :pages (count (:documents idx))
                                        :links (count (:links idx))
                                        :backlink-groups (count (:backlinks idx))})))

(defn parse-args [args]
  (loop [args args
         opts {}]
    (if (empty? args)
      opts
      (let [[k v & rest] args]
        (recur rest (assoc opts (keyword (subs k 2)) v))))))

(defn -main [& args]
  (let [summary (build-site! (parse-args args))]
    (println "Built site" (select-keys summary [:edn-files :pages :links :backlink-groups :output-dir]))))
