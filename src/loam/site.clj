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
            [loam.json :as json]
            [loam.route :as route]))

(def usage
  (str "Usage:\n"
       "  bb build --edn-dir build/edn --output-dir build/site\n"
       "  bb build --config loam.edn\n"
       "  npm run build -- --edn-dir build/edn --output-dir build/site\n\n"
       "Required:\n"
       "  --edn-dir DIR      EDN document directory\n"
       "  --output-dir DIR   Static site output directory\n\n"
       "Optional:\n"
       "  --config FILE     EDN build config file; CLI args override file values\n"
       "  --url-prefix URL   Document URL prefix, default /notes/\n"
       "  --public-dir DIR   Static asset directory, default public\n"
       "  --site-title TEXT  Default theme title"))

(defn edn-files [dir]
  (let [dir-file (io/file dir)]
    (when-not (and dir (.exists dir-file) (.isDirectory dir-file))
      (throw (ex-info "EDN directory does not exist or is not a directory"
                      {:edn-dir dir})))
    (->> (file-seq dir-file)
         (filter #(.isFile %))
         (filter #(str/ends-with? (.getName %) ".edn"))
         (sort-by #(.getPath %)))))

(defn relative-path [root file]
  (-> (.toPath (io/file root))
      (.relativize (.toPath file))
      str))

(defn parent-dir [path]
  (let [parent (.getParent (io/file path))]
    (if (or (nil? parent) (str/blank? parent))
      "notes"
      (str/replace parent "\\" "/"))))

(defn read-document [edn-dir file]
  (let [source-rel (relative-path edn-dir file)]
    {:source (.getPath file)
     :source-rel source-rel
     :source-dir (parent-dir source-rel)
     :ast (edn/read-string (slurp file))}))

(defn ensure-dir! [path]
  (.mkdirs (io/file path)))

(defn write-file! [path content]
  (ensure-dir! (.getParentFile (io/file path)))
  (spit path content))

(defn copy-file! [from to]
  (ensure-dir! (.getParentFile (io/file to)))
  (io/copy (io/file from) (io/file to)))

(defn copy-public-dir! [public-dir output-dir]
  (let [public-dir (io/file public-dir)]
    (when (.exists public-dir)
      (doseq [file (file-seq public-dir)
              :when (.isFile file)]
        (let [rel (relative-path public-dir file)]
          (copy-file! file (.getPath (io/file output-dir rel))))))))

(defn public-index [idx]
  (select-keys idx [:pages :directories :toc :ids :custom-ids :titles :targets :links :backlinks :search/documents :graph]))

(defn search-index [idx]
  {:documents (:search/documents idx)})

(defn write-assets! [output-dir assets]
  (doseq [[path content] assets]
    (write-file! (.getPath (io/file output-dir path)) content)))

(defn write-index! [output-dir idx]
  (write-file! (.getPath (io/file output-dir "index.edn"))
               (with-out-str (pprint/pprint (public-index idx))))
  (write-file! (.getPath (io/file output-dir "search-index.json"))
               (json/render-json (search-index idx)))
  (write-file! (.getPath (io/file output-dir "graph.edn"))
               (with-out-str (pprint/pprint (:graph idx))))
  (write-file! (.getPath (io/file output-dir "graph.json"))
               (json/render-json (:graph idx))))

(def config-path-keys #{:edn-dir :output-dir :public-dir})

(defn relative-path-string? [path]
  (and (string? path)
       (not (.isAbsolute (io/file path)))))

(defn resolve-config-paths [config-file opts]
  (let [base-dir (or (.getParentFile (io/file config-file))
                     (io/file "."))]
    (reduce-kv (fn [resolved k v]
                 (assoc resolved k
                        (if (and (contains? config-path-keys k)
                                 (relative-path-string? v))
                          (.getPath (io/file base-dir v))
                          v)))
               {}
               opts)))

(defn read-config-file [config-file]
  (let [file (io/file config-file)]
    (when-not (.exists file)
      (throw (ex-info "Loam config file does not exist"
                      {:config-file config-file
                       :usage usage})))
    (when-not (.isFile file)
      (throw (ex-info "Loam config path is not a file"
                      {:config-file config-file
                       :usage usage})))
    (let [config (edn/read-string (slurp file))]
      (when-not (map? config)
        (throw (ex-info "Loam config file must contain an EDN map"
                        {:config-file config-file
                         :value config
                         :usage usage})))
      (resolve-config-paths config-file config))))

(defn validate-build-opts! [opts]
  (let [missing (vec (remove #(seq (str (get opts %))) [:edn-dir :output-dir]))]
    (when (seq missing)
      (throw (ex-info "Missing required Loam build option"
                      {:missing missing
                       :usage usage}))))
  opts)

(defn build-site!
  "Build a static site.

  Required opts:
  - :edn-dir
  - :output-dir

  Optional opts:
  - :url-prefix default /notes/
  - :site-title default Loam
  - :public-dir default public/
  - :highlight default :shiki
  - :extensions extension maps"
  [opts]
  (validate-build-opts! opts)
  (let [edn-dir (:edn-dir opts)
        output-dir (:output-dir opts)
        public-dir (or (:public-dir opts) "public")
        system (defaults/create-system (merge {:url-prefix "/notes/"} opts))
        files (edn-files edn-dir)
        documents (map #(read-document edn-dir %) files)
        idx (index/build-index system documents)
        ctx (assoc system :index idx)
        home-layout (get-in system [:layouts :home])
        page-layout (get-in system [:layouts :page])]
    (ensure-dir! output-dir)
    (copy-public-dir! public-dir output-dir)
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

(defn normalize-args [args]
  (remove #{"--"} args))

(defn parse-cli-args [args]
  (loop [args (seq (normalize-args args))
         opts {}]
    (if (empty? args)
      opts
      (let [[k v & rest] args]
        (when-not (and (string? k) (str/starts-with? k "--"))
          (throw (ex-info "Invalid Loam build argument"
                          {:argument k
                           :usage usage})))
        (when (or (nil? v) (str/starts-with? v "--"))
          (throw (ex-info "Missing value for Loam build argument"
                          {:argument k
                           :usage usage})))
        (recur rest (assoc opts (keyword (subs k 2)) v))))))

(defn parse-args [args]
  (let [cli-opts (parse-cli-args args)
        config-file (:config cli-opts)
        config-opts (if config-file
                      (read-config-file config-file)
                      {})]
    (merge config-opts (dissoc cli-opts :config))))

(defn help? [args]
  (some #{"--help" "-h"} (normalize-args args)))

(defn print-error! [error]
  (binding [*out* *err*]
    (println (ex-message error))
    (when-let [data (ex-data error)]
      (when-let [missing (:missing data)]
        (println "Missing:" (str/join ", " (map name missing))))
      (when-let [argument (:argument data)]
        (println "Argument:" argument))
      (when-let [edn-dir (:edn-dir data)]
        (println "EDN dir:" edn-dir))
      (when-let [config-file (:config-file data)]
        (println "Config file:" config-file))
      (when-let [usage (:usage data)]
        (println)
        (println usage)))))

(defn -main [& args]
  (if (help? args)
    (println usage)
    (try
      (let [summary (build-site! (parse-args args))]
        (println "Built site" (select-keys summary [:edn-files :pages :links :backlink-groups :output-dir])))
      (catch clojure.lang.ExceptionInfo error
        (print-error! error)
        (System/exit 1)))))
