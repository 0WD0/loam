(ns loam.emit.starlight
  "Filesystem boundary for Loam Manifest v1 and HTML fragments."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [loam.compile :as compile]
            [loam.route :as route])
  (:import [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files LinkOption OpenOption Path Paths
            StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.util UUID]))

(def no-link-options (make-array LinkOption 0))
(def no-file-attributes (make-array FileAttribute 0))

(defn- exists? [^Path path]
  (Files/exists path no-link-options))

(defn- delete-tree! [^Path root]
  (when (exists? root)
    (with-open [paths (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
      (doseq [path (reverse (vec (.toArray paths)))]
        (Files/deleteIfExists ^Path path)))))

(defn- force-file! [^Path path]
  (with-open [channel (FileChannel/open path
                                       (into-array OpenOption [StandardOpenOption/WRITE]))]
    (.force channel true)))

(defn- safe-output-file [^Path root relative]
  (when-not (route/safe-relative-path? relative)
    (throw (ex-info "Unsafe relative artifact path"
                    {:code :unsafe-artifact-path
                     :path relative
                     :problems (route/relative-path-problems relative)})))
  (let [target (.normalize (.resolve root relative))]
    (when-not (.startsWith target root)
      (throw (ex-info "Artifact path escapes output directory"
                      {:code :unsafe-artifact-path :path relative})))
    target))

(defn- write-generation! [^Path root files]
  (doseq [[relative content] (sort-by key files)]
    (let [target (safe-output-file root relative)
          parent (.getParent target)]
      (Files/createDirectories parent no-file-attributes)
      (Files/write target
                   (.getBytes (str content) StandardCharsets/UTF_8)
                   (into-array OpenOption [StandardOpenOption/CREATE_NEW
                                           StandardOpenOption/WRITE]))
      (force-file! target))))

(defn- move! [^Path from ^Path to]
  (try
    (Files/move from to
                (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
    (catch Exception _
      (Files/move from to (make-array CopyOption 0)))))

(defn- artifact-map [value]
  (cond
    (and (map? value) (:files value)) value
    (and (map? value) (get-in value [:artifacts :files])) (:artifacts value)
    :else (throw (ex-info "Starlight emitter requires successful compiler artifacts"
                          {:code :missing-compiler-artifacts}))))

(defn write-artifacts!
  "Atomically replace OUTPUT-DIR with one complete compiler generation.

  A fresh sibling directory is populated and forced before rename. The prior
  generation is restored if publication fails; removed pages cannot remain as
  stale files because generations are replaced rather than updated in place."
  [output-dir value]
  (let [artifacts (artifact-map value)
        output (.normalize (.toAbsolutePath (Paths/get (str output-dir) (make-array String 0))))
        parent (.getParent output)]
    (when (or (nil? parent) (= output (.getRoot output)))
      (throw (ex-info "Refusing to replace filesystem root"
                      {:code :unsafe-output-directory})))
    (Files/createDirectories parent no-file-attributes)
    (let [prefix (str "." (.getFileName output) ".loam-")
          temp (Files/createTempDirectory parent prefix no-file-attributes)
          backup (.resolve parent (str "." (.getFileName output) ".backup-" (UUID/randomUUID)))
          had-output? (exists? output)]
      (try
        (write-generation! temp (:files artifacts))
        (when had-output? (move! output backup))
        (try
          (move! temp output)
          (catch Exception publish-error
            (when (and had-output? (exists? backup) (not (exists? output)))
              (move! backup output))
            (throw publish-error)))
        (delete-tree! backup)
        {:output-dir (str output)
         :files (count (:files artifacts))
         :pages (count (get-in artifacts [:manifest :pages]))
         :content-hash (get-in artifacts [:manifest :build :contentHash])
         :unreleasable? (:unreleasable? artifacts)}
        (finally
          (delete-tree! temp)
          ;; A backup can only remain if restoration itself failed. Avoid
          ;; deleting that recovery generation in this exceptional case.
          (when (and (exists? backup) (exists? output))
            (delete-tree! backup)))))))

(defn- resolve-source-file [repo-root source]
  (when-not (route/safe-relative-path? source)
    (throw (ex-info "Envelope source path is unsafe"
                    {:code :unsafe-source-path :source source})))
  (let [root (.normalize (.toAbsolutePath (Paths/get (str repo-root) (make-array String 0))))
        source-file (.normalize (.resolve root source))]
    (when-not (.startsWith source-file root)
      (throw (ex-info "Envelope source escapes repo root"
                      {:code :unsafe-source-path :source source})))
    source-file))

(defn read-envelope-input
  "Read an envelope file and its repository-relative source for the pure
  compiler. Absolute local paths stay outside the returned/public model."
  [repo-root envelope-file]
  (let [envelope (edn/read-string (slurp (io/file envelope-file)))
        source (get-in envelope [:ox-edn/source :path])
        source-file (resolve-source-file repo-root source)]
    (when-not (Files/isRegularFile source-file no-link-options)
      (throw (ex-info "Envelope source file does not exist"
                      {:code :missing-source-file :source source})))
    {:envelope envelope
     :source-content (slurp (.toFile source-file))}))

(defn compile-to-directory!
  "Compile in-memory Envelope v1 inputs, then atomically publish artifacts."
  [inputs {:keys [output-dir] :as opts}]
  (when (str/blank? (str output-dir))
    (throw (ex-info "compile-to-directory! requires :output-dir"
                    {:code :missing-output-directory})))
  (let [result (compile/compile-documents! inputs (dissoc opts :output-dir))]
    (assoc (write-artifacts! output-dir result)
           :result result)))

(defn compile-envelope-files!
  "Read Envelope v1 files relative to explicit REPO-ROOT, compile, and atomically
  publish. Input paths are never copied into public artifacts."
  [repo-root envelope-files opts]
  (compile-to-directory! (mapv #(read-envelope-input repo-root %) envelope-files)
                         opts))

(def usage
  (str "Usage:\n"
       "  bb docs --repo-root DIR --output-dir DIR [options] ENVELOPE.edn...\n"
       "  npm run docs:compile -- --repo-root DIR --output-dir DIR ENVELOPE.edn...\n\n"
       "Required:\n"
       "  --repo-root DIR        Repository root used to resolve Envelope source paths\n"
       "  --output-dir DIR       Atomic Manifest v1/HTML fragment output directory\n"
       "  ENVELOPE.edn...        One or more ox-edn Envelope v1 files\n\n"
       "Optional:\n"
       "  --base PATH            Route base, default /docs\n"
       "  --version VERSION      Documentation version, default dev\n"
       "  --locale LOCALE        Optional locale route component\n"
       "  --vcs-change-id ID     Override the jj change ID (requires --vcs-commit-id)\n"
       "  --vcs-commit-id ID     Override the jj commit ID (requires --vcs-change-id)\n\n"
       "Without VCS overrides, Loam reads @ from the repo-root jj workspace."))

(def cli-option-keys
  {"--repo-root" :repo-root
   "--output-dir" :output-dir
   "--base" :base
   "--version" :version
   "--locale" :locale
   "--vcs-change-id" :vcs-change-id
   "--vcs-commit-id" :vcs-commit-id})

(defn parse-cli-args
  "Parse docs compiler flags and positional Envelope v1 files."
  [args]
  (loop [args (seq (remove #{"--"} args))
         opts {:envelope-files []}]
    (if-not args
      opts
      (let [argument (first args)]
        (if (str/starts-with? argument "--")
          (let [key (get cli-option-keys argument)
                value (second args)]
            (when-not key
              (throw (ex-info "Unknown Loam docs compiler option"
                              {:argument argument :usage usage})))
            (when (or (nil? value) (str/starts-with? value "--"))
              (throw (ex-info "Missing Loam docs compiler option value"
                              {:argument argument :usage usage})))
            (recur (nnext args) (assoc opts key value)))
          (recur (next args) (update opts :envelope-files conj argument)))))))

(defn- validate-cli-opts! [opts]
  (let [missing (vec (remove #(not (str/blank? (str (get opts %))))
                             [:repo-root :output-dir]))
        provided-vcs (filter #(not (str/blank? (str (get opts %))))
                             [:vcs-change-id :vcs-commit-id])]
    (when (seq missing)
      (throw (ex-info "Missing required Loam docs compiler option"
                      {:missing missing :usage usage})))
    (when (empty? (:envelope-files opts))
      (throw (ex-info "At least one Envelope v1 file is required"
                      {:missing [:envelope-files] :usage usage})))
    (when (= 1 (count provided-vcs))
      (throw (ex-info "VCS change and commit overrides must be supplied together"
                      {:missing (vec (remove (set provided-vcs)
                                             [:vcs-change-id :vcs-commit-id]))
                       :usage usage})))
    opts))

(defn- jj-vcs [repo-root]
  (let [{:keys [exit out err]}
        (shell/sh "jj" "-R" (str repo-root)
                  "log" "-r" "@" "--no-graph"
                  "-T" "change_id ++ \"\\n\" ++ commit_id ++ \"\\n\"")
        [change-id commit-id & extra]
        (remove str/blank? (str/split-lines out))]
    (when (or (not (zero? exit)) (str/blank? change-id)
              (str/blank? commit-id) (seq extra))
      (throw (ex-info "Could not read jj build identity from repo root"
                      {:code :missing-build-vcs
                       :repo-root (str repo-root)
                       :exit exit
                       :stderr (str/trim err)})))
    {:system "jj" :changeId change-id :commitId commit-id}))

(defn- cli-vcs [opts]
  (if (:vcs-change-id opts)
    {:system "jj"
     :changeId (:vcs-change-id opts)
     :commitId (:vcs-commit-id opts)}
    (jj-vcs (:repo-root opts))))

(defn compile-cli!
  "Compile CLI-style OPTS into one atomic Manifest v1 generation."
  [opts]
  (let [opts (validate-cli-opts! opts)
        compile-opts (cond-> {:output-dir (:output-dir opts)
                              :build {:vcs (cli-vcs opts)}}
                       (:base opts) (assoc :base (:base opts))
                       (:version opts) (assoc :version (:version opts))
                       (:locale opts) (assoc :locale (:locale opts)))]
    (compile-envelope-files! (:repo-root opts) (:envelope-files opts) compile-opts)))

(defn- print-cli-error! [error]
  (binding [*out* *err*]
    (println (ex-message error))
    (let [data (ex-data error)]
      (when-let [missing (:missing data)]
        (println "Missing:" (str/join ", " (map name missing))))
      (when-let [argument (:argument data)]
        (println "Argument:" argument))
      (when-let [diagnostics (:diagnostics data)]
        (doseq [{:keys [severity code message source-span]} diagnostics]
          (println (str (name severity) " " (name code) ": " message
                        (when-let [path (:path source-span)] (str " (" path ")"))))))
      (when-let [details (:stderr data)]
        (when-not (str/blank? details) (println details)))
      (when-let [text (:usage data)]
        (println)
        (println text)))))

(defn -main [& args]
  (try
    (if (some #{"--help" "-h"} args)
      (println usage)
      (try
        (let [summary (compile-cli! (parse-cli-args args))]
          (println "Compiled docs"
                   (select-keys summary [:output-dir :files :pages :content-hash
                                         :unreleasable?])))
        (catch clojure.lang.ExceptionInfo error
          (print-cli-error! error)
          (System/exit 1))))
    (finally
      ;; `jj-vcs` uses clojure.java.shell/sh, whose stream readers run on the
      ;; non-daemon agent pool. A command-line entry point owns that pool's
      ;; lifecycle and must stop it after the final result is printed.
      (shutdown-agents))))
