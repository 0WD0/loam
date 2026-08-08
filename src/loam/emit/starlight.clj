(ns loam.emit.starlight
  "Compatibility facade for the former Starlight-specific bundle emitter.

  New consumers should depend on `loam.emit.bundle`. This namespace keeps the
  docs CLI and public function names stable while selecting the docs profile."
  (:require [clojure.string :as str]
            [loam.emit.bundle :as bundle]))

(def write-artifacts! bundle/write-artifacts!)
(def read-envelope-input bundle/read-envelope-input)
(def compile-to-directory! bundle/compile-to-directory!)
(def compile-envelope-files! bundle/compile-envelope-files!)

(def usage
  (str "Usage:\n"
       "  bb docs --repo-root DIR --output-dir DIR [options] ENVELOPE.edn...\n\n"
       "Required:\n"
       "  --repo-root DIR        Repository root used to resolve Envelope source paths\n"
       "  --output-dir DIR       Atomic Manifest v1/HTML fragment output directory\n"
       "  ENVELOPE.edn...        One or more ox-edn Envelope v1 files\n\n"
       "Optional:\n"
       "  --commit-id ID         Source commit represented by a release build\n"
       "  --base PATH            Route base, default /docs\n"
       "  --version VERSION      Documentation version, default dev\n"
       "  --locale LOCALE        Optional locale route component\n"))

(def cli-option-keys
  {"--repo-root" :repo-root
   "--output-dir" :output-dir
   "--base" :base
   "--version" :version
   "--locale" :locale
   "--commit-id" :commit-id})

(defn parse-cli-args
  "Parse the legacy docs compiler flags and positional Envelope v1 files."
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

(defn compile-cli!
  "Compile using the legacy docs profile through the generic bundle emitter."
  [opts]
  (bundle/compile-cli! (assoc opts :profile "docs")))

(defn -main [& args]
  (if (some #{"--help" "-h"} args)
    (println usage)
    (try
      (let [summary (compile-cli! (parse-cli-args args))]
        (println "Compiled docs"
                 (select-keys summary [:output-dir :files :pages :content-hash
                                       :unreleasable?])))
      (catch clojure.lang.ExceptionInfo error
        (binding [*out* *err*]
          (println (ex-message error))
          (when-let [text (:usage (ex-data error))]
            (println)
            (println text)))
        (System/exit 1)))))
