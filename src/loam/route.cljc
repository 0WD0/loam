(ns loam.route
  "URL and slug helpers."
  (:require [clojure.string :as str]
            [loam.anchor :as anchor]))

(defn slugify [s]
  (some-> s
          str
          str/trim
          (str/replace #"\s+" "-")
          (str/replace #"[^A-Za-z0-9._~:/?#\[\]@!$&'()*+,;=%-]" "")))

(defn fragment [s]
  (some-> s slugify not-empty))

(defn href
  ([url] url)
  ([url anchor]
   (if-let [anchor (fragment anchor)]
     (anchor/with-fragment url anchor)
     url)))

(defn clean-url [url]
  (let [url (or url "/")]
    (if (str/starts-with? url "/") url (str "/" url))))

(defn relative-root [url]
  (let [url (clean-url url)
        depth (count (remove str/blank? (str/split url #"/")))]
    (if (zero? depth) "." (str/join "/" (repeat depth "..")))))

(defn asset-url [page-url asset-path]
  (if (str/starts-with? asset-path "/")
    (str (relative-root page-url) asset-path)
    asset-path))

(defn url->path [output-dir url]
  (let [url (clean-url url)
        path (cond
               (= url "/") "index.html"
               (str/ends-with? url "/") (str (subs url 1) "index.html")
               (str/ends-with? url ".html") (subs url 1)
               :else (str (subs url 1) "/index.html"))]
    (str output-dir "/" path)))

(def unsafe-percent-escape
  #"(?i)%(?:2e|2f|5c|3f|23|00)")

(defn relative-path-problems
  "Return machine-readable reasons why PATH is not a safe repository-relative
  POSIX path. An empty vector means PATH is safe."
  [path]
  (let [path (when (string? path) path)
        segments (when path (str/split path #"/" -1))]
    (cond-> []
      (not (string? path)) (conj :not-a-string)
      (and path (str/blank? path)) (conj :blank)
      (and path (str/starts-with? path "/")) (conj :absolute)
      (and path (re-find #"^[A-Za-z]:" path)) (conj :absolute)
      (and path (str/includes? path "\\")) (conj :backslash)
      (and path (or (str/includes? path "?")
                    (str/includes? path "#"))) (conj :query-or-fragment)
      (and path (re-find unsafe-percent-escape path)) (conj :unsafe-encoding)
      (and path (some str/blank? segments)) (conj :empty-segment)
      (and path (some #{"." ".."} segments)) (conj :dot-segment)
      (and path (re-find #"[\u0000-\u001f\u007f]" path)) (conj :control-character))))

(defn safe-relative-path?
  "True when PATH is a non-empty, repository-relative POSIX path."
  [path]
  (empty? (relative-path-problems path)))

(defn page-path-problems
  "Validate an extensionless logical page path."
  [path]
  (cond-> (relative-path-problems path)
    (and (string? path) (str/starts-with? path "/")) (conj :leading-slash)
    (and (string? path) (str/ends-with? path "/")) (conj :trailing-slash)
    (and (string? path) (re-find #"\s" path)) (conj :whitespace)
    (and (string? path) (str/includes? path ":")) (conj :scheme-or-colon)))

(defn safe-page-path?
  [path]
  (empty? (page-path-problems path)))

(defn normalized-route-key
  "Collision key for routes. Routes are case-insensitive for collision checks."
  [value]
  (some-> value str/lower-case))

(defn site-route
  "Build a canonical trailing-slash site route from BASE and PAGE-PATH.

  BASE may be `/` for a root-level personal site. PAGE-PATH remains an
  extensionless safe logical path."
  [{:keys [base page-path]
    :or {base "/"}}]
  (let [base-path (str/replace (or base "") #"^/+|/+$" "")
        invalid (cond-> {}
                  (and (not (str/blank? base-path))
                       (seq (page-path-problems base-path)))
                  (assoc :base (page-path-problems base-path))
                  (seq (page-path-problems page-path))
                  (assoc :page-path (page-path-problems page-path)))]
    (when (seq invalid)
      (throw (ex-info "Unsafe Loam site route"
                      {:code :unsafe-route
                       :fields invalid})))
    (str "/"
         (str/join "/" (remove str/blank? [base-path page-path]))
         "/")))

(defn docs-route
  "Build a canonical trailing-slash docs route from explicit profile fields.

  BASE defaults to `/docs`. VERSION and PAGE-PATH are required safe path
  components; LOCALE is optional. Invalid values throw with structured data."
  [{:keys [base version locale page-path]
    :or {base "/docs"}}]
  (let [base-path (str/replace (or base "") #"^/+|/+$" "")
        fields (cond-> [[:base base-path] [:version version] [:page-path page-path]]
                 locale (conj [:locale locale]))
        invalid (into {}
                      (keep (fn [[field value]]
                              (let [problems (page-path-problems value)]
                                (when (seq problems) [field problems]))))
                      fields)]
    (when (seq invalid)
      (throw (ex-info "Unsafe Loam docs route"
                      {:code :unsafe-route
                       :fields invalid})))
    (str "/" (str/join "/" (cond-> [base-path]
                               locale (conj locale)
                               true (conj version page-path))) "/")))
