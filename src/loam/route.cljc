(ns loam.route
  "URL and slug helpers."
  (:require [clojure.string :as str]))

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
     (str url "#" anchor)
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
