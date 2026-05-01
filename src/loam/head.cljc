(ns loam.head
  "Utilities for Loam extension-provided <head> content."
  (:require [loam.route :as route]))

(defn asset-url
  "Resolve HREF relative to CURRENT-URL using Loam's static asset rules."
  [current-url href]
  (route/asset-url current-url href))

(defn stylesheet
  "Return a stylesheet link tag for HREF, resolved for CURRENT-URL."
  [current-url href]
  [:link {:rel "stylesheet"
          :href (asset-url current-url href)}])

(defn script
  "Return a script tag for SRC, resolved for CURRENT-URL.

  OPTS are merged into the script attrs."
  ([current-url src]
   (script current-url src {}))
  ([current-url src opts]
   [:script (merge {:src (asset-url current-url src)} opts)]))

(defn normalize-entry
  "Normalize one extension head ENTRY into a sequence of Hiccup nodes.

  ENTRY may be:
  - a function of `[ctx current-url]`
  - a single Hiccup vector
  - a sequential collection of Hiccup vectors
  - nil"
  [ctx current-url entry]
  (cond
    (nil? entry) []
    (fn? entry) (normalize-entry ctx current-url (entry ctx current-url))
    (and (vector? entry) (keyword? (first entry))) [entry]
    (sequential? entry) (mapcat #(normalize-entry ctx current-url %) entry)
    :else []))

(defn render-head
  "Render all `:head` entries from CTX for CURRENT-URL."
  [ctx current-url]
  (mapcat #(normalize-entry ctx current-url %) (:head ctx)))
