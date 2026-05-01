(ns loam.core
  "Composable system and extension registry for Loam."
  (:require [clojure.set :as set]))

(def empty-system
  {:extensions []
   :renderers {}
   :inline-types #{}
   :indexers []
   :layouts {}
   :assets {}
   :head []
   :hooks {}})

(defn- merge-hooks [a b]
  (merge-with into (or a {}) (or b {})))

(defn install-extension
  "Install EXTENSION into SYSTEM.

  Extensions are plain maps. Supported keys:
  - :id
  - :renderers
  - :inline-types
  - :indexers
  - :layouts
  - :assets
  - :head
  - :hooks"
  [system extension]
  (-> system
      (update :extensions conj extension)
      (update :renderers merge (:renderers extension))
      (update :inline-types set/union (or (:inline-types extension) #{}))
      (update :indexers into (:indexers extension))
      (update :layouts merge (:layouts extension))
      (update :assets merge (:assets extension))
      (update :head into (:head extension))
      (update :hooks merge-hooks (:hooks extension))))

(defn create-system
  "Create a Loam system from OPTS.

  OPTS may include regular system keys plus :extensions. Extensions are
  installed in order, so later extensions can override renderers and layouts."
  [{:keys [extensions] :as opts}]
  (let [base (merge empty-system (dissoc opts :extensions))]
    (reduce install-extension base extensions)))

(defn emit-hook
  "Run HOOK handlers as a reducing pipeline over EVENT."
  [system hook event]
  (reduce (fn [event f] (f event)) event (get-in system [:hooks hook] [])))
