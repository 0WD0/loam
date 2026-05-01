(ns loam.core
  "Composable service targets and extension folding for Loam."
  (:require [clojure.set :as set]))

(defn- merge-hooks [a b]
  (merge-with into (or a {}) (or b {})))

(defn- compose-map [values]
  (reduce merge {} values))

(defn- compose-set [values]
  (reduce set/union #{} values))

(defn- compose-vector [values]
  (reduce into [] values))

(defn- compose-hooks [values]
  (reduce merge-hooks {} values))

(defn service-type
  "Create a service target description.

  :default is the target's base value.
  :compose combines extension contributions.
  :extend combines base value and composed contributions."
  [{:keys [default compose extend]}]
  {:default default
   :compose compose
   :extend extend})

(def default-service-types
  {:renderers (service-type {:default {}
                             :compose compose-map
                             :extend merge})
   :inline-types (service-type {:default #{}
                                :compose compose-set
                                :extend set/union})
   :indexers (service-type {:default []
                            :compose compose-vector
                            :extend into})
   :layouts (service-type {:default {}
                           :compose compose-map
                           :extend merge})
   :assets (service-type {:default {}
                          :compose compose-map
                          :extend merge})
   :head (service-type {:default []
                        :compose compose-vector
                        :extend into})
   :hooks (service-type {:default {}
                         :compose compose-hooks
                         :extend merge-hooks})})

(defn extension
  "Create an extension that contributes VALUES to service targets."
  [id extends]
  {:id id
   :extends extends})

(defn- target-defaults [service-types]
  (into {} (map (fn [[target {:keys [default]}]] [target default]) service-types)))

(def legacy-extension-keys
  #{:renderers
    :inline-types
    :indexers
    :layouts
    :assets
    :head
    :hooks})

(defn- legacy-extension-shape [extension]
  (seq (filter #(contains? legacy-extension-keys %) (keys extension))))

(defn- unknown-targets [service-types extension]
  (seq (remove #(contains? service-types %) (keys (:extends extension)))))

(defn- assert-known-targets! [service-types extension]
  (when-let [keys (legacy-extension-shape extension)]
    (throw (ex-info "Legacy Loam extension shape not supported"
                    {:extension (:id extension)
                     :keys (vec keys)})))
  (when-let [targets (unknown-targets service-types extension)]
    (throw (ex-info "Unknown Loam service target"
                    {:extension (:id extension)
                     :targets (vec targets)}))))

(defn- target-contributions [extensions target]
  (keep #(get-in % [:extends target]) extensions))

(defn- fold-target [extensions system [target {:keys [default compose extend]}]]
  (let [base (get system target default)
        contribution (compose (target-contributions extensions target))]
    (assoc system target (extend base contribution))))

(defn create-system
  "Create a Loam system from OPTS.

  Extensions contribute to explicit service targets under :extends. Each target
  controls how contributions compose and extend the final system value."
  [{:keys [extensions service-types] :as opts}]
  (let [service-types (merge default-service-types service-types)
        extensions (vec extensions)
        base (merge (target-defaults service-types)
                    (dissoc opts :extensions :service-types)
                    {:extensions extensions})]
    (doseq [extension extensions]
      (assert-known-targets! service-types extension))
    (reduce (partial fold-target extensions) base service-types)))

(defn emit-hook
  "Run HOOK handlers as a reducing pipeline over EVENT."
  [system hook event]
  (reduce (fn [event f] (f event)) event (get-in system [:hooks hook] [])))
