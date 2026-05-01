(ns loam.defaults
  "Default Loam system composition."
  (:require [loam.core :as core]
            [loam.directory :as directory]
            [loam.graph :as graph]
            [loam.highlight.shiki :as shiki]
            [loam.highlight.treesitter.wasm :as treesitter.wasm]
            [loam.render :as render]
            [loam.search :as search]
            [loam.theme.default :as theme.default]))

(def default-highlight :treesitter)

(defn- normalize-provider [provider]
  (cond
    (keyword? provider) provider
    (string? provider) (keyword provider)
    :else provider))

(defn highlight-extension
  "Create a built-in highlight extension from PROVIDER.

  PROVIDER may be:
  - :treesitter, :tree-sitter, or :tree-sitter-wasm
  - :shiki
  - :none or false
  - a highlight extension map
  - a map with :provider plus provider options"
  [provider]
  (cond
    (nil? provider) (treesitter.wasm/extension)
    (false? provider) nil
    (and (map? provider) (:id provider)) provider
    (map? provider) (let [provider-name (normalize-provider (or (:provider provider)
                                                                (:type provider)))
                          opts (dissoc provider :provider :type)]
                      (case provider-name
                        :treesitter (treesitter.wasm/extension opts)
                        :tree-sitter (treesitter.wasm/extension opts)
                        :tree-sitter-wasm (treesitter.wasm/extension opts)
                        :shiki (shiki/extension opts)
                        :none nil
                        (throw (ex-info "Unknown Loam highlight provider"
                                        {:provider provider-name}))))
    :else (case (normalize-provider provider)
            :treesitter (treesitter.wasm/extension)
            :tree-sitter (treesitter.wasm/extension)
            :tree-sitter-wasm (treesitter.wasm/extension)
            :shiki (shiki/extension)
            :none nil
            (throw (ex-info "Unknown Loam highlight provider"
                            {:provider provider})))))

(defn- default-extensions-for [opts]
  (let [user-extensions (vec (:extensions opts))
        provider (if (contains? opts :highlight)
                   (:highlight opts)
                   default-highlight)
        highlight (highlight-extension provider)]
    (vec
     (concat [render/extension]
             (when highlight [highlight])
             [search/extension
              graph/extension
              directory/extension
              theme.default/extension]
             user-extensions))))

(def default-extensions
  (default-extensions-for {}))

(defn create-system
  ([] (create-system {}))
  ([opts]
   (core/create-system
    (assoc (dissoc opts :highlight)
           :extensions
           (default-extensions-for opts)))))
