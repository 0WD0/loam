(ns loam.test-runner
  (:require [clojure.test :as test]
            [loam.anchor-test]
            [loam.core-test]
            [loam.compile-test]
            [loam.defaults-test]
            [loam.directory-test]
            [loam.docs-index-link-test]
            [loam.docs-model-test]
            [loam.emit-starlight-test]
            [loam.graph-test]
            [loam.head-test]
            [loam.html-test]
            [loam.index-test]
            [loam.json-test]
            [loam.reference-index-test]
            [loam.render-test]
            [loam.route-test]
            [loam.shiki-test]
            [loam.site-test]
            [loam.svg-test]
            [loam.toc-test]
            [loam.treesitter-wasm-test]))

(defn -main [& _]
  (let [result (test/run-tests 'loam.anchor-test
                               'loam.compile-test
                               'loam.core-test
                               'loam.defaults-test
                               'loam.directory-test
                               'loam.docs-index-link-test
                               'loam.docs-model-test
                               'loam.emit-starlight-test
                               'loam.graph-test
                               'loam.head-test
                               'loam.html-test
                               'loam.index-test
                               'loam.json-test
                               'loam.reference-index-test
                               'loam.render-test
                               'loam.route-test
                               'loam.shiki-test
                               'loam.site-test
                               'loam.svg-test
                               'loam.toc-test
                               'loam.treesitter-wasm-test)]
    (when (pos? (+ (:fail result) (:error result)))
      (System/exit 1))))
