(ns loam.test-runner
  (:require [clojure.test :as test]
            [loam.anchor-test]
            [loam.core-test]
            [loam.graph-test]
            [loam.head-test]
            [loam.html-test]
            [loam.index-test]
            [loam.json-test]
            [loam.render-test]
            [loam.site-test]
            [loam.treesitter-wasm-test]))

(defn -main [& _]
  (let [result (test/run-tests 'loam.anchor-test
                               'loam.core-test
                               'loam.graph-test
                               'loam.head-test
                               'loam.html-test
                               'loam.index-test
                               'loam.json-test
                               'loam.render-test
                               'loam.site-test
                               'loam.treesitter-wasm-test)]
    (when (pos? (+ (:fail result) (:error result)))
      (System/exit 1))))
