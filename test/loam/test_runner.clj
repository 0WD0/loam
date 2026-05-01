(ns loam.test-runner
  (:require [clojure.test :as test]
            [loam.core-test]
            [loam.html-test]
            [loam.index-test]
            [loam.render-test]
            [loam.site-test]))

(defn -main [& _]
  (let [result (test/run-tests 'loam.core-test
                               'loam.html-test
                               'loam.index-test
                               'loam.render-test
                               'loam.site-test)]
    (when (pos? (+ (:fail result) (:error result)))
      (System/exit 1))))
