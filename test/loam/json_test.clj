(ns loam.json-test
  (:require [clojure.test :refer [deftest is]]
            [loam.json :as json]))

(deftest renders-json-primitives-and-containers
  (is (= "{\"a\":1,\"b\":[true,false,null],\"c\":\"x\\ny\"}"
         (json/render-json {:a 1 :b [true false nil] :c "x\ny"}))))
