(ns loam.core-test
  (:require [clojure.test :refer [deftest is]]
            [loam.core :as loam]))

(deftest folds-extensions-through-service-targets
  (let [a {:id :a
           :extends {:renderers {:paragraph (fn [_ _] [:a])}
                     :inline-types #{:x}
                     :indexers [(fn [idx _] (assoc idx :a true))]
                     :assets {"a.css" "a"}
                     :head [[:meta {:name "a"}]]
                     :layouts {:page :a}
                     :hooks {:site/built [(fn [event] (assoc event :a true))]}}}
        b {:id :b
           :extends {:renderers {:paragraph (fn [_ _] [:b])}
                     :inline-types #{:y}
                     :assets {"b.css" "b"}
                     :head [[:meta {:name "b"}]]
                     :layouts {:page :b}
                     :hooks {:site/built [(fn [event] (assoc event :b true))]}}}
        system (loam/create-system {:extensions [a b]})]
    (is (= [:a :b] (map :id (:extensions system))))
    (is (= #{:x :y} (:inline-types system)))
    (is (= :b (get-in system [:layouts :page])))
    (is (= 1 (count (:indexers system))))
    (is (= #{"a.css" "b.css"} (set (keys (:assets system)))))
    (is (= [[:meta {:name "a"}] [:meta {:name "b"}]] (:head system)))
    (is (= {:a true :b true}
           (loam/emit-hook system :site/built {})))))

(deftest rejects-unknown-service-targets
  (try
    (loam/create-system {:extensions [{:id :bad
                                       :extends {:unknown []}}]})
    (is false "Expected unknown target error")
    (catch clojure.lang.ExceptionInfo error
      (is (= "Unknown Loam service target" (ex-message error)))
      (is (= {:extension :bad :targets [:unknown]} (ex-data error))))))

(deftest rejects-legacy-extension-shape
  (try
    (loam/create-system {:extensions [{:id :legacy
                                       :renderers {:paragraph :x}}]})
    (is false "Expected legacy shape error")
    (catch clojure.lang.ExceptionInfo error
      (is (= "Legacy Loam extension shape not supported" (ex-message error)))
      (is (= {:extension :legacy :keys [:renderers]} (ex-data error))))))

(deftest folds-docs-compiler-service-targets
  (let [extension {:id :compiler
                   :extends {:validators [:validate]
                             :document-transforms [:normalize]
                             :page-partitioners [:partition]
                             :diagnostic-rules [:diagnose]
                             :emitters [:emit]}}
        system (loam/create-system {:extensions [extension]})]
    (is (= [:validate] (:validators system)))
    (is (= [:normalize] (:document-transforms system)))
    (is (= [:partition] (:page-partitioners system)))
    (is (= [:diagnose] (:diagnostic-rules system)))
    (is (= [:emit] (:emitters system)))))
