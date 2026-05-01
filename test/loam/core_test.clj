(ns loam.core-test
  (:require [clojure.test :refer [deftest is]]
            [loam.core :as loam]))

(deftest installs-extensions-in-order
  (let [a {:id :a
           :renderers {:paragraph (fn [_ _] [:a])}
           :inline-types #{:x}
           :indexers [(fn [idx _] (assoc idx :a true))]
           :assets {"a.css" "a"}
           :asset-files {"a.js" "/tmp/a.js"}
           :head [[:meta {:name "a"}]]
           :layouts {:page :a}
           :hooks {:site/built [(fn [event] (assoc event :a true))]}}
        b {:id :b
           :renderers {:paragraph (fn [_ _] [:b])}
           :inline-types #{:y}
           :assets {"b.css" "b"}
           :asset-files {"b.js" "/tmp/b.js"}
           :head [[:meta {:name "b"}]]
           :layouts {:page :b}
           :hooks {:site/built [(fn [event] (assoc event :b true))]}}
        system (loam/create-system {:extensions [a b]})]
    (is (= [:a :b] (map :id (:extensions system))))
    (is (= #{:x :y} (:inline-types system)))
    (is (= :b (get-in system [:layouts :page])))
    (is (= #{"a.css" "b.css"} (set (keys (:assets system)))))
    (is (= {"a.js" "/tmp/a.js" "b.js" "/tmp/b.js"} (:asset-files system)))
    (is (= [[:meta {:name "a"}] [:meta {:name "b"}]] (:head system)))
    (is (= {:a true :b true}
           (loam/emit-hook system :site/built {})))))
