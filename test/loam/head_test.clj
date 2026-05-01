(ns loam.head-test
  (:require [clojure.test :refer [deftest is]]
            [loam.head :as head]))

(deftest normalizes-extension-head-entries
  (let [ctx {:site-title "Head Test"
             :head [[:meta {:name "a"}]
                    [[:meta {:name "b"}] [:meta {:name "c"}]]
                    (fn [_ current-url]
                      [(head/stylesheet current-url "/assets/x.css")
                       (head/script current-url "/assets/x.js" {:type "module"})])]}]
    (is (= [[:meta {:name "a"}]
            [:meta {:name "b"}]
            [:meta {:name "c"}]
            [:link {:rel "stylesheet" :href "../../assets/x.css"}]
            [:script {:src "../../assets/x.js" :type "module"}]]
           (vec (head/render-head ctx "/notes/a"))))))
