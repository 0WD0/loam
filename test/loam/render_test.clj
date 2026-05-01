(ns loam.render-test
  (:require [clojure.test :refer [deftest is]]
            [loam.defaults :as defaults]
            [loam.render :as render]))

(deftest renders-basic-document
  (let [doc {:type :org-data
             :contents [{:type :headline
                         :properties {:level 1 :raw-value "Title" :ID "uuid-1"}
                         :contents [{:type :section
                                     :contents [{:type :paragraph
                                                 :contents ["Hello " {:type :bold :contents ["world"]}]}]}]}]}]
    (is (= [:div.loam-document
            [:section {:id "uuid-1"}
             [:h1 "Title"]
             [:p "Hello " [:strong "world"]]]]
           (render/render-document (defaults/create-system) doc)))))

(deftest allows-renderer-extension-override
  (let [ext {:id :test/render
             :extends {:renderers {:paragraph (fn [_ node]
                                                [[:p.custom (str "custom:" (count (:contents node)))]] )}}}
        system (defaults/create-system {:extensions [ext]})]
    (is (= [:div.loam-document [:p.custom "custom:1"]]
           (render/render-document system {:type :paragraph :contents ["x"]})))))

(deftest renders-links-with-custom-resolver
  (let [system (assoc (defaults/create-system)
                      :resolve-link (fn [props] (str "/nodes/" (:path props))))
        link {:type :link
              :properties {:type "id" :path "uuid-1"}
              :contents ["node"]}]
    (is (= [:div.loam-document [:a {:href "/nodes/uuid-1"} "node"]]
           (render/render-document system link)))))
