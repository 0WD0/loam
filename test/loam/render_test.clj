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

(deftest renders-checkbox-as-part-of-the-item-first-paragraph
  (let [doc {:type :plain-list
             :properties {:type :unordered}
             :contents [{:type :item
                         :properties {:checkbox :on}
                         :contents [{:type :paragraph
                                     :contents ["same line"]}]}]}]
    (is (= [:div.loam-document
            [:ul
             [:li
              [:p [:input {:type "checkbox" :checked true :disabled true}]
               "same line"]]]]
           (render/render-document (defaults/create-system) doc)))))

(deftest renders-description-list-term-with-inline-org-semantics
  (let [doc {:type :plain-list
             :properties {:type :descriptive}
             :contents [{:type :item
                         :properties {:tag {:type :anonymous
                                            :properties {}
                                            :contents [{:type :verbatim
                                                        :properties {:value "CUSTOM_ID"}
                                                        :contents []}]}}
                         :contents [{:type :paragraph :contents ["Human-readable anchor."]}]}]}]
    (is (= [:div.loam-document
            [:dl
             [:dt [:code.verbatim "CUSTOM_ID"]]
             [:dd [:p "Human-readable anchor."]]]]
           (render/render-document (defaults/create-system) doc)))))

(deftest renders-code-and-verbatim-from-org-value-properties
  (is (= [:div.loam-document [:code "value"]]
         (render/render-document (defaults/create-system)
                                 {:type :code :properties {:value "value"} :contents []})))
  (is (= [:div.loam-document [:code.verbatim "raw_value"]]
         (render/render-document (defaults/create-system)
                                 {:type :verbatim :properties {:value "raw_value"} :contents []}))))

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
