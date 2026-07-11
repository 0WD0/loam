(ns loam.html-test
  (:require [clojure.test :refer [deftest is]]
            [loam.html :as html]))

(deftest escapes-text-and-attributes
  (is (= "&lt;a href=&quot;x&quot;&gt;&amp;&#39;"
         (html/escape-html "<a href=\"x\">&'")))
  (is (= "<a href=\"/?q=&amp;x=&lt;y&gt;\">link</a>"
         (html/render-html [:a {:href "/?q=&x=<y>"} "link"]))))

(deftest renders-tag-shorthand-and-void-tags
  (is (= "<div id=\"main\" class=\"page active\">ok</div>"
         (html/render-html [:div#main.page {:class "active"} "ok"])))
  (is (= "<input type=\"checkbox\" checked disabled>"
         (html/render-html [:input {:type "checkbox" :checked true :disabled true :hidden false}]))))

(deftest canonical-rendering-orders-attributes-without-changing-legacy-rendering
  (is (= "<article class=\"org-document\" data-page-id=\"p\" id=\"content\">x</article>"
         (html/render-canonical-html
          [:article {:id "content" :data-page-id "p" :class "org-document"} "x"])))
  (is (= (html/render-canonical-html
          [:div {:z "last" :a "first"} [:span {:y 2 :b 1} "x"]])
         (html/render-canonical-html
          [:div {:a "first" :z "last"} [:span {:b 1 :y 2} "x"]]))))
