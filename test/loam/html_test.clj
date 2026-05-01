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
