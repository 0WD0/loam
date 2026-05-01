(ns loam.theme.default
  "Default Loam static theme."
  (:require [loam.ast :as ast]
            [loam.index :as index]
            [loam.render :as render]
            [loam.route :as route]
            [clojure.string :as str]))

(def css
  "body{margin:0;font:16px/1.6 system-ui,-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif;color:#172033;background:#f8fafc}a{color:#2563eb;text-decoration:none}a:hover{text-decoration:underline}.site-shell{display:grid;grid-template-columns:18rem minmax(0,1fr);min-height:100vh}.site-nav{background:#0f172a;color:#e2e8f0;padding:1.25rem;overflow:auto}.site-nav a{color:#e2e8f0;display:block;padding:.2rem 0}.site-nav .site-title{font-weight:700;margin-bottom:1rem}.site-main{max-width:58rem;padding:2rem 3rem;background:white}.loam-document section{margin:1.25rem 0}.loam-document h1,.loam-document h2,.loam-document h3{line-height:1.25}.loam-document pre{overflow:auto;background:#0f172a;color:#e2e8f0;padding:1rem;border-radius:.5rem}.loam-document code{background:#e2e8f0;padding:.1rem .25rem;border-radius:.25rem}.loam-document pre code{background:transparent;padding:0}.loam-document blockquote{border-left:4px solid #cbd5e1;margin-left:0;padding-left:1rem;color:#475569}.loam-document table{border-collapse:collapse}.loam-document td,.loam-document th{border:1px solid #cbd5e1;padding:.25rem .5rem}.backlinks{margin-top:3rem;border-top:1px solid #e2e8f0;padding-top:1rem;color:#475569}.backlinks li{margin:.25rem 0}.page-list{columns:2;list-style:none;padding:0}.page-list li{break-inside:avoid;margin:.25rem 0}@media(max-width:800px){.site-shell{display:block}.site-nav{position:static}.site-main{padding:1.25rem}}")

(defn nav [ctx current-url]
  [:nav.site-nav
   [:div.site-title [:a {:href (route/asset-url current-url "/index.html")}
                     (or (:site-title ctx) "Loam")]]
   (for [page (sort-by :page-title (vals (get-in ctx [:index :pages])))]
     [:a {:href (:page-url page)
          :aria-current (when (= current-url (:page-url page)) "page")}
      (:page-title page)])])

(defn backlinks [ctx document]
  (let [idx (:index ctx)
        target-keys (->> (get-in idx [:sources (:source document)])
                         (mapcat (fn [entry]
                                   (concat
                                    (when-let [id (:id entry)] [[:id id]])
                                    (when-let [custom-id (:custom-id entry)] [[:custom-id custom-id]])
                                    (when-let [value (:value entry)] [[:target value]])
                                    (when-let [title (:title entry)] [[:title title]]))))
                         distinct)
        links (->> target-keys
                   (mapcat #(index/backlinks-for idx %))
                   distinct
                   (remove #(= (:source %) (:source document))))]
    (when (seq links)
      [:aside.backlinks
       [:h2 "Backlinks"]
       [:ul
        (for [link links]
          [:li [:a {:href (or (:heading-url link) (:source-url link))}
                (or (:heading-title link) (:source-title link))]
           " — " (:text link)])]])))

(defn page [ctx document]
  (let [page-title (:title document)
        page-url (:url document)
        render-ctx (assoc ctx
                          :document document
                          :resolve-link (index/link-resolver (:index ctx)))
        rendered (render/render-document render-ctx (:ast document))]
    [:html {:lang (or (:lang ctx) "en")}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:title page-title]
      [:link {:rel "stylesheet" :href (route/asset-url page-url "/assets/site.css")}]]
     [:body
      [:div.site-shell
       (nav ctx page-url)
       [:main.site-main
        rendered
        (backlinks ctx document)]]]]))

(defn home [ctx]
  [:html {:lang (or (:lang ctx) "en")}
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:title (or (:site-title ctx) "Loam")]
    [:link {:rel "stylesheet" :href "assets/site.css"}]]
   [:body
    [:main.site-main
     [:h1 (or (:site-title ctx) "Loam")]
     [:p "Generated from semantic EDN documents."]
     [:ul.page-list
      (for [page (sort-by :page-title (vals (get-in ctx [:index :pages])))]
        [:li [:a {:href (:page-url page)} (:page-title page)]])]]]])

(def extension
  {:id :loam.theme/default
   :layouts {:page page
             :home home}
   :assets {"assets/site.css" css}})
