(ns loam.theme.default
  "Default Loam static theme."
  (:require [loam.head :as head]
            [loam.index :as index]
            [loam.render :as render]
            [loam.route :as route]
            [clojure.string :as str]))

(def css
  (str
   ":root{--bg:#f8fafc;--panel:#0f172a;--panel-2:#111c33;--text:#172033;--muted:#64748b;--line:#e2e8f0;--link:#2563eb;--code:#e2e8f0}"
   "*{box-sizing:border-box}body{margin:0;font:16px/1.6 system-ui,-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif;color:var(--text);background:var(--bg)}a{color:var(--link);text-decoration:none}a:hover{text-decoration:underline}"
   ".site-shell{display:grid;grid-template-columns:20rem minmax(0,1fr);min-height:100vh}.site-nav{position:sticky;top:0;height:100vh;background:var(--panel);color:#e2e8f0;padding:1rem;overflow:auto}.site-nav a{color:#e2e8f0}.site-title{font-weight:800;font-size:1.25rem;margin-bottom:1rem}.site-title a{display:inline-block}.site-section{margin:.75rem 0}.site-section summary{cursor:pointer;color:#cbd5e1;font-weight:700;font-size:.85rem;text-transform:uppercase;letter-spacing:.04em}.site-section ol{list-style:none;margin:.35rem 0 .75rem 0;padding:0}.site-section li{margin:.15rem 0}.site-page-link{display:block;padding:.2rem .35rem;border-radius:.35rem;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}.site-page-link[aria-current=page],.site-page-link:hover{background:var(--panel-2);text-decoration:none}.site-count{color:#94a3b8;font-weight:400;text-transform:none}"
   ".search-box{margin:0 0 1rem 0}.search-box input{width:100%;border:1px solid #334155;border-radius:.45rem;background:#020617;color:#e2e8f0;padding:.55rem .65rem}.search-box input::placeholder{color:#94a3b8}.search-results{list-style:none;margin:.5rem 0 0 0;padding:0;max-height:18rem;overflow:auto}.search-results li{border-top:1px solid #1e293b;padding:.4rem 0}.search-results a{font-weight:700}.search-results small{display:block;color:#94a3b8;line-height:1.35}.search-empty{color:#94a3b8;font-size:.9rem}"
   ".site-main{max-width:64rem;padding:2rem 3rem;background:white}.home-main{max-width:72rem}.hero{border-bottom:1px solid var(--line);margin-bottom:1.5rem;padding-bottom:1rem}.hero h1{font-size:2.25rem;line-height:1.1;margin:.25rem 0}.stats{display:flex;flex-wrap:wrap;gap:.75rem;margin:1rem 0}.stat{border:1px solid var(--line);border-radius:.65rem;padding:.7rem .9rem;min-width:8rem;background:#f8fafc}.stat strong{display:block;font-size:1.4rem}.stat span{color:var(--muted);font-size:.9rem}.page-list{columns:2;list-style:none;padding:0}.page-list li{break-inside:avoid;margin:.25rem 0}"
   ".loam-document section{margin:1.25rem 0}.loam-document h1,.loam-document h2,.loam-document h3{line-height:1.25}.loam-document pre{overflow:auto;background:#0f172a;color:#e2e8f0;padding:1rem;border-radius:.5rem}.loam-document code{background:var(--code);padding:.1rem .25rem;border-radius:.25rem}.loam-document pre code{background:transparent;padding:0}.loam-document blockquote{border-left:4px solid #cbd5e1;margin-left:0;padding-left:1rem;color:#475569}.loam-document table{border-collapse:collapse}.loam-document td,.loam-document th{border:1px solid #cbd5e1;padding:.25rem .5rem}.loam-document img{max-width:100%}"
   ".backlinks{margin-top:3rem;border-top:1px solid var(--line);padding-top:1rem;color:#475569}.backlinks h2{margin-bottom:.25rem}.backlinks p{color:var(--muted);margin-top:0}.backlinks ul{padding-left:1.2rem}.backlinks li{margin:.35rem 0}.backlinks small{display:block;color:var(--muted)}.page-meta{color:var(--muted);font-size:.9rem;margin:-.5rem 0 1.5rem 0}.graph-link{display:inline-block;margin-left:.5rem}"
   "@media(max-width:900px){.site-shell{display:block}.site-nav{position:static;height:auto}.site-main{padding:1.25rem}.page-list{columns:1}}"))

(def search-js
  "(() => {\n  const script = document.currentScript;\n  const indexUrl = script?.dataset?.index || '/search-index.json';\n  const input = document.querySelector('[data-loam-search]');\n  const results = document.querySelector('[data-loam-search-results]');\n  if (!input || !results) return;\n  let docs = [];\n  const normalize = (s) => (s || '').toLowerCase();\n  const render = (items, q) => {\n    results.innerHTML = '';\n    if (!q) return;\n    if (!items.length) {\n      const li = document.createElement('li');\n      li.className = 'search-empty';\n      li.textContent = 'No results';\n      results.appendChild(li);\n      return;\n    }\n    for (const doc of items.slice(0, 12)) {\n      const li = document.createElement('li');\n      const a = document.createElement('a');\n      a.href = doc.url;\n      a.textContent = doc.title || doc.url;\n      const small = document.createElement('small');\n      const text = doc.text || '';\n      const i = normalize(text).indexOf(q);\n      small.textContent = i >= 0 ? text.slice(Math.max(0, i - 50), i + 120) : (doc.url || '');\n      li.appendChild(a);\n      li.appendChild(small);\n      results.appendChild(li);\n    }\n  };\n  fetch(indexUrl).then(r => r.json()).then(data => { docs = data.documents || []; }).catch(() => {});\n  input.addEventListener('input', () => {\n    const q = normalize(input.value).trim();\n    if (!q) { render([], q); return; }\n    const terms = q.split(/\\s+/).filter(Boolean);\n    const scored = [];\n    for (const doc of docs) {\n      const haystack = normalize(`${doc.title || ''} ${doc.url || ''} ${doc.text || ''}`);\n      if (terms.every(t => haystack.includes(t))) {\n        const title = normalize(doc.title || '');\n        const score = terms.reduce((n, t) => n + (title.includes(t) ? 10 : 1), 0);\n        scored.push([score, doc]);\n      }\n    }\n    scored.sort((a, b) => b[0] - a[0]);\n    render(scored.map(x => x[1]), q);\n  });\n})();\n")

(defn page-groups [ctx]
  (->> (vals (get-in ctx [:index :pages]))
       (group-by #(or (:source-dir %) "notes"))
       (map (fn [[group pages]]
              {:group group
               :pages (sort-by :page-title pages)}))
       (sort-by (fn [{:keys [group]}] [(if (= group "notes") 0 1) group]))))

(defn search-box [ctx current-url]
  [:div.search-box
   [:input {:type "search"
            :placeholder "Search notes..."
            :autocomplete "off"
            :data-loam-search true}]
   [:ol.search-results {:data-loam-search-results true}]])

(defn nav [ctx current-url]
  (let [groups (page-groups ctx)]
    [:nav.site-nav
     [:div.site-title [:a {:href (route/asset-url current-url "/index.html")}
                       (or (:site-title ctx) "Loam")]]
     (search-box ctx current-url)
     (for [{:keys [group pages]} groups]
       [:details.site-section {:open (or (= group "notes") (some #(= current-url (:page-url %)) pages))}
        [:summary group " " [:span.site-count "(" (count pages) ")"]]
        [:ol
         (for [page pages]
           [:li [:a.site-page-link
                 {:href (:page-url page)
                  :title (:page-title page)
                  :aria-current (when (= current-url (:page-url page)) "page")}
                 (:page-title page)]])]])]))

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
       [:p (count links) " incoming references"]
       [:ul
        (for [link (sort-by (juxt :source-title :heading-title :text) links)]
          [:li [:a {:href (or (:heading-url link) (:source-url link))}
                (or (:heading-title link) (:source-title link))]
           [:small (or (:source-title link) (:source-url link)) " — " (:text link)]])]])))

(defn head [ctx title current-url]
  (into [:head
         [:meta {:charset "utf-8"}]
         [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
         [:title title]
         [:link {:rel "stylesheet" :href (route/asset-url current-url "/assets/site.css")}]
         [:script {:src (route/asset-url current-url "/assets/search.js")
                   :data-index (route/asset-url current-url "/search-index.json")
                   :defer true}]]
        (head/render-head ctx current-url)))

(defn page [ctx document]
  (let [page-title (:title document)
        page-url (:url document)
        render-ctx (assoc ctx
                          :document document
                          :resolve-link (index/link-resolver (:index ctx)))
        rendered (render/render-document render-ctx (:ast document))]
    [:html {:lang (or (:lang ctx) "en")}
     (head ctx page-title page-url)
     [:body
      [:div.site-shell
       (nav ctx page-url)
       [:main.site-main
        [:div.page-meta
         (or (:source-rel document) (:source document))
         [:a.graph-link {:href (route/asset-url page-url "/graph.json")} "graph.json"]]
        rendered
        (backlinks ctx document)]]]]))

(defn home [ctx]
  (let [idx (:index ctx)
        current-url "/"]
    [:html {:lang (or (:lang ctx) "en")}
     (head ctx (or (:site-title ctx) "Loam") current-url)
     [:body
      [:div.site-shell
       (nav ctx current-url)
       [:main.site-main.home-main
        [:section.hero
         [:h1 (or (:site-title ctx) "Loam")]
         [:p "Generated from semantic EDN documents."]
         [:div.stats
          [:div.stat [:strong (count (:pages idx))] [:span "Pages"]]
          [:div.stat [:strong (count (:links idx))] [:span "Links"]]
          [:div.stat [:strong (count (:backlinks idx))] [:span "Backlink groups"]]
          [:div.stat [:strong (count (get-in idx [:graph :edges]))] [:span "Graph edges"]]]]
        [:h2 "All pages"]
        [:ul.page-list
         (for [page (sort-by :page-title (vals (:pages idx)))]
           [:li [:a {:href (:page-url page)} (:page-title page)]])]]]]]))

(def extension
  {:id :loam.theme/default
   :extends {:layouts {:page page
                       :home home}
             :assets {"assets/site.css" css
                      "assets/search.js" search-js}}})
