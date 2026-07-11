(ns loam.docs-model-test
  (:require [clojure.test :refer [deftest is testing]]
            [loam.compile :as compile]
            [loam.docs-fixtures :as fixtures]
            [loam.docs.model :as model]))

(deftest partitions-the-frozen-sixteen-page-snapshot
  (let [snapshot (fixtures/page-root-snapshot)
        result (compile/compile-documents [(fixtures/sixteen-page-input)]
                                          fixtures/compile-opts)
        pages (:pages result)
        expected-ids (mapv #(str "majutsu:" (:custom-id %)) snapshot)
        expected-paths (mapv :export-file-name snapshot)]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= compile/phase-order (:phase-trace result)))
    (is (= 16 (count pages)))
    (is (= expected-ids (mapv :page/id pages)))
    (is (= expected-paths (mapv :page/path pages)))
    (is (= (mapv #(str "/docs/dev/" % "/") expected-paths)
           (mapv :page/route pages)))
    (is (= expected-ids (get-in result [:index :sources "docs/majutsu.org"])))
    (is (= "majutsu:manipulating"
           (:page/previous (nth pages 6))))
    (is (= "majutsu:bookmarks"
           (:page/next (nth pages 6))))
    (is (= (set expected-ids) (set (keys (get-in result [:index :pages])))))
    (is (not (contains? (get-in result [:artifacts :manifest]) :source)))
    (is (vector? (get-in result [:artifacts :manifest :sources])))
    (is (= expected-ids
           (mapv :id (get-in result [:artifacts :manifest :navigation]))))
    (is (every? empty?
                (map :children (get-in result [:artifacts :manifest :navigation]))))))

(deftest models-nested-page-ownership-without-mutating-the-ast
  (let [nested-child (fixtures/headline 4 "Nested detail" {:CUSTOM_ID "nested-detail"}
                                        (fixtures/section (fixtures/paragraph "Nested body")))
        inline-heading
        (fn [raw title]
          (fixtures/headline 4 raw
                             {:title {:type :anonymous :properties {} :contents title}}
                             (fixtures/section (fixtures/paragraph "Heading body"))))
        log-columns (inline-heading
                     "~majutsu-log-commit-columns~"
                     [{:type :verbatim
                       :properties {:value "majutsu-log-commit-columns"}
                       :contents []}])
        understanding (inline-heading
                       "Understanding =--revisions="
                       ["Understanding "
                        {:type :code :properties {:value "--revisions"} :contents []}])
        face-policy (inline-heading
                     "Face Policy (=:face=)"
                     ["Face Policy ("
                      {:type :code :properties {:value ":face"} :contents []}
                      ")"])
        postprocessing (inline-heading
                        "Postprocessing (=:post=)"
                        ["Postprocessing ("
                         {:type :code :properties {:value ":post"} :contents []}
                         ")"])
        nested-root (fixtures/headline 3 "Nested page"
                                       {:CUSTOM_ID "nested-page"
                                        :EXPORT_FILE_NAME "guide/nested"
                                        :DESCRIPTION "Nested description"
                                        :start-line 20
                                        :end-line 40}
                                       nested-child
                                       log-columns
                                       understanding
                                       face-policy
                                       postprocessing)
        parent-heading (fixtures/headline 2 "Parent detail" {:CUSTOM_ID "parent-detail"}
                                          (fixtures/section (fixtures/paragraph "Parent body"))
                                          nested-root)
        after-heading (fixtures/headline 2 "After child" {:CUSTOM_ID "after-child"}
                                         (fixtures/section
                                          (fixtures/paragraph "After child body")))
        ast (fixtures/document "Manual"
                               (fixtures/page "Parent" "parent" "guide/parent"
                                              (fixtures/section
                                               (fixtures/paragraph
                                                "Open the "
                                                (fixtures/link "custom-id" "nested-page"
                                                               "nested page")
                                                " or its "
                                                (fixtures/link "custom-id" "nested-detail"
                                                               "nested detail")
                                                "."))
                                              parent-heading
                                              after-heading))
        original ast
        result (compile/compile-documents [(fixtures/envelope-input "docs/manual.org" ast)]
                                          fixtures/compile-opts)
        [parent nested] (:pages result)
        manifest (get-in result [:artifacts :manifest])
        [parent-manifest nested-manifest] (:pages manifest)
        parent-html (get-in result [:artifacts :fragments "pages/guide-parent.html"])
        nested-html (get-in result [:artifacts :fragments "pages/guide-nested.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= original ast))
    (is (= ["manual:parent" "manual:nested-page"] (mapv :page/id (:pages result))))
    (is (= [[0] [1]] (mapv :page/order (:pages result))))
    (is (= "manual:parent" (:page/parent nested)))
    (is (= ["manual:nested-page"] (:page/children parent)))
    (is (= "manual:nested-page" (:page/next parent)))
    (is (= "manual:parent" (:page/previous nested)))
    (is (= "Nested page" (:page/title nested)))
    (is (= "Nested description" (:page/description nested)))
    (is (= 20 (get-in nested [:page/source :start-line])))
    (is (= 40 (get-in nested [:page/source :end-line])))
    (is (re-find #"<h2[^>]*id=\"parent-detail\"" parent-html))
    (is (re-find #"<h2[^>]*id=\"after-child\"" parent-html))
    (is (re-find #"After child body" parent-html))
    (is (not (re-find #"Nested body" parent-html)))
    (is (re-find #"<h2[^>]*id=\"nested-detail\"" nested-html))
    (is (not (re-find #"<h[34][^>]*id=\"nested-detail\"" nested-html)))
    (is (re-find
         #"id=\"majutsu-log-commit-columns\"[^>]*>.*<code class=\"org-verbatim\">majutsu-log-commit-columns</code>"
         nested-html))
    (is (re-find
         #"id=\"understanding-revisions\"[^>]*>.*Understanding <code>--revisions</code>"
         nested-html))
    (is (re-find #"id=\"face-policy-face\"" nested-html))
    (is (re-find #"id=\"postprocessing-post\"" nested-html))
    (is (not (re-find #"[~=]majutsu-log-commit-columns[~=]" nested-html)))
    (is (re-find #"href=\"/docs/dev/guide/nested/\">nested page" parent-html))
    (is (re-find #"href=\"/docs/dev/guide/nested/#nested-detail\">nested detail"
                 parent-html))
    (is (= "manual:nested-page"
           (get-in result [:index :custom-ids "nested-detail" :page-id])))
    (is (= "manual:parent" (:id parent-manifest)))
    (is (= ["manual:nested-page"] (:childIds parent-manifest)))
    (is (= "manual:parent" (:parentId nested-manifest)))
    (is (= [{:depth 2 :slug "nested-detail" :text "Nested detail"}
            {:depth 2 :slug "majutsu-log-commit-columns"
             :text "majutsu-log-commit-columns"}
            {:depth 2 :slug "understanding-revisions"
             :text "Understanding --revisions"}
            {:depth 2 :slug "face-policy-face" :text "Face Policy (:face)"}
            {:depth 2 :slug "postprocessing-post" :text "Postprocessing (:post)"}]
           (:headings nested-manifest)))
    (is (= {:id "manual:parent"
            :title "Parent"
            :path "guide/parent"
            :route "/docs/dev/guide/parent/"
            :order [0]
            :children [{:id "manual:nested-page"
                        :title "Nested page"
                        :path "guide/nested"
                        :route "/docs/dev/guide/nested/"
                        :order [1]
                        :children []}]}
           (first (:navigation manifest))))
    (is (not (re-find #"<h1" (str parent-html nested-html))))))

(deftest rejects-a-nested-page-root-route-collision
  (let [nested (fixtures/headline 2 "Nested"
                                  {:CUSTOM_ID "nested"
                                   :EXPORT_FILE_NAME "guide/parent"
                                   :DESCRIPTION "Nested description"}
                                  (fixtures/section (fixtures/paragraph "nested")))
        ast (fixtures/document "Collision"
                               (fixtures/page "Parent" "parent" "guide/parent" nested))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/nested-collision.org" ast)]
                fixtures/compile-opts)]
    (is (= :error (:status result)))
    (is (some #(= :duplicate-route (:code %)) (:diagnostics result)))
    (is (nil? (:artifacts result)))))

(deftest assigns-visible-preamble-to-a-landing-page
  (let [ast (fixtures/document "Manual"
                               (fixtures/section (fixtures/paragraph "Landing copy"))
                               (fixtures/page "Start" "start" "guide/start"
                                              (fixtures/section (fixtures/paragraph "Page copy"))))
        result (compile/compile-documents [(fixtures/envelope-input "docs/manual.org" ast)]
                                          fixtures/compile-opts)
        [landing page] (:pages result)]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (:page/landing? landing))
    (is (= "manual:landing" (:page/id landing)))
    (is (= "manual" (:page/path landing)))
    (is (= "manual:landing" (:page/parent page)))
    (is (= ["manual:start"] (:page/children landing)))
    (is (= "manual:landing" (:page/previous page)))
    (is (re-find #"Landing copy" (get-in result [:artifacts :fragments "pages/manual.html"])))
    (is (not (re-find #"Page copy" (get-in result [:artifacts :fragments "pages/manual.html"]))))
    (is (= ["manual:start"]
           (mapv :id (get-in result [:artifacts :manifest :navigation 0 :children]))))
    (is (true? (get-in result [:artifacts :manifest :navigation 0 :landing])))))

(deftest rejects-unsafe-or-extension-bearing-page-routes
  (doseq [[path expected-code]
          [["../escape" :unsafe-route]
           ["guide//empty" :unsafe-route]
           ["guide/query?q=1" :unsafe-route]
           ["guide/page.html" :page-path-has-extension]]]
    (testing path
      (let [ast (fixtures/document "Unsafe"
                                   (fixtures/page "Unsafe" "unsafe" path
                                                  (fixtures/section (fixtures/paragraph "body"))))
            result (compile/compile-documents [(fixtures/envelope-input "docs/unsafe.org" ast)]
                                              fixtures/compile-opts)]
        (is (= :error (:status result)))
        (is (some #(= expected-code (:code %)) (:diagnostics result)))
        (is (nil? (:artifacts result)))))))
