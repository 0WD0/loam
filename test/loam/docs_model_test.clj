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
    (is (vector? (get-in result [:artifacts :manifest :sources])))))

(deftest models-nested-page-ownership-without-mutating-the-ast
  (let [nested-child (fixtures/headline 4 "Nested detail" {:CUSTOM_ID "nested-detail"}
                                        (fixtures/section (fixtures/paragraph "Nested body")))
        nested-root (fixtures/headline 3 "Nested page"
                                       {:CUSTOM_ID "nested-page"
                                        :EXPORT_FILE_NAME "guide/nested"
                                        :DESCRIPTION "Nested description"}
                                       nested-child)
        parent-heading (fixtures/headline 2 "Parent detail" {:CUSTOM_ID "parent-detail"}
                                          (fixtures/section (fixtures/paragraph "Parent body"))
                                          nested-root)
        ast (fixtures/document "Manual"
                               (fixtures/page "Parent" "parent" "guide/parent"
                                              parent-heading))
        original ast
        result (compile/compile-documents [(fixtures/envelope-input "docs/manual.org" ast)]
                                          fixtures/compile-opts)
        [parent nested] (:pages result)
        parent-html (get-in result [:artifacts :fragments "pages/guide-parent.html"])
        nested-html (get-in result [:artifacts :fragments "pages/guide-nested.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (= original ast))
    (is (= "manual:parent" (:page/parent nested)))
    (is (= ["manual:nested-page"] (:page/children parent)))
    (is (= "manual:parent" (:page/previous nested)))
    (is (re-find #"<h2[^>]*id=\"parent-detail\"" parent-html))
    (is (not (re-find #"Nested body" parent-html)))
    (is (re-find #"<h2[^>]*id=\"nested-detail\"" nested-html))
    (is (not (re-find #"<h1" (str parent-html nested-html))))))

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
    (is (= "manual:landing" (:page/previous page)))
    (is (re-find #"Landing copy" (get-in result [:artifacts :fragments "pages/manual.html"])))
    (is (not (re-find #"Page copy" (get-in result [:artifacts :fragments "pages/manual.html"]))))))

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
