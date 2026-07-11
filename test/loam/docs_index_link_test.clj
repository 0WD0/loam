(ns loam.docs-index-link-test
  (:require [clojure.test :refer [deftest is testing]]
            [loam.compile :as compile]
            [loam.docs-fixtures :as fixtures]))

(defn diagnostic-codes [result]
  (set (map :code (:diagnostics result))))

(deftest duplicate-identities-routes-and-anchors-are-hard-errors
  (let [duplicate-headings
        [(fixtures/headline 2 "One" {:CUSTOM_ID "same-anchor" :ID "same-id"}
                            (fixtures/section (fixtures/paragraph "one")))
         (fixtures/headline 2 "Two" {:CUSTOM_ID "same-anchor" :ID "same-id"}
                            (fixtures/section (fixtures/paragraph "two")))]
        first-page (apply fixtures/headline 1 "First"
                          {:CUSTOM_ID "duplicate-page"
                           :EXPORT_FILE_NAME "Guide/Same"
                           :DESCRIPTION "First"}
                          duplicate-headings)
        second-page (fixtures/headline 1 "Second"
                                       {:CUSTOM_ID "duplicate-page"
                                        :EXPORT_FILE_NAME "guide/same"
                                        :DESCRIPTION "Second"}
                                       (fixtures/section (fixtures/paragraph "second")))
        ast (fixtures/document "Duplicates" first-page second-page)
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/duplicates.org" ast)]
                fixtures/compile-opts)
        codes (diagnostic-codes result)]
    (is (= :error (:status result)))
    (is (every? codes [:duplicate-page-id :duplicate-route
                       :duplicate-custom-id :duplicate-id :duplicate-anchor]))
    (is (every? #(= "docs/duplicates.org" (get-in % [:source-span :path]))
                (filter #(contains? #{:duplicate-page-id :duplicate-route
                                      :duplicate-custom-id :duplicate-id
                                      :duplicate-anchor}
                                    (:code %))
                        (:diagnostics result))))
    (is (nil? (:artifacts result)))))

(deftest resolves-explicit-and-local-precedence-links
  (let [page-a
        (fixtures/page "A" "a" "guide/a"
                       (fixtures/section
                        (fixtures/paragraph
                         (fixtures/target "Shared")
                         " "
                         (fixtures/link "fuzzy" "Shared" "local")
                         " "
                         (fixtures/link "id" "page-b-id" "B"))))
        page-b
        (fixtures/headline 1 "B"
                           {:CUSTOM_ID "b"
                            :ID "page-b-id"
                            :EXPORT_FILE_NAME "guide/b"
                            :DESCRIPTION "B description"}
                           (fixtures/headline 2 "Shared" {:CUSTOM_ID "remote-shared"}
                                              (fixtures/section (fixtures/paragraph "remote"))))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/links.org"
                                          (fixtures/document "Links" page-a page-b))]
                fixtures/compile-opts)
        html (get-in result [:artifacts :fragments "pages/guide-a.html"])]
    (is (= :ok (:status result)) (:diagnostics result))
    (is (re-find #"href=\"/docs/dev/guide/a/#Shared\">local" html))
    (is (re-find #"href=\"/docs/dev/guide/b/\">B" html))
    (is (= "/docs/dev/guide/b/" (get-in result [:index :ids "page-b-id" :href])))
    (is (= ["links:a" "links:b"]
           (get-in result [:index :sources "docs/links.org"])))))

(deftest ambiguous-and-unresolved-links-never-select-the-first-candidate
  (let [shared (fn [id path]
                 (fixtures/page id id path
                                (fixtures/headline 2 "Shared" {}
                                                   (fixtures/section (fixtures/paragraph id)))))
        link-page (fixtures/page "Links" "links" "guide/links"
                                 (fixtures/section
                                  (fixtures/paragraph
                                   (fixtures/link "fuzzy" "Shared" "ambiguous")
                                   " "
                                   (fixtures/link "fuzzy" "Missing" "missing"))))
        result (compile/compile-documents
                [(fixtures/envelope-input
                  "docs/ambiguous.org"
                  (fixtures/document "Ambiguous"
                                     (shared "one" "guide/one")
                                     (shared "two" "guide/two")
                                     link-page))]
                fixtures/compile-opts)
        diagnostics (:diagnostics result)]
    (is (= :error (:status result)))
    (is (= 1 (count (filter #(= :ambiguous-link (:code %)) diagnostics))))
    (is (= 1 (count (filter #(= :unresolved-link (:code %)) diagnostics))))
    (is (= 2 (count (get-in (first (filter #(= :ambiguous-link (:code %)) diagnostics))
                            [:data :candidates]))))
    (is (nil? (:artifacts result)))))

(deftest rejects-dangerous-link-schemes
  (let [page (fixtures/page "Unsafe" "unsafe" "guide/unsafe"
                            (fixtures/section
                             (fixtures/paragraph
                              (fixtures/link "javascript" "alert(1)" "boom"))))
        result (compile/compile-documents
                [(fixtures/envelope-input "docs/unsafe-link.org"
                                          (fixtures/document "Unsafe" page))]
                fixtures/compile-opts)]
    (is (= :error (:status result)))
    (is (contains? (diagnostic-codes result) :unsafe-link-scheme))))
