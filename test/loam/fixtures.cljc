(ns loam.fixtures)

(def doc-a
  {:type :org-data
   :properties {:path "/notes/a.org"
                :ID "page-a"
                :EXPORT_FILE_NAME "a"}
   :contents [{:type :keyword
               :properties {:key "TITLE" :value "Page A"}}
              {:type :headline
               :properties {:level 1 :raw-value "References" :ID "heading-a"}
               :contents [{:type :section
                           :contents [{:type :paragraph
                                       :contents ["See "
                                                  {:type :link
                                                   :properties {:type "id"
                                                                :path "page-b"
                                                                :raw-link "id:page-b"}
                                                   :contents ["Page B"]}
                                                  " and "
                                                  {:type :link
                                                   :properties {:type "fuzzy"
                                                                :path "Radio B"
                                                                :resolved {:resolved-type :radio-target
                                                                           :resolved-value "Radio B"}}
                                                   :contents ["radio"]}]}]}]}]})

(def doc-b
  {:type :org-data
   :properties {:path "/notes/b.org"
                :ID "page-b"
                :EXPORT_FILE_NAME "b"}
   :contents [{:type :keyword
               :properties {:key "TITLE" :value "Page B"}}
              {:type :headline
               :properties {:level 1
                            :raw-value "Custom Heading"
                            :CUSTOM_ID "custom-b"}
               :contents [{:type :section
                           :contents [{:type :paragraph
                                       :contents [{:type :radio-target
                                                   :properties {:value "Radio B"}}
                                                  "Target page"]}]}]}]})
