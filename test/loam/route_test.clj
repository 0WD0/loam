(ns loam.route-test
  (:require [clojure.test :refer [deftest is testing]]
            [loam.route :as route]))

(deftest validates-repository-and-page-paths
  (is (route/safe-relative-path? "docs/majutsu.org"))
  (is (route/safe-page-path? "workflows/inspecting"))
  (doseq [path ["/home/user/docs.org" "../docs.org" "docs//x.org"
                "C:/docs/x.org" "docs\\x.org" "docs/x.org?q=1"]]
    (testing path
      (is (not (route/safe-relative-path? path)))
      (is (seq (route/relative-path-problems path)))))
  (is (= "/docs/zh/dev/guide/start/"
         (route/docs-route {:base "/docs/"
                            :locale "zh"
                            :version "dev"
                            :page-path "guide/start"})))
  (is (= "/docs/v1.2/guide/start/"
         (route/docs-route {:version "v1.2" :page-path "guide/start"}))))

(deftest route-collision-keys-are-case-insensitive
  (is (= (route/normalized-route-key "/docs/dev/Guide/Start/")
         (route/normalized-route-key "/docs/dev/guide/start/"))))
