(ns jinteki.i18n-test
  (:require
   [clojure.test :refer [deftest is]]
   [jinteki.i18n :as sut]))

(deftest check-translations-test
  (is (empty? (sut/load-dictionary! "resources/public/i18n"))))
