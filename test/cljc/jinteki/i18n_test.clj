(ns jinteki.i18n-test
  (:require
   [clojure.test :refer [deftest is]]
   [jinteki.i18n :as sut]))

(deftest check-translations-test
  (is (empty? (sut/load-dictionary! "public/i18n"))))

(deftest msg-map-validates-keywords-test
  (is (= {:msg/type :tie} (sut/->msg-map :tie)))
  (is (thrown? Exception (sut/->msg-map :not-a-registered-msg))))
