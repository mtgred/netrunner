(ns game.quotes-test
  (:require
   [clojure.test :refer [deftest is]]
   [game.quotes :as sut]))

(deftest check-translations-test
  (is (sut/load-quotes!)))
