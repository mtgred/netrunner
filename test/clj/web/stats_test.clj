(ns web.stats-test
  (:require
   [clojure.test :refer :all]
   [web.stats :refer :all]))

(deftest strip-opponent-deck-name-test
  (let [game {:corp   {:player {:username "alice"} :deck-name "HB Fast Advance"}
              :runner {:player {:username "bob"}   :deck-name "Stealth Andy"}}]
    (testing "corp user: runner deck-name is stripped, corp deck-name is kept"
      (let [result (strip-opponent-deck-name game "alice")]
        (is (= "HB Fast Advance" (get-in result [:corp :deck-name])))
        (is (nil? (get-in result [:runner :deck-name])))))
    (testing "runner user: corp deck-name is stripped, runner deck-name is kept"
      (let [result (strip-opponent-deck-name game "bob")]
        (is (= "Stealth Andy" (get-in result [:runner :deck-name])))
        (is (nil? (get-in result [:corp :deck-name])))))))
