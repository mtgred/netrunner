(ns game-test.cards.agendas.veterans-program
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest veterans-program
  ;; Veterans Program
  (testing "Veterans Program basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2) "Veterans Program"]}})
      (play-and-score state "Hostile Takeover")
      (play-and-score state "Hostile Takeover")
      (is (= 19 (:credit (get-corp))) "Should gain 14 credits from 5 to 19")
      (is (= 2 (:bad-publicity (get-corp))) "Should gain 2 bad publicity")
      (play-and-score state "Veterans Program")
      (is (zero? (:bad-publicity (get-corp))) "Should lose 2 bad publicity")))
  (testing "Removes _up to 2_ bad publicity"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Veterans Program"]}})
      (play-and-score state "Hostile Takeover")
      (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
      (is (= 1 (:bad-publicity (get-corp))) "Should gain 1 bad publicity")
      (play-and-score state "Veterans Program")
      (is (zero? (:bad-publicity (get-corp))) "Should lose 1 bad publicity"))))
