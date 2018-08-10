(ns game-test.cards.icebreakers.study-guide
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest study-guide
  ;; Study Guide - 2c to add a power counter; +1 strength per counter
  (do-game
    (new-game {:runner {:deck ["Study Guide" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Study Guide")
    (let [sg (get-program state 0)]
      (card-ability state :runner sg 1)
      (is (= 4 (:credit (get-runner))) "Paid 2c")
      (is (= 1 (get-counters (refresh sg) :power)) "Has 1 power counter")
      (is (= 1 (:current-strength (refresh sg))) "1 strength")
      (card-ability state :runner sg 1)
      (is (= 2 (:credit (get-runner))) "Paid 2c")
      (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
      (is (= 2 (:current-strength (refresh sg))) "2 strength"))))
