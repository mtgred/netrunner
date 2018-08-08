(ns game-test.cards.icebreakers.tycoon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tycoon
  ;; Tycoon
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Tycoon"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/rez state state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Tycoon")
    (let [tycoon (get-program state 0)
          credits (:credit (get-corp))]
      (run-on state "HQ")
      (card-ability state :runner tycoon 0)
      (is (= credits (:credit (get-corp))) "Corp doesn't gain credits until encounter is over")
      (run-continue state)
      (is (= (+ credits 2) (:credit (get-corp))) "Corp gains 2 credits from Tycoon being used"))))
