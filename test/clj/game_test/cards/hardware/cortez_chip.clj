(ns game-test.cards.hardware.cortez-chip
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cortez-chip
  ;; Cortez Chip - Trash to add 2 credits to rez cost of an ICE until end of turn
  (do-game
    (new-game {:corp {:deck ["Quandary"]}
               :runner {:deck ["Cortez Chip"]}})
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Cortez Chip")
    (let [quan (get-ice state :rd 0)
          cortez (get-hardware state 0)]
      (card-ability state :runner cortez 0)
      (click-card state :runner quan)
      (is (= 1 (count (:discard (get-runner)))) "Cortez Chip trashed")
      (core/rez state :corp quan)
      (is (= 4 (:credit (get-corp))) "Paid 3c instead of 1c to rez Quandary"))))
