(ns game-test.cards.identities.reina-roja-freedom-fighter
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reina-roja-freedom-fighter
  ;; Reina Roja - Increase cost of first rezzed ICE
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 3)]}
               :runner {:id "Reina Roja: Freedom Fighter"}})
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (core/rez state :corp quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))
