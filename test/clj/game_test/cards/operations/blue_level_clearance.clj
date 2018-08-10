(ns game-test.cards.operations.blue-level-clearance
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest blue-level-clearance
  ;; Blue Level Clearance - Gain 5 credits and draw 2 cards
  (do-game
    (new-game {:corp {:deck [(qty "Blue Level Clearance" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Sweeps Week" 2)]}})
    (play-from-hand state :corp "Blue Level Clearance")
    (is (= 8 (:credit (get-corp))) "Gained 5 credits")
    (is (= 1 (:click (get-corp))))
    (is (= 7 (count (:hand (get-corp)))) "Drew 2 cards")))
