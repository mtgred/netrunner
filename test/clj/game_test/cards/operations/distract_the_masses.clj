(ns game-test.cards.operations.distract-the-masses
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest distract-the-masses
  (do-game
    (new-game {:corp {:deck [(qty "Distract the Masses" 2) (qty "Hedge Fund" 3)]}})
    (starting-hand state :corp ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Distract the Masses" "Distract the Masses"])
    (play-from-hand state :corp "Distract the Masses")
    (click-card state :corp (first (:hand (get-corp))))
    (click-card state :corp (first (next (:hand (get-corp)))))
    (click-card state :corp (first (:discard (get-corp))))
    (click-prompt state :corp "Done")
    (is (= 1 (count (:discard (get-corp)))) "1 card still discarded")
    (is (= 1 (count (:deck (get-corp)))) "1 card shuffled into R&D")
    (is (= 1 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-runner))) "Runner gained 2 credits")
    (play-from-hand state :corp "Distract the Masses")
    (click-card state :corp (first (:hand (get-corp))))
    (click-prompt state :corp "Done")
    (click-card state :corp (first (:discard (get-corp))))
    (click-card state :corp (first (next (:discard (get-corp)))))
    (is (zero? (count (:discard (get-corp)))) "No cards left in archives")
    (is (= 3 (count (:deck (get-corp)))) "2 more cards shuffled into R&D")
    (is (= 2 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 9 (:credit (get-runner))) "Runner gained 2 credits")))
