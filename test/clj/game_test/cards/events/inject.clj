(ns game-test.cards.events.inject
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest inject
  ;; Inject - Draw 4 cards from Stack and gain 1 credit per trashed program
  (do-game
    (new-game {:runner {:deck ["Inject" (qty "Imp" 2) (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Imp" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Imp" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (is (= 4 (count (:deck (get-runner)))))
    (play-from-hand state :runner "Inject")
    (is (= 2 (count (:hand (get-runner)))) "2 non-programs kept in Grip")
    (is (= 2 (count (filter #(= (:type %) "Program") (:discard (get-runner)))))
        "2 programs in Heap")
    (is (= 6 (:credit (get-runner)))
        "Paid 1 credit to play Inject, gained 2 credits from trashed programs")))
