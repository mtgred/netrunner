(ns game-test.cards.ice.aimor
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest aimor
  ;; Aimor - trash the top 3 cards of the stack, trash Aimor
  (do-game
    (new-game {:corp {:deck ["Aimor"]}
               :runner {:deck [(qty "Sure Gamble" 2) "Desperado" "Corroder" "Patron"]}})
    (starting-hand state :runner ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :corp "Aimor" "HQ")
    (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Aimor installed")
    (take-credits state :corp)
    (let [aim (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp aim)
      (card-subroutine state :corp aim 0)
      (is (= 3 (count (:discard (get-runner)))) "Runner trashed 3 cards")
      (is (= 1 (count (:deck (get-runner)))) "Runner has 1 card in deck"))
    (is (zero? (count (get-in @state [:corp :servers :hq :ices]))) "Aimor trashed")))
