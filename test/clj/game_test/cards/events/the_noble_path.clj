(ns game-test.cards.events.the-noble-path
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-noble-path
  ;; The Noble Path - Prevents damage during run
  (do-game
    (new-game {:runner {:deck ["The Noble Path" (qty "Sure Gamble" 2)]}})
    (let [hand-count #(count (:hand (get-runner)))]
      (starting-hand state :runner ["The Noble Path" "Sure Gamble"])
      (take-credits state :corp)
      ;; Play The Noble Path and confirm it trashes remaining cards in hand
      (is (= 2 (hand-count)) "Start with 2 cards")
      (play-from-hand state :runner "The Noble Path")
      (is (zero? (hand-count)) "Playing Noble Path trashes the remaining cards in hand")
      ;; Put a card into hand so I can confirm it's not discarded by damage
      ;; Don't want to dealing with checking damage on a zero card hand
      (starting-hand state :runner ["Sure Gamble"])
      (core/damage state :runner :net 1)
      (is (= 1 (hand-count)) "Damage was prevented")
      ;; Finish the run and check that damage works again
      (click-prompt state :runner "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (core/damage state :runner :net 1)
      (is (zero? (hand-count)) "Damage works again after run"))))
