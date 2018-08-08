(ns game-test.cards.hardware.paragon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest paragon
  ;; Paragon - Gain 1 credit and may look at and move top card of Stack to bottom
  (do-game
    (new-game {:runner {:deck ["Paragon" "Easy Mark" "Sure Gamble"]}})
    (starting-hand state :runner ["Paragon"])
    (take-credits state :corp)
    (play-from-hand state :runner "Paragon")
    (run-empty-server state "HQ")
    (is (prompt-is-card? state :runner (get-hardware state 0)) "Prompt from Paragon")
    (click-prompt state :runner "Yes")
    (is (= (+ 5 -3 1) (:credit (get-runner))) "Gained 1 credit from Paragon")
    (is (prompt-is-card? state :runner (get-hardware state 0)) "Prompt from Paragon")
    (let [top-cid (:cid (first (:deck (get-runner))))]
      (click-prompt state :runner "Yes")
      (is (= top-cid (:cid (last (:deck (get-runner))))) "Moved top card to bottom"))
    (run-empty-server state "HQ")
    (is (not (prompt-is-card? state :runner (get-hardware state 0))) "No prompt from Paragon")))
