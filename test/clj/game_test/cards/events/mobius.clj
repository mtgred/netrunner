(ns game-test.cards.events.mobius
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mobius
  ;; Mobius
  (do-game
    (new-game {:runner {:deck [(qty "Möbius" 3)]}})
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "Möbius")
    (core/no-action state :corp nil)
    (run-successful state)
    (is (= 5 (:credit (get-runner))))
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
    (core/no-action state :corp nil)
    (run-successful state)
    (click-prompt state :runner "No action")
    (is (= 9 (:credit (get-runner))))
    (is (empty? (:prompt (get-runner))) "No prompt to run a third time")
    (is (not (:run @state)) "Run is over")
    (play-from-hand state :runner "Möbius")
    (run-jack-out state)
    (is (empty? (:prompt (get-runner))) "No option to run again on unsuccessful run")))
