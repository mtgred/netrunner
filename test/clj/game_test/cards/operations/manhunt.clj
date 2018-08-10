(ns game-test.cards.operations.manhunt
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest manhunt
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game {:corp {:deck ["Manhunt" (qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Manhunt")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (is (:prompt (get-corp)) "Manhunt trace initiated")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (run-empty-server state "HQ")
    (is (empty? (:prompt (get-corp))) "No Manhunt trace on second run")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))
