(ns game-test.cards.upgrades.tempus
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tempus
  ;; Tempus - Trace^3, the runner chooses to lose 2 clicks or take 1 brain damage
  (do-game
    (new-game {:corp {:deck [(qty "Tempus" 3)]}
               :runner {:deck [(qty "Sure Gamble" 3)]}})
    (starting-hand state :corp ["Tempus"])
    (play-from-hand state :corp "Tempus" "New remote")
    (take-credits state :corp)
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
    (click-prompt state :runner "Lose [Click][Click]")
    (is (= 1 (:click (get-runner))) "Runner loses 2 clicks")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (run-on state "Server 1")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (is (zero? (:brain-damage (get-runner))) "Runner starts with 0 brain damage")
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (take-credits state :runner)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-successful state)
    (is (= 1 (:brain-damage (get-runner))) "Runner takes no brain damage")
    (is (= 3 (:click (get-runner))) "Runner loses no clicks")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner starts with 1 brain damage")
    (click-prompt state :runner "Take 1 brain damage")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 1 brain damage")
    (click-prompt state :runner "No action") ; don't trash
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "4")
    (click-prompt state :runner "Pay 0 [Credits] to trash")))
