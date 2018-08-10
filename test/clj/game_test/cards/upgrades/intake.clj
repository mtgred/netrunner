(ns game-test.cards.upgrades.intake
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest intake
  ;; Intake - Trace4, add an installed program or virtual resource to the grip
  (do-game
    (new-game {:corp {:deck [(qty "Intake" 3)]}
               :runner {:deck ["Corroder" "Fester" "Daily Casts"]}})
    (starting-hand state :corp ["Intake" "Intake"])
    (play-from-hand state :corp "Intake" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 5 :credit 10)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Fester")
    (play-from-hand state :runner "Daily Casts")
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (empty? (:hand (get-runner))) "Runner starts with no cards in hand")
    (click-card state :corp (get-program state 0))
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (run-on state "Archives")
    (run-successful state)
    (is (empty? (:prompt (get-corp))) "No prompt from Archives access")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (run-on state "Server 1")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (click-card state :corp (get-resource state 0))
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (click-prompt state :runner "No action") ; trash
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (click-prompt state :corp "Done")
    (click-prompt state :runner "No action") ; trash
    (is (empty? (:prompt (get-corp))) "Prompt closes after done")
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "5")
    (is (empty? (:prompt (get-corp))) "Prompt closes after lost trace")))
