(ns game-test.cards.resources.neutralize-all-threats
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest neutralize-all-threats
  ;; Neutralize All Threats - Access 2 cards from HQ, force trash first accessed card with a trash cost
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 2) "Breaker Bay Grid" "Elizabeth Mills"]}
               :runner {:deck ["Neutralize All Threats"]}})
    (play-from-hand state :corp "Breaker Bay Grid" "New remote")
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Neutralize All Threats")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Card from hand")
    (click-prompt state :runner "No action") ; access first Hedge Fund
    (click-prompt state :runner "Card from hand")
    (click-prompt state :runner "No action") ; access second Hedge Fund
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 2 [Credits] to trash")
    (is (= 3 (:credit (get-runner))) "Forced to pay 2c to trash BBG")
    (is (= 1 (count (:discard (get-corp)))) "Breaker Bay Grid trashed")
    (run-empty-server state "Server 2")
    (is (not (empty? (:prompt (get-runner)))) "Runner prompt to trash Elizabeth Mills")))
