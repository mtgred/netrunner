(ns game.core.actions-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest undo-turn-test
  (do-game
    (new-game)
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 1 (:click (get-corp))) "Corp spent 2 clicks")
    (is (= 13 (:credit (get-corp))) "Corp has 13 credits")
    (is (= 1 (count (:hand (get-corp)))) "Corp has 1 card in HQ")
    (core/command-undo-turn state :runner)
    (core/command-undo-turn state :corp)
    (is (= 3 (count (:hand (get-corp)))) "Corp has 3 cards in HQ")
    (is (zero? (:click (get-corp))) "Corp has no clicks - turn not yet started")
    (is (= 5 (:credit (get-corp))) "Corp has 5 credits")))

(deftest undo-click-test
  (do-game
    (new-game {:corp {:deck ["Ikawah Project"]}
               :runner {:deck ["Day Job"]}})
    (play-from-hand state :corp "Ikawah Project" "New remote")
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay to steal")
    (is (= 2 (:click (get-runner))) "Runner should lose 1 click to steal")
    (is (= 3 (:credit (get-runner))) "Runner should lose 2 credits to steal")
    (is (= 1 (count (:scored (get-runner)))) "Runner should steal Ikawah Project")
    (core/command-undo-click state :corp)
    (is (= 1 (count (:scored (get-runner)))) "Corp attempt to undo click does nothing")
    (core/command-undo-click state :runner)
    (is (zero? (count (:scored (get-runner)))) "Runner attempt to undo click works ok")
    (is (= 4 (:click (get-runner))) "Runner back to 4 clicks")
    (is (= 5 (:credit (get-runner))) "Runner back to 5 credits")
    (play-from-hand state :runner "Day Job")
    (is (zero? (:click (get-runner))) "Runner spent 4 clicks")
    (core/command-undo-click state :runner)
    (is (= 4 (:click (get-runner))) "Runner back to 4 clicks")
    (is (= 5 (:credit (get-runner))) "Runner back to 5 credits")))

(deftest undo-click-with-bioroid-cost-test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "R&D")
    (take-credits state :corp)
    (run-on state :rd)
    (let [ice (get-ice state :rd 0)]
      (rez state :corp ice)
      (run-continue state)
      (card-side-ability state :runner ice 0)
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state "Runner loses \\[Click\\] to use Eli 1.0 to break 1 subroutine on Eli 1.0"))
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state "Runner loses \\[Click\\] to use Eli 1.0 to break 1 subroutine on Eli 1.0")))
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "No action")
    (is (not (get-run)))
    (is (= 1 (:click (get-runner))))
    (core/command-undo-click state :runner)
    (is (= 4 (:click (get-runner))))
    (is (last-log-contains? state "Runner uses the undo-click command"))))

(deftest no-action-during-action-test
  (do-game
    (new-game {:runner {:deck [(qty "Sure Gamble" 10)]}})
    (take-credits state :corp)
    (run-on state :hq)
    (changes-val-macro
      0 (count (:hand (get-runner)))
      "Runner draws no cards"
      (core/process-action "draw" state :runner nil))))
