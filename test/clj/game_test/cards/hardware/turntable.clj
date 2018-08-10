(ns game-test.cards.hardware.turntable
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest turntable
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers" "Project Vitruvius"]}
                 :runner {:deck ["Turntable"]}})
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (let [ag1 (get-content state :remote1 0)]
        (score-agenda state :corp ag1)
        (take-credits state :corp)
        (play-from-hand state :runner "Turntable")
        (is (= 3 (:credit (get-runner))))
        (let [tt (get-hardware state 0)]
          (run-empty-server state "HQ")
          (click-prompt state :runner "Steal")
          (is (zero? (:agenda-point (get-runner))) "Stole Domestic Sleepers")
          (is (prompt-is-card? state :runner tt))
          (click-prompt state :runner "Yes")
          (click-card state :runner (find-card "Project Vitruvius" (:scored (get-corp))))
          (is (= 2 (:agenda-point (get-runner))) "Took Project Vitruvius from Corp")
          (is (zero? (:agenda-point (get-corp))) "Swapped Domestic Sleepers to Corp")))))
  (testing "vs Mandatory Upgrades"
    ;; Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn
    ;;           - Corp doesn't gain a click on the Runner's turn when it receives a Mandatory Upgrades
    (do-game
      (new-game {:corp {:deck [(qty "Mandatory Upgrades" 2) "Project Vitruvius"]}
                 :runner {:deck ["Turntable"]}})
      (score-agenda state :corp (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (let [tt (get-hardware state 0)]
        ;; steal Project Vitruvius and swap for Mandatory Upgrades
        (core/steal state :runner (find-card "Project Vitruvius" (:hand (get-corp))))
        (is (prompt-is-card? state :runner tt))
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Mandatory Upgrades" (:scored (get-corp))))
        (is (= 3 (:click-per-turn (get-corp))) "Back down to 3 clicks per turn")
        ;; steal second Mandatory Upgrades and swap for Project Vitruvius
        (core/steal state :runner (find-card "Mandatory Upgrades" (:hand (get-corp))))
        (is (prompt-is-card? state :runner tt))
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Project Vitruvius" (:scored (get-corp))))
        (is (zero? (:click (get-corp))) "Corp doesn't gain a click on Runner's turn")
        (is (= 4 (:click-per-turn (get-corp))))))))
