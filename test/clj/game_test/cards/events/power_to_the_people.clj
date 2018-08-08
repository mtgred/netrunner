(ns game-test.cards.events.power-to-the-people
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest power-to-the-people
  ;; Power to the People - Gain 7c the first time you access an agenda
  (do-game
    (new-game {:corp {:deck ["NAPD Contract" "Hostile Takeover"]}
               :runner {:deck ["Power to the People"]}})
    (play-from-hand state :corp "NAPD Contract" "New remote")
    (take-credits state :corp)
    (core/lose state :runner :credit 2)
    (let [napd (get-content state :remote1 0)]
      (play-from-hand state :runner "Power to the People")
      (is (= 3 (:credit (get-runner))) "Can't afford to steal NAPD")
      (run-empty-server state "Server 1")
      (is (= 10 (:credit (get-runner))) "Gained 7c on access, can steal NAPD")
      (click-prompt state :runner "Pay 4 [Credits] to steal")
      (is (= 2 (:agenda-point (get-runner))) "Stole agenda")
      (is (= 6 (:credit (get-runner))))
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 6 (:credit (get-runner))) "No credits gained from 2nd agenda access"))))
