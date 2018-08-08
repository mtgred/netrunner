(ns game-test.cards.agendas.quantum-predictive-model
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest quantum-predictive-model
  ;; Quantum Predictive Model
  (do-game
    (new-game {:corp {:deck [(qty "Quantum Predictive Model" 4)]}})
    (testing "Set up"
      (starting-hand state :corp ["Quantum Predictive Model" "Quantum Predictive Model"])
      (play-from-hand state :corp "Quantum Predictive Model" "New remote")
      (play-from-hand state :corp "Quantum Predictive Model" "New remote")
      (take-credits state :corp))
    (testing "Access installed with no tag"
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner should steal"))
    (testing "Access R&D with no tag"
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should steal"))
    (core/gain state :runner :tag 1)
    (testing "Access intalled with tag"
      (run-on state :remote2)
      (run-successful state)
      (click-prompt state :runner "OK") ;; this is now a prompt that QPM was added to Corp score area
      (is (= 2 (:agenda-point (get-runner))) "Runner should not steal")
      (is (= 1 (:agenda-point (get-corp))) "Corp should score"))
    (testing "Access R&D with tag"
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :runner "OK")
      (is (= 2 (:agenda-point (get-runner))) "Runner should not steal")
      (is (= 2 (:agenda-point (get-corp))) "Corp should score"))
    (is (zero? (count (:deck (get-corp)))))))
