(ns game-test.cards.agendas.the-future-perfect
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-future-perfect
  ;; The Future Perfect
  (do-game
    (new-game {:corp {:deck [(qty "The Future Perfect" 2)]}})
    (play-from-hand state :corp "The Future Perfect" "New remote")
    (take-credits state :corp)
    (testing "No steal on not-equal Psi game"
      (run-empty-server state "HQ")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      ;; Cannot steal prompt
      (click-prompt state :runner "No action")
      (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP"))
    (testing "Successful steal on equal Psi game"
      (run-empty-server state "HQ")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "Steal")
      (is (= 3 (:agenda-point (get-runner))) "Runner stole TFP"))
    (testing "No Psi game and successful steal when installed"
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 6 (:agenda-point (get-runner))) "Runner stole TFP - no Psi game on installed TFP"))))
