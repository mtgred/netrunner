(ns game-test.cards.agendas.ikawah-project
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ikawah-project
  ;; Ikawah Project
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ikawah Project"]}})
      (play-from-hand state :corp "Ikawah Project" "New remote")
      (testing "No credits"
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click 3)
        (run-empty-server state :remote1)
        (click-prompt state :runner "No action")
        (is (zero? (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Ikawah Project"))
      (testing "No clicks"
        (take-credits state :runner)
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click 3)
        (run-empty-server state :remote1)
        (click-prompt state :runner "No action")
        (is (zero? (:click (get-runner))) "Runner couldn't afford to steal, so no clicks spent")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Ikawah Project"))
      (testing "Enough of both"
        (take-credits state :runner)
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click (:click (get-runner)))
        (core/gain state :runner :credit 5 :click 4)
        (is (= 5 (:credit (get-runner))) "Runner should be reset to 5 credits")
        (is (= 4 (:click (get-runner))) "Runner should be reset to 4 clicks")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay to steal")
        (click-prompt state :runner "[Click]")
        (click-prompt state :runner "2 [Credits]")
        (is (= 2 (:click (get-runner))) "Runner should lose 1 click to steal")
        (is (= 3 (:credit (get-runner))) "Runner should lose 2 credits to steal")
        (is (= 3 (:agenda-point (get-runner))))
        (is (= 1 (count (:scored (get-runner)))) "Runner should steal Ikawah Project"))))
  (testing "Not stealing"
    ;; do not reveal when the Runner does not steal from R&D
    (do-game
      (new-game {:corp {:deck [(qty "Ikawah Project" 2)]}})
      (take-credits state :corp)
      (starting-hand state :corp ["Ikawah Project"])
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (not (last-log-contains? state "Ikawah Project")) "Ikawah Project should not be mentioned")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (last-log-contains? state "Ikawah Project") "Ikawah Project should be mentioned"))))
