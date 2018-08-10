(ns game-test.cards.programs.bankroll
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bankroll
  ;; Bankroll
  (do-game
    (new-game {:runner {:deck ["Bankroll"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Bankroll")
    (is (= 3 (core/available-mu state)) "Bankroll uses up 1 MU")
    (is (= 4 (:credit (get-runner))) "Bankroll cost 1 to install")
    (let [bankroll (get-program state 0)
          hosted-credits #(get-counters (refresh bankroll) :credit)]
      (is (= 0 (hosted-credits)) "No counters on Bankroll on install")
      (run-empty-server state "Archives")
      (is (= 1 (hosted-credits)) "One credit counter on Bankroll after one successful run")
      (run-empty-server state "R&D")
      (is (= 2 (hosted-credits)) "Two credit counter on Bankroll after two successful runs")
      (run-empty-server state "HQ")
      (is (= 3 (hosted-credits)) "Three credit counter on Bankroll after three successful runs")
      (card-ability state :runner bankroll 0)
      (is (= (+ 4 3) (:credit (get-runner))) "Gained 3 credits when trashing Bankroll")
      (is (= 1 (-> (get-runner) :discard count)) "Bankroll was trashed"))))
