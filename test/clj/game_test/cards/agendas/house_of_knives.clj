(ns game-test.cards.agendas.house-of-knives
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest house-of-knives
  ;; House of Knives
  (do-game
    (new-game {:corp {:deck ["House of Knives"]}})
    (play-and-score state "House of Knives")
    (let [hok-scored (get-scored state :corp 0)]
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should start with 3 counters")
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (run-phase-43 state)
      (card-ability state :corp hok-scored 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner should pay 1 net damage")
      (run-empty-server state "R&D")
      (run-phase-43 state)
      (card-ability state :corp hok-scored 0)
      (card-ability state :corp hok-scored 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner should pay 1 net damage"))))
