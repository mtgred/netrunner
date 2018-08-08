(ns game-test.cards.programs.pheromones
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest pheromones
  ;; Pheromones ability shouldn't have a NullPointerException when fired with 0 virus counter
  (do-game
    (new-game {:runner {:deck ["Pheromones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Pheromones")
    (let [ph (get-program state 0)]
      (card-ability state :runner (refresh ph) 0)
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (refresh ph) :virus)) "Pheromones gained 1 counter")
      (card-ability state :runner (refresh ph) 0)))) ; this doesn't do anything, but shouldn't crash
