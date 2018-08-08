(ns game-test.cards.operations.power-grid-overload
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest power-grid-overload
  ;; Power Grid Overload
  (do-game
    (new-game {:corp {:deck ["Power Grid Overload"]}
               :runner {:deck ["Dyson Mem Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Dyson Mem Chip")
    (run-empty-server state :rd)
    (take-credits state :runner)
    (play-from-hand state :corp "Power Grid Overload")
    (click-prompt state :corp "3")
    (click-prompt state :runner "0")
    (click-card state :corp (get-hardware state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Dyson Mem Chip should be in heap after Runner loses Power Grid Overload trace")))
