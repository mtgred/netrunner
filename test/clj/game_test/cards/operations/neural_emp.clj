(ns game-test.cards.operations.neural-emp
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest neural-emp
  ;; Neural EMP - Play if Runner made a run the previous turn to do 1 net damage
  (do-game
    (new-game {:corp {:deck ["Neural EMP"]}})
    (play-from-hand state :corp "Neural EMP")
    (is (= 3 (:click (get-corp))) "Neural precondition not met; card not played")
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (take-credits state :runner)
    (play-from-hand state :corp "Neural EMP")
    (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")))
