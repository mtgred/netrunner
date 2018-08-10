(ns game-test.cards.operations.punitive-counterstrike
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest punitive-counterstrike
  ;; Punitive Counterstrike - deal meat damage equal to printed agenda points
  (do-game
    (new-game {:corp {:deck ["Global Food Initiative" "Punitive Counterstrike"]}})
    (play-from-hand state :corp "Global Food Initiative" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Runner scored 2 points")
    (take-credits state :runner)
    (play-from-hand state :corp "Punitive Counterstrike")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (empty? (:hand (get-runner))) "Runner took 3 meat damage")))
