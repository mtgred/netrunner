(ns game-test.cards.programs.wari
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest wari
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Wari"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Wari")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Barrier")
    (click-card state :runner (get-ice state :rd 0))
    (is (= 1 (count (:discard (get-runner)))) "Wari in heap")
    (is (not (empty? (get-in @state [:runner :prompt]))) "Runner is currently accessing Ice Wall")))
