(ns game-test.cards.hardware.brain-chip
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
    (new-game {:runner {:deck ["Brain Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Brain Chip")
    (swap! state assoc-in [:runner :agenda-point] -2) ; hard set ap
    (is (= 5 (core/hand-size state :runner)) "Hand size unaffected")
    (is (= 4 (core/available-mu state)) "Memory limit unaffected")
    (swap! state assoc-in [:runner :agenda-point] 2)
    (is (= 7 (core/hand-size state :runner)) "Hand size increased by 2")
    (is (= 6 (core/available-mu state)) "Memory limit increased by 2")
    (core/move state :runner (get-hardware state 0) :discard)
    (is (= 5 (core/hand-size state :runner)) "Hand size reset")
    (is (= 4 (core/available-mu state)) "Memory limit reset")))
