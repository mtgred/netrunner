(ns game-test.cards.operations.hatchet-job
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hatchet-job
  ;; Hatchet Job - Win trace to add installed non-virtual to grip
  (do-game
    (new-game {:corp {:deck ["Hatchet Job"]}
               :runner {:deck ["Upya" "Ghost Runner"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Ghost Runner")
    (play-from-hand state :runner "Upya")
    (take-credits state :runner)
    (play-from-hand state :corp "Hatchet Job")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (is (empty? (:hand (get-runner))) "Can't choose virtual card")
    (is (not (empty? (:prompt (get-corp)))))
    (click-card state :corp (get-program state 0))
    (is (= 1 (count (:hand (get-runner)))) "Upya returned to grip")))
