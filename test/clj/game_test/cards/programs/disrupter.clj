(ns game-test.cards.programs.disrupter
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest disrupter
  ;; Disrupter
  (do-game
    (new-game {:corp {:deck [(qty "SEA Source" 2)]}
               :runner {:deck ["Disrupter"]}})
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (play-from-hand state :runner "Disrupter")
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :runner "Yes")
    (is (zero? (-> (get-corp) :prompt first :base)) "Base trace should now be 0")
    (is (= 1 (-> (get-runner) :discard count)) "Disrupter should be in Heap")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (zero? (:tag (get-runner))) "Runner should gain no tag from beating trace")
    (play-from-hand state :corp "SEA Source")
    (is (= 3 (-> (get-corp) :prompt first :base)) "Base trace should be reset to 3")))
