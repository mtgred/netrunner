(ns game-test.cards.events.leave-no-trace
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest leave-no-trace
  ;; Leave No Trace should derez ICE that was rezzed during the run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Leave No Trace"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 1))
      (take-credits state :corp)
      (play-from-hand state :runner "Leave No Trace")
      (click-prompt state :runner "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (run-successful state)
      (is (not (:rezzed (get-ice state :hq 0))) "Inner Ice Wall should not be rezzed")
      (is (:rezzed (get-ice state :hq 1)) "Outer Ice Wall should be rezzed still")))
  (testing "should not derez ICE that has changed during a run"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Leave No Trace"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (is (:rezzed (get-ice state :hq 0)) "Ice Wall should be rezzed initially")
      (play-from-hand state :runner "Leave No Trace")
      (click-prompt state :runner "Archives")
      (core/add-prop state :corp (get-ice state :hq 0) :advance-counter 1)
      (run-successful state)
      (is (= 1 (get-counters (get-ice state :hq 0) :advancement)))
      (is (:rezzed (get-ice state :hq 0)) "Ice Wall should still be rezzed"))))
