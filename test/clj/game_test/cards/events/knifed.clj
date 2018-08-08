(ns game-test.cards.events.knifed
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest knifed
  ;; Knifed - Make a run, trash a barrier if all subs broken
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Knifed"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Knifed")
    (click-prompt state :runner "HQ")
    (run-successful state)))
