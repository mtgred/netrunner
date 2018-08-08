(ns game-test.cards.events.mad-dash
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mad-dash
  ;; Mad Dash - Make a run. Move to score pile as 1 point if steal agenda.  Take 1 meat if not
  (do-game
    (new-game {:corp {:deck ["Project Atlas"]}
               :runner {:deck [(qty "Mad Dash" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mad Dash")
    (click-prompt state :runner "Archives")
    (run-successful state)
    (is (= 2 (count (:discard (get-runner)))) "Took a meat damage")
    (play-from-hand state :runner "Mad Dash")
    (click-prompt state :runner "HQ")
    (run-successful state)
    (click-prompt state :runner "Steal")
    (is (= 2 (count (:scored (get-runner)))) "Mad Dash moved to score area")
    (is (= 3 (:agenda-point (get-runner))) "Mad Dash scored for 1 agenda point")))
