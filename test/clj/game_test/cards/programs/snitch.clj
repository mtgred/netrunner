(ns game-test.cards.programs.snitch
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest snitch
  ;; Snitch - Only works on unrezzed ice
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 2)]}
               :runner {:deck ["Snitch"]}})
    (play-from-hand state :corp "Quandary" "R&D")
    (play-from-hand state :corp "Quandary" "HQ")
    (let [hqice (get-ice state :hq 0)]
      (core/rez state :corp hqice))
    (take-credits state :corp)
    (play-from-hand state :runner "Snitch")
    (let [snitch (get-program state 0)]
      ;; unrezzed ice scenario
      (run-on state "R&D")
      (card-ability state :runner snitch 0)
      (is (prompt-is-card? state :runner snitch) "Option to jack out")
      (click-prompt state :runner "Yes")
      ;; rezzed ice scenario
      (run-on state "HQ")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out")
      ;; no ice scenario
      (run-on state "Archives")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out"))))
