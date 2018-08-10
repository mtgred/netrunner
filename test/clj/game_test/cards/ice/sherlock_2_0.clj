(ns game-test.cards.ice.sherlock-2-0
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sherlock-2-0
  ;; Sherlock 2.0 - Trace to add an installed program to the bottom of Runner's Stack
  (do-game
    (new-game {:corp {:deck [(qty "Sherlock 2.0" 1)]}
               :runner {:deck [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Sherlock 2.0" "HQ")
    (take-credits state :corp)
    (let [sherlock (get-ice state :hq 0)]
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :hq)
      (core/rez state :corp sherlock)
      (card-subroutine state :corp sherlock 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-program state 0))
      (is (empty? (get-program state)) "Gordian uninstalled")
      (is (= "Gordian Blade" (:title (last (:deck (get-runner))))) "Gordian on bottom of Stack"))))
