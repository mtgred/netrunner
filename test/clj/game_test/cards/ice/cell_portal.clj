(ns game-test.cards.ice.cell-portal
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cell-portal
  ;; Cell Portal - Bounce Runner to outermost position and derez itself
  (do-game
    (new-game {:corp {:deck ["Cell Portal" (qty "Paper Wall" 2)]}})
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Cell Portal" "HQ")
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (run-continue state)
    (run-continue state)
    (is (= 1 (get-in @state [:run :position])))
    (let [cp (get-ice state :hq 0)]
      (core/rez state :corp cp)
      (card-subroutine state :corp cp 0)
      (is (= 3 (get-in @state [:run :position])) "Run back at outermost position")
      (is (not (:rezzed (refresh cp))) "Cell Portal derezzed"))))
