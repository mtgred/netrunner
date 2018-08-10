(ns game-test.cards.ice.flare
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest flare
  ;; Flare - Trash 1 program, do 2 unpreventable meat damage, and end the run
  (do-game
    (new-game {:corp {:deck ["Flare"]}
               :runner {:deck ["Plascrete Carapace" "Clone Chip" (qty "Cache" 3)]}})
    (play-from-hand state :corp "Flare" "HQ")
    (core/gain state :corp :credit 2)
    (take-credits state :corp)
    (play-from-hand state :runner "Plascrete Carapace")
    (play-from-hand state :runner "Clone Chip")
    (let [flare (get-ice state :hq 0)
          cc (get-hardware state 1)]
      (run-on state :hq)
      (core/rez state :corp flare)
      (card-subroutine state :corp flare 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp cc)
      (is (= 1 (count (get-hardware state))) "Clone Chip trashed")
      (is (empty? (:prompt (get-runner))) "Plascrete didn't try preventing meat damage")
      (is (= 1 (count (:hand (get-runner)))))
      (is (= 3 (count (:discard (get-runner)))) "Clone Chip plus 2 cards lost from damage in discard")
      (is (not (:run @state)) "Run ended"))))
