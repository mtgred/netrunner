(ns game-test.cards.upgrades.signal-jamming
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest signal-jamming
  ;; Trash to stop installs for the rest of the run
  (do-game
    (new-game {:corp {:deck [(qty "Signal Jamming" 3)]}
               :runner {:deck [(qty "Self-modifying Code" 3) "Reaver"]}})
    (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
    (play-from-hand state :corp "Signal Jamming" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Self-modifying Code")
    (let [smc1 (get-program state 0)
          smc2 (get-program state 1)
          sj (get-content state :hq 0)]
      (core/rez state :corp sj)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :corp sj 0)
      (card-ability state :runner smc1 0)
      (is (empty? (:prompt (get-runner))) "SJ blocking SMC")
      (run-jack-out state)
      (card-ability state :runner smc2 0)
      (click-prompt state :runner "Reaver"))))
