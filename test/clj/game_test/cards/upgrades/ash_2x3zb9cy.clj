(ns game-test.cards.upgrades.ash-2x3zb9cy
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ash-2x3zb9cy
  ;; Ash 2X3ZB9CY
  (do-game
    (new-game {:corp {:deck ["Ash 2X3ZB9CY" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Ash 2X3ZB9CY" "Ice Wall"])
    (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
    (take-credits state :corp)
    (let [ash (get-content state :hq 0)]
      (core/rez state :corp ash)
      (run-empty-server state "HQ")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= "Ash 2X3ZB9CY" (-> (get-runner) :prompt first :card :title)) "Should access Ash")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (not (:run @state)) "Accessing Ash then ends the run"))))
