(ns game-test.cards.operations.kill-switch
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kill-switch
  ;; Kill Switch
  (do-game
    (new-game {:corp {:deck ["Kill Switch" (qty "Hostile Takeover" 2)]}})
    (play-from-hand state :corp "Kill Switch")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 brain damage")
    (play-and-score state "Hostile Takeover")
    (click-prompt state :corp "Hostile Takeover")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner should get 1 brain damage from Kill Switch after Corp scores an agenda")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 2 (:brain-damage (get-runner))) "Runner should get 1 brain damage from Kill Switch after accecssing an agenda")))
