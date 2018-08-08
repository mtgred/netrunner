(ns game-test.cards.assets.edge-of-world
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest edge-of-world
  ;; Edge of World
  (do-game
    (new-game {:corp {:deck [(qty "Edge of World" 3) (qty "Ice Wall" 3)]}})
    (core/gain state :corp :credit 6 :click 1)
    (play-from-hand state :corp "Edge of World" "New remote")
    (play-from-hand state :corp "Edge of World" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :runner :prompt first :prompt-type))
        "Runner waiting for Corp to act")
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "Pay 0 [Credits] to trash")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
    (run-empty-server state "Server 2")
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "Pay 0 [Credits] to trash")
    (is (= 2 (:brain-damage (get-runner))) "Runner did not take brain damage when no ICE protected Edge of World")))
