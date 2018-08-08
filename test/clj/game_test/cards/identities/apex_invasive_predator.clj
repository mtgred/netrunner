(ns game-test.cards.identities.apex-invasive-predator
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest apex-invasive-predator
  ;; Apex - Allow facedown install of a second console. Issue #1326
  (do-game
    (new-game {:runner {:id "Apex: Invasive Predator"
                        :deck [(qty "Heartbeat" 2)]}})
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (click-prompt state :runner "Done") ; no facedown install on turn 1
    (play-from-hand state :runner "Heartbeat")
    (is (= 1 (count (get-hardware state))))
    (take-credits state :runner)
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
    (is (= 1 (count (get-runner-facedown state))) "2nd console installed facedown")))
