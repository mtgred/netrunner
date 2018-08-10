(ns game-test.cards.operations.threat-level-alpha
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest threat-level-alpha
  ;; Threat Level Alpha - Win trace to give tags = Runner tags; or 1 tag if 0
  (do-game
    (new-game {:corp {:deck [(qty "Threat Level Alpha" 2)]}})
    (core/gain state :corp :click 2)
    (core/gain state :corp :credit 2)
    (is (zero? (:tag (get-runner))))
    (play-from-hand state :corp "Threat Level Alpha")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag because they had 0")
    (core/gain state :runner :tag 2)
    (play-from-hand state :corp "Threat Level Alpha")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 6 (:tag (get-runner))) "Runner took 3 tag because they had 3")))
