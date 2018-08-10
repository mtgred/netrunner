(ns game-test.cards.hardware.akamatsu-mem-chip
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest akamatsu-mem-chip
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game {:runner {:deck [(qty "Akamatsu Mem Chip" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 5 (core/available-mu state)) "Gain 1 memory")))
