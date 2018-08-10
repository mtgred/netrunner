(ns game-test.cards.operations.patch
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game {:corp {:deck ["Patch" "Vanilla"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Patch")
    (click-card state :corp (get-ice state :hq 0))
    (is (= 2 (:current-strength (get-ice state :hq 0))) "Vanilla at 2 strength")))
