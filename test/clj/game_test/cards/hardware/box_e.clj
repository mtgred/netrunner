(ns game-test.cards.hardware.box-e
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
    (new-game {:runner {:deck ["Box-E"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Box-E")
    (is (= 6 (core/available-mu state)))
    (is (= 7 (core/hand-size state :runner)))))
