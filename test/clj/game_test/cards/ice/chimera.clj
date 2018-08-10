(ns game-test.cards.ice.chimera
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest chimera
  ;; Chimera - Gains chosen subtype
  (do-game
    (new-game {:corp {:deck ["Chimera"]}})
    (play-from-hand state :corp "Chimera" "HQ")
    (let [ch (get-ice state :hq 0)]
      (core/rez state :corp ch)
      (click-prompt state :corp "Barrier")
      (is (core/has-subtype? (refresh ch) "Barrier") "Chimera has Barrier")
      (take-credits state :corp)
      (is (not (core/has-subtype? (refresh ch) "Barrier")) "Chimera does not have Barrier"))))
