(ns game-test.cards.upgrades.satellite-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest satellite-grid
  ;; Satellite Grid - Add 1 fake advancement on all ICE protecting server
  (do-game
    (new-game {:corp {:deck ["Satellite Grid" (qty "Ice Wall" 2)]}})
    (play-from-hand state :corp "Satellite Grid" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)
          sg (get-content state :hq 0)]
      (core/gain state :corp :click 1)
      (advance state iw1)
      (core/rez state :corp sg)
      (core/rez state :corp (refresh iw1))
      (is (= 1 (:extra-advance-counter (refresh iw1))) "1 fake advancement token")
      (is (= 1 (get-counters (refresh iw1) :advancement)) "Only 1 real advancement token")
      (is (= 3 (:current-strength (refresh iw1))) "Satellite Grid counter boosting strength by 1")
      (core/rez state :corp (refresh iw2))
      (is (= 1 (:current-strength (refresh iw2))) "Satellite Grid not impacting ICE elsewhere")
      (core/derez state :corp sg)
      (is (= 2 (:current-strength (refresh iw1))) "Ice Wall strength boost only from real advancement"))))
