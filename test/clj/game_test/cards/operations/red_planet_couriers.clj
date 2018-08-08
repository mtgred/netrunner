(ns game-test.cards.operations.red-planet-couriers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest red-planet-couriers
  ;; Red Planet Couriers - Move all advancements on cards to 1 advanceable card
  (do-game
    (new-game {:corp {:deck ["Red Planet Couriers" (qty "Ice Wall" 2)
                             "GRNDL Refinery" "Government Takeover"]}})
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Government Takeover" "New remote")
    (play-from-hand state :corp "GRNDL Refinery" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [gt (get-content state :remote1 0)
          gr (get-content state :remote2 0)
          iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)]
      (core/add-prop state :corp gr :advance-counter 3)
      (core/add-prop state :corp iw1 :advance-counter 2)
      (core/add-prop state :corp iw2 :advance-counter 1)
      (play-from-hand state :corp "Red Planet Couriers")
      (click-card state :corp gt)
      (is (zero? (get-counters (refresh gr) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw1) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw2) :advancement)) "Advancements removed")
      (is (= 6 (get-counters (refresh gt) :advancement)) "Gained 6 advancements"))))
