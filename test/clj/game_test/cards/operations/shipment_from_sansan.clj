(ns game-test.cards.operations.shipment-from-sansan
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest shipment-from-sansan
  ;; Shipment from SanSan - placing advancements
  (do-game
    (new-game {:corp {:deck [(qty "Shipment from SanSan" 3) (qty "Ice Wall" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iwall (get-ice state :hq 0)]
      (play-from-hand state :corp "Shipment from SanSan")
      (click-prompt state :corp "2")
      (click-card state :corp iwall)
      (is (= 5 (:credit (get-corp))))
      (is (= 2 (get-counters (refresh iwall) :advancement))))))
