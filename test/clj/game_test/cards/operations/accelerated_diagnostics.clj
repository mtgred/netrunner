(ns game-test.cards.operations.accelerated-diagnostics
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Accelerated Diagnostics" "Cerebral Overwriter" "Shipment from SanSan"
                               "Hedge Fund" "Back Channels"]}})
      (starting-hand state :corp ["Accelerated Diagnostics" "Cerebral Overwriter"])
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Accelerated Diagnostics")
      (let [playarea (get-in @state [:corp :play-area])
            hf (find-card "Hedge Fund" playarea)
            ss (find-card "Shipment from SanSan" playarea)
            bc (find-card "Back Channels" playarea)
            co (get-content state :remote1 0)]
        (is (= 3 (count playarea)) "3 cards in play area")
        (click-card state :corp ss)
        (click-prompt state :corp "2")
        (click-card state :corp co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-card state :corp hf)
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")
        (click-card state :corp bc)
        (click-card state :corp (refresh co))
        (is (= 15 (:credit (get-corp))) "Corp gained 6 credits for Back Channels"))))
  (testing "Interaction with Current"
    (do-game
      (new-game {:corp {:deck ["Accelerated Diagnostics" "Cerebral Overwriter"
                               "Enhanced Login Protocol" "Shipment from SanSan"
                               "Hedge Fund"]}})
      (starting-hand state :corp ["Accelerated Diagnostics" "Cerebral Overwriter"])
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (core/gain state :corp :credit 3)
      (play-from-hand state :corp "Accelerated Diagnostics")
      (let [playarea (get-in @state [:corp :play-area])
            hf (find-card "Hedge Fund" playarea)
            ss (find-card "Shipment from SanSan" playarea)
            elp (find-card "Enhanced Login Protocol" playarea)
            co (get-content state :remote1 0)]
        (is (= 3 (count playarea)) "3 cards in play area")
        (click-card state :corp elp)
        (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:corp :current]))))
            "Enhanced Login Protocol active in Current area")
        (click-card state :corp ss)
        (click-prompt state :corp "2")
        (click-card state :corp co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-card state :corp hf)
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")))))
