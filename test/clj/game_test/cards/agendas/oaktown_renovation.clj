(ns game-test.cards.agendas.oaktown-renovation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest oaktown-renovation
  ;; Oaktown Renovation
  (do-game
    (new-game {:corp {:deck ["Oaktown Renovation" "Shipment from SanSan"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [oak (get-content state :remote1 0)]
      (is (:rezzed (refresh oak)) "Oaktown installed face up")
      (advance state oak)
      (is (= 6 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (play-from-hand state :corp "Shipment from SanSan")
      (click-prompt state :corp "2")
      (click-card state :corp oak)
      (is (= 3 (get-counters (refresh oak) :advancement)))
      (is (= 6 (:credit (get-corp))) "No credits gained due to advancements being placed")
      (advance state oak)
      (is (= 7 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (advance state oak)
      (is (= 5 (get-counters (refresh oak) :advancement)))
      (is (= 9 (:credit (get-corp)))
          "Spent 1 credit to advance, gained 3 credits from Oaktown"))))
