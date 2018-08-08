(ns game-test.cards.agendas.underway-renovation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest underway-renovation
  ;; Underway Renovation
  (do-game
    (new-game {:corp {:deck ["Underway Renovation" "Shipment from SanSan"]}})
    (core/gain state :corp :click 2)
    (starting-hand state :runner [])
    (play-from-hand state :corp "Underway Renovation" "New remote")
    (let [ur (get-content state :remote1 0)]
      (advance state ur)
      (is (last-log-contains? state "Sure Gamble")
          "Underway Renovation trashed card name is in log")
      ; check for #2370
      (is (not (last-log-contains? state "Sure Gamble, Sure Gamble"))
          "Underway Renovation trashed card name is in log")
      (is (= 1 (count (:discard (get-runner)))) "1 card milled from Runner Stack")
      (play-from-hand state :corp "Shipment from SanSan")
      (click-prompt state :corp "2")
      (click-card state :corp ur)
      (is (= 3 (get-counters (refresh ur) :advancement)))
      (is (= 1 (count (:discard (get-runner)))) "No Runner mills; advancements were placed")
      (advance state ur)
      (is (= 4 (get-counters (refresh ur) :advancement)))
      (is (last-log-contains? state "Sure Gamble, Sure Gamble")
          "Underway Renovation trashed card name is in log")
      (is (= 3 (count (:discard (get-runner)))) "2 cards milled from Runner Stack; 4+ advancements"))))
