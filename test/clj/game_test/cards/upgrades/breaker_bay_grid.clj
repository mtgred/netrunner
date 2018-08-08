(ns game-test.cards.upgrades.breaker-bay-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest breaker-bay-grid
  ;; Breaker Bay Grid - Reduce rez cost of other cards in this server by 5 credits
  (do-game
    (new-game {:corp {:deck [(qty "Breaker Bay Grid" 2) "The Root" "Strongbox"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Breaker Bay Grid" "New remote")
    (play-from-hand state :corp "The Root" "Server 1")
    (let [bbg1 (get-content state :remote1 0)
          root (get-content state :remote1 1)]
      (core/rez state :corp bbg1)
      (core/rez state :corp root)
      (is (= 4 (:credit (get-corp))) "Paid only 1 to rez The Root")
      (play-from-hand state :corp "Breaker Bay Grid" "R&D")
      (play-from-hand state :corp "Strongbox" "R&D")
      (let [bbg2 (get-content state :rd 0)
            sbox (get-content state :rd 1)]
        (core/rez state :corp bbg2)
        (core/rez state :corp sbox)
        (is (= 1 (:credit (get-corp))) "Paid full 3 credits to rez Strongbox")))))
