(ns game-test.cards.assets.private-contracts
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest private-contracts
  ;; Private Contracts
  (do-game
    (new-game {:corp {:deck ["Private Contracts"]}})
    (play-from-hand state :corp "Private Contracts" "New remote")
    (let [pri (get-content state :remote1 0)]
      (core/rez state :corp pri)
      (is (= 14 (get-counters (refresh pri) :credit)) "Should start with 14 credits")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have 0 cards in Archives")
      (core/gain state :corp :click 7)
      (core/lose state :corp :credit 2)
      (dotimes [_ 7]
        (card-ability state :corp pri 0))
      (is (= 1 (-> (get-corp) :discard count)) "Private Contracts should be in discard")
      (is (= 14 (:credit (get-corp))) "Corp should now have 14 credits"))))
