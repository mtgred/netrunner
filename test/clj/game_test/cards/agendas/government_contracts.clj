(ns game-test.cards.agendas.government-contracts
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest government-contracts
  ;; Government Contracts
  (do-game
    (new-game {:corp {:deck ["Government Contracts"]}})
    (play-and-score state "Government Contracts")
    (is (= 2 (:click (get-corp))))
    (card-ability state :corp (get-scored state :corp 0) 0)
    (is (zero? (:click (get-corp))) "Spent 2 clicks")
    (is (= 9 (:credit (get-corp))) "Gained 4 credits")))
