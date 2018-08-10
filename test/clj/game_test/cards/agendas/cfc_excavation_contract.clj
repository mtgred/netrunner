(ns game-test.cards.agendas.cfc-excavation-contract
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cfc-excavation-contract
  ;; CFC Excavation Contract
  (dotimes [n 5]
    (do-game
      (new-game {:corp {:deck ["CFC Excavation Contract" (qty "Eli 1.0" n)]}})
      (core/gain state :corp :click 10 :credit 10)
      (is (= 15 (:credit (get-corp))) "Should start with 5 credits")
      (dotimes [_ n]
        (play-from-hand state :corp "Eli 1.0" "New remote")
        (core/rez state :corp (get-ice state (keyword (str "remote" (:rid @state))) 0)))
      (let [credit (:credit (get-corp))]
        (play-and-score state "CFC Excavation Contract")
        (is (= (+ credit (* 2 n)) (:credit (get-corp)))
            (str "Should now have with " (+ credit (* 2 n)) " credits"))))))
