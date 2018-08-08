(ns game-test.cards.agendas.corporate-war
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest corporate-war
  ;; Corporate War
  (do-game
    (new-game {:corp {:deck [(qty "Corporate War" 2)]}})
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "Corporate War")
    (is (zero? (:credit (get-corp))) "Lost all credits")
    (core/gain state :corp :credit 7)
    (play-and-score state "Corporate War")
    (is (= 14 (:credit (get-corp))) "Had 7 credits when scoring, gained another 7")))
