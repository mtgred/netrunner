(ns game-test.cards.assets.haas-arcology-ai
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest haas-arcology-ai
  ;; Haas Arcology AI - Click and advancement to gain 2 clicks, once per turn
  (do-game
    (new-game {:corp {:deck ["Haas Arcology AI"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Haas Arcology AI" "New remote")
    (let [haa (get-content state :remote1 0)]
      (advance state haa 2)
      (core/rez state :corp (refresh haa))
      (is (= 1 (:click (get-corp))))
      (is (= 2 (get-counters (refresh haa) :advancement)))
      (card-ability state :corp (refresh haa) 0)
      (is (= 1 (get-counters (refresh haa) :advancement)) "Spent 1 advancement")
      (is (= 2 (:click (get-corp))) "Spent last click to gain 2 clicks")
      (card-ability state :corp (refresh haa) 0)
      (is (= 1 (get-counters (refresh haa) :advancement)) "Can't use twice in a turn")
      (is (= 2 (:click (get-corp))) "Didn't spend a click"))))
