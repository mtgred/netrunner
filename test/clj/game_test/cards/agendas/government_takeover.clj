(ns game-test.cards.agendas.government-takeover
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest government-takeover
  ;; Government Takeover
  (do-game
    (new-game {:corp {:deck ["Government Takeover"]}})
    (play-and-score state "Government Takeover")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (let [gt-scored (get-scored state :corp 0)]
      (card-ability state :corp gt-scored 0)
      (is (= 8 (:credit (get-corp))) "Should gain 3 credits from 5 to 8"))))
