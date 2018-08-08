(ns game-test.cards.agendas.bifrost-array
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bifrost-array
  ;; Bifrost Array
  (do-game
    (new-game {:corp {:deck ["Bifrost Array" "Hostile Takeover"]}})
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
    (is (= 1 (:bad-publicity (get-corp))) "Should gain 1 bad publicity")
    (let [ht-scored (get-scored state :corp 0)]
      (play-and-score state "Bifrost Array")
      (click-prompt state :corp "Yes")
      (click-card state :corp "Hostile Takeover")
      (is (= 19 (:credit (get-corp))) "Should gain 7 credits from 12 to 19")
      (is (= 2 (:bad-publicity (get-corp))) "Should gain 1 bad publicity"))))
