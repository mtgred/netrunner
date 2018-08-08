(ns game-test.cards.assets.levy-university
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest levy-university
  ;; Levy University
  (do-game
    (new-game {:corp {:deck ["Levy University" "Ice Wall" (qty "Fire Wall" 10)]}})
    (starting-hand state :corp ["Levy University"])
    (play-from-hand state :corp "Levy University" "New remote")
    (let [levy (get-content state :remote1 0)
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
      (core/rez state :corp levy)
      (is (zero? (-> (get-corp) :hand count)) "HQ should be empty")
      (let [clicks (:click (get-corp))
            credits (:credit (get-corp))]
        (card-ability state :corp (refresh levy) 0)
        (click-prompt state :corp (find-card "Ice Wall" (:deck (get-corp))))
        (is (= (- credits 1) (:credit (get-corp))) "Levy University ability should cost 1 credit")
        (is (= (- clicks 1) (:click (get-corp))) "Levy University ability should cost 1 click"))
      (is (= 1 (-> (get-corp) :hand count)) "HQ should have 1 card")
      (is (= "Ice Wall" (-> (get-corp) :hand first :title)) "HQ should contain Ice Wall")
      (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle deck"))))
