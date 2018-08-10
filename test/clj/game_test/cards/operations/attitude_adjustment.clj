(ns game-test.cards.operations.attitude-adjustment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest attitude-adjustment
  ;; Attitude Adjustment
  (do-game
    (new-game {:corp {:deck ["Attitude Adjustment"
                             (qty "Hostile Takeover" 2)
                             (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Attitude Adjustment" "Hostile Takeover" "Hostile Takeover"])
    (trash-from-hand state :corp "Hostile Takeover")
    (let [hand (-> (get-corp) :hand count dec)] ;; cuz we're playing Attitude Adjustment
      (play-from-hand state :corp "Attitude Adjustment")
      (is (= (+ 2 hand) (-> (get-corp) :hand count)) "Corp should draw 2 cards"))
    (let [credits (-> (get-corp) :credit)
          hand (-> (get-corp) :hand count)
          discard (-> (get-corp) :discard count)
          deck (-> (get-corp) :deck count)]
      (click-card state :corp (find-card "Hostile Takeover" (:hand (get-corp))))
      (click-card state :corp (find-card "Hostile Takeover" (:discard (get-corp))))
      (is (= (+ 4 credits) (:credit (get-corp))) "Corp should gain 4 [Credits] for two revealed agendas")
      (is (= (dec hand) (-> (get-corp) :hand count)) "One card from HQ is shuffled into R&D")
      (is (= (dec discard) (-> (get-corp) :discard count)) "One card from Archives should be shuffled into R&D")
      (is (= (+ 2 deck) (-> (get-corp) :deck count)) "Corp should draw two cards and shuffle two cards into R&D"))))
