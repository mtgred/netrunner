(ns game-test.cards.assets.shannon-claire
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest shannon-claire
  ;; Shannon Claire
  (do-game
    (new-game {:corp {:deck ["Shannon Claire" "Hostile Takeover" "Ronin" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Shannon Claire" "Ronin"])
    (core/move state :corp (find-card "Ronin" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Shannon Claire" "New remote")
    (let [shannon (get-content state :remote1 0)]
      (core/rez state :corp shannon)
      (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in hand to start")
      (card-ability state :corp shannon 0)
      (is (= "Ronin" (-> (get-corp) :hand first :title)) "Corp should draw Ronin with Shannon's click ability")
      (let [number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (card-ability state :corp shannon 1)
        (click-prompt state :corp (find-card "Hostile Takeover" (:deck (get-corp))))
        (is (= "Hostile Takeover" (-> (get-corp) :deck last :title))
            "Agenda selected with Shannon's R&D ability should be on bottom of deck")
        (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Searching R&D should shuffle deck")))
    (core/move state :corp (find-card "Hostile Takeover" (:deck (get-corp))) :discard)
    (core/move state :corp (find-card "Shannon Claire" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Shannon Claire" "New remote")
    (let [shannon (get-content state :remote2 0)
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
      (core/rez state :corp shannon)
      (card-ability state :corp shannon 2)
      (click-prompt state :corp (find-card "Hostile Takeover" (:discard (get-corp))))
      (is (= "Hostile Takeover" (-> (get-corp) :deck last :title))
          "Agenda selected with Shannon's Archives ability should be on bottom of deck")
      (is (= number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck)))
          "Searching Archives shouldn't shuffle deck"))))
