(ns game-test.cards.agendas.executive-retreat
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest executive-retreat
  ;; Executive Retreat
  (do-game
    (new-game {:corp {:deck ["Executive Retreat" (qty "Hedge Fund" 5)]}})
    (starting-hand state :corp ["Executive Retreat" "Hedge Fund"])
    (is (= 2 (count (:hand (get-corp)))) "Corp should start with 1 card in HQ")
    (play-and-score state "Executive Retreat")
    (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in HQ after shuffling HQ back into R&D")
    (let [er-scored (get-scored state :corp 0)]
      (card-ability state :corp er-scored 0)
      (is (= 5 (count (:hand (get-corp)))) "Corp should have 5 cards in hand")
      (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters")))
  (testing "Overdraw"
    (do-game
      (new-game {:corp {:deck ["Executive Retreat" (qty "Hedge Fund" 4)]}})
      (starting-hand state :corp ["Executive Retreat" "Hedge Fund"])
      (is (= 2 (count (:hand (get-corp)))) "Corp should start with 1 card in HQ")
      (play-and-score state "Executive Retreat")
      (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in HQ after shuffling HQ back into R&D")
      (let [er-scored (get-scored state :corp 0)]
        (card-ability state :corp er-scored 0)
        (is (= 4 (count (:hand (get-corp)))) "Corp should have 5 cards in hand")
        (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters")
        (is (= :runner (:winner @state)) "Runner wins")
        (is (= "Decked" (:reason @state)) "Win condition reports decked")))))
