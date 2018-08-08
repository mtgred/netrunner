(ns game-test.cards.agendas.advanced-concept-hopper
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest advanced-concept-hopper
  ;; Advanced Concept Hopper
  (do-game
    (new-game {:corp {:deck ["Advanced Concept Hopper" (qty "Hedge Fund" 4)]}})
    (starting-hand state :corp ["Advanced Concept Hopper"])
    (play-and-score state "Advanced Concept Hopper")
    (take-credits state :corp)
    (testing "Corp draws 1 card, only once per turn"
      (let [cards (count (:hand (get-corp)))]
        (is (= cards (count (:hand (get-corp)))) (str "Corp should have " cards " cards in hand"))
        (run-on state :archives)
        (click-prompt state :corp "Draw 1 card")
        (is (= (inc cards) (count (:hand (get-corp)))) (str "Corp should have " (inc cards) " card in hand"))
        (run-successful state)
        (run-on state :archives)
        (is (empty (:prompt (get-corp))) "No prompt as it's once per turn")))
    (take-credits state :runner)
    (take-credits state :corp)
    (testing "Corp gains 1 credit, only once per turn"
      (let [credits (:credit (get-corp))]
        (is (= credits (:credit (get-corp))) (str "Corp should have " credits " credits"))
        (run-on state :archives)
        (click-prompt state :corp "Gain 1 [Credits]")
        (is (= (inc credits) (:credit (get-corp))) (str "Corp should have " (inc credits) " credits"))
        (run-successful state)
        (run-on state :archives)
        (is (empty (:prompt (get-corp))) "No prompt as it's once per turn")))))
