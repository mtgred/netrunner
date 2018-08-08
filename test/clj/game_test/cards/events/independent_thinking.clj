(ns game-test.cards.events.independent-thinking
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest independent-thinking
  ;; Independent Thinking - Trash 2 installed cards, including a facedown directive, and draw 2 cards
  (do-game
    (new-game {:runner {:id "Apex: Invasive Predator"
                        :deck ["Neutralize All Threats" (qty "Independent Thinking" 2)
                               (qty "Fan Site" 3) (qty "Street Magic" 3)]}})
    (starting-hand state :runner ["Fan Site" "Fan Site" "Neutralize All Threats"
                                  "Independent Thinking" "Independent Thinking"])
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (click-card state :runner (find-card "Neutralize All Threats" (:hand (get-runner))))
    (play-from-hand state :runner "Fan Site")
    (let [fs (get-resource state 0)
          nat (get-runner-facedown state 0)]
      (play-from-hand state :runner "Independent Thinking")
      (click-card state :runner fs)
      (click-card state :runner nat)
      (click-prompt state :runner "Done")
      (is (= 4 (count (:hand (get-runner)))) "Trashing 2 cards draws 2 card"))))
