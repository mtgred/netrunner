(ns game-test.cards.ice.mlinzi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mlinzi
  ;; Mlinzi - take X net damage or trash the top X+1 cards from the Stack
  (do-game
    (new-game {:corp {:deck ["Mlinzi"]}
               :runner {:deck [(qty "Sure Gamble" 3)]}})
    (starting-hand state :runner ["Sure Gamble"])
    (play-from-hand state :corp "Mlinzi" "HQ")
    (take-credits state :corp)
    (let [ml (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp ml)
      (card-subroutine state :corp (refresh ml) 0)
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
      (click-prompt state :runner "Take 1 net damage")
      (is (= 1 (count (:discard (get-runner)))) "Runner trashed 1 card")
      (is (empty? (:hand (get-runner))) "Runner trashed card from hand")
      (card-subroutine state :corp (refresh ml) 0)
      (is (= 2 (count (:deck (get-runner)))) "Runner has 2 cards in stack")
      (click-prompt state :runner "Trash the top 2 cards of the stack")
      (is (= 3 (count (:discard (get-runner)))) "Runner trashed 2 cards")
      (is (empty? (:deck (get-runner))) "Runner trashed card from stack"))))
