(ns game-test.cards.agendas.the-future-is-now
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-future-is-now
  ;; The Future is Now
  (testing "With at least one card in deck"
    (do-game
      (new-game {:corp {:deck ["The Future is Now" "Ice Wall"]}})
      (starting-hand state :corp ["The Future is Now"])
      (is (= 1 (count (:hand (get-corp)))))
      (is (= 1 (count (:deck (get-corp)))))
      (play-and-score state "The Future is Now")
      (click-prompt state :corp (find-card "Ice Wall" (:deck (get-corp))))
      (is (= 1 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))))
  (testing "With an empty deck"
    (do-game
      (new-game {:corp {:deck ["The Future is Now"]}})
      (is (= 1 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))
      (play-and-score state "The Future is Now")
      (is (empty? (:prompt (get-corp))) "Ability shouldn't fire if deck is empty")
      (is (zero? (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp))))))))
