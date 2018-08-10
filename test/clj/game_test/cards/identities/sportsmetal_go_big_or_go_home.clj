(ns game-test.cards.identities.sportsmetal-go-big-or-go-home
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sportsmetal-go-big-or-go-home
  ;; SportsMetal - gain 2 credits or draw 2 cards on agenda scored or stolen
  (testing "Gain 2 credits on score"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :corp "2 credits")
      (is (= 7 (:credit (get-corp))) "Corp gains 2 credits")))
  (testing "Gain 2 credits on steal"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "2 credits")
      (is (= 9 (:credit (get-corp))) "Corp gains 2 credits")))
  (testing "Gain 2 cards on score"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger" (qty "Hedge Fund" 2)]}})
      (starting-hand state :corp ["Merger"])
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (empty? (:hand (get-corp))) "Corp starts with no cards")
      (click-prompt state :corp "2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards")))
  (testing "Gain 2 cards on steal"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger" (qty "Hedge Fund" 2)]}})
      (starting-hand state :corp ["Merger"])
      (play-from-hand state :corp "Merger" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (empty? (:hand (get-corp))) "Corp starts with no cards")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards"))))
