(ns game-test.cards.agendas.project-atlas
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest project-atlas
  ;; Project Atlas
  (testing "basic test"
    (do-game
      (new-game {:corp {:deck ["Project Atlas"
                               "Beanstalk Royalties"]}})
      ;; Set up
      (starting-hand state :corp ["Project Atlas"])
      (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand")
      (core/gain state :corp :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote1 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (core/score state :corp {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (click-prompt state :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand"))))
  (testing "test with Titan"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck [(qty "Project Atlas" 2) "Beanstalk Royalties" "Hedge Fund"]}})
      ;; Set up
      (starting-hand state :corp ["Project Atlas" "Project Atlas"])
      (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")
      (core/gain state :corp :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote1 0)]
        (advance state atlas 3)
        (is (= 3 (get-counters (refresh atlas) :advancement)) "Atlas should have 3 advancement tokens")
        (core/score state :corp {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (click-prompt state :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 card in hand"))
      ;; Should gain 2 counters
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote2 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (core/score state :corp {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :corp 1)]
        (is (= 2 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 2 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (click-prompt state :corp (find-card "Hedge Fund" (:deck (get-corp))))
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counters")
        (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")))))
