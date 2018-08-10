(ns game-test.cards.agendas.standoff
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest standoff
  ;; Standoff
  (testing "Runner declines first"
    (do-game
      (new-game {:corp {:deck ["Standoff" "Ice Wall" "News Team"]}
                 :runner {:deck ["Cache"]}})
      (starting-hand state :corp ["Standoff" "Ice Wall"])
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (play-and-score state "Standoff")
      (starting-hand state :corp [])
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in Heap")
      (click-card state :runner (get-program state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Runner should now have 1 card in Heap")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have no cards in Archives")
      (click-card state :corp (get-ice state :hq 0))
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in Archives")
      (is (zero? (-> (get-corp) :hand count)) "Corp should have no cards in hand")
      (let [credits (:credit (get-corp))]
        (click-prompt state :runner "Done")
        (is (= (+ credits 5) (:credit (get-corp))) "Corp should gain 5 credits from Runner declining to trash an installed card")
        (is (= 1 (-> (get-corp) :hand count)) "Corp should draw a card from Runner declining to trash an installed card"))))
  (testing "Corp declines first"
    (do-game
      (new-game {:corp {:deck ["Standoff" "Ice Wall" "News Team"]}
                 :runner {:deck ["Cache" "Cache"]}})
      (starting-hand state :corp ["Standoff" "Ice Wall"])
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (play-and-score state "Standoff")
      (starting-hand state :corp [])
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in Heap")
      (click-card state :runner (get-program state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Runner should now have 1 card in Heap")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have no cards in Archives")
      (click-card state :corp (get-ice state :hq 0))
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in Archives")
      (is (zero? (-> (get-corp) :hand count)) "Corp should have no cards in hand")
      (click-card state :runner (get-program state 0))
      (is (= 2 (-> (get-runner) :discard count)) "Runner should now have 2 cards in Heap")
      (let [credits (:credit (get-corp))]
        (click-prompt state :corp "Done")
        (is (= credits (:credit (get-corp))) "Corp should gain no credits from declining to trash an installed card")
        (is (zero? (-> (get-corp) :hand count)) "Corp should draw no cards from declining to trash an installed card")))))
