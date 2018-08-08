(ns game-test.cards.identities.industrial-genomics-growing-solutions
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest industrial-genomics-growing-solutions
  ;; Industrial Genomics - Increase trash cost
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck [(qty "PAD Campaign" 3) (qty "Hedge Fund" 3)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (let [pad (get-content state :remote1 0)]
        (core/rez state :corp pad)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (= 8 (core/trash-cost state :runner (refresh pad)))))))
  (testing "with Product Recall"
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck ["Product Recall" (qty "PAD Campaign" 3) (qty "Hedge Fund" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (let [pad (get-content state :remote1 0)]
        (core/rez state :corp pad)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (= 8 (core/trash-cost state :runner (refresh pad))))
        (run-jack-out state)
        (take-credits state :runner)
        (play-from-hand state :corp "Product Recall")
        (let [credits (:credit (get-corp))]
          (click-card state :corp pad)
          (is (= (+ credits 8) (:credit (get-corp))) "Gain 8 credits from trashing PAD Campaign"))))))
