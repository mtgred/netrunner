(ns game-test.cards.operations.hellion-beta-test
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hellion-beta-test
  ;; Hellion Beta Test
  (testing "Winning Trace - Trashing 2 cards"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team" "Hellion Beta Test"]}
                 :runner {:deck ["Daily Casts" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "Dyson Mem Chip")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (is (zero? (-> (get-runner) :discard count)) "Runner's heap should be empty")
      (play-from-hand state :corp "Hellion Beta Test")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-resource state 0))
      (click-card state :corp (get-hardware state 0))
      (is (= 2 (-> (get-runner) :discard count)) "Runner should have 2 cards in heap after losing Hellion Beta Test trace")))
  (testing "Losing trace - Gaining bad publicity"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team" "Hellion Beta Test"]}
                 :runner {:deck ["Daily Casts" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "Dyson Mem Chip")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (is (zero? (:bad-publicity (get-corp))) "Corp should start with 0 bad publicity")
      (play-from-hand state :corp "Hellion Beta Test")
      (click-prompt state :corp "0")
      (click-prompt state :runner "2")
      (is (= 1 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity from losing Hellion Beta Test trace"))))
