(ns game-test.cards.operations.divert-power
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest divert-power
  (do-game
    (new-game {:corp {:deck [(qty "Divert Power" 2) "Paper Wall" (qty "Eve Campaign" 3) ]}})
    (core/gain state :corp :click 3 :credit 11)
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (let [pw (get-ice state :hq 0)
          ec1 (get-content state :remote1 0)
          ec2 (get-content state :remote2 0)
          ec3 (get-content state :remote3 0)]
      (core/rez state :corp pw)
      (core/rez state :corp ec1)
      (core/rez state :corp ec2)
      (play-from-hand state :corp "Divert Power")
      (is (= 4 (:credit (get-corp))) "Corp has 4 credits after rezzes and playing Divert Power")
      (testing "Choose 2 targets to derez"
        (click-card state :corp (refresh pw))
        (click-card state :corp (refresh ec1))
        (click-prompt state :corp "Done"))
      (testing "Choose a target to rez for -6 cost"
        (click-card state :corp (refresh ec3)))
      (is (core/rezzed? (refresh ec3)) "Eve Campaign was rezzed")
      (is (= 4 (:credit (get-corp))) "Rezzed Eve Campaign for 0 credits")
      (is (not (core/rezzed? (refresh pw))) "Paper Wall was derezzed")
      (is (not (core/rezzed? (refresh ec1))) "First Eve Campaign was derezzed")
      (is (= 16 (get-counters (refresh ec3) :credit)) "Eve gained 16 credits on rez")
      (play-from-hand state :corp "Divert Power")
      (testing "Choose 1 target to derez"
        (click-card state :corp (refresh ec2))
        (click-prompt state :corp "Done"))
      (testing "Choose a target to rez for -3 cost"
        (click-card state :corp (refresh ec1)))
      (is (core/rezzed? (refresh ec1)) "First Eve Campaign was rezzed")
      (is (= 0 (:credit (get-corp))) "Rezzed Eve Campaign for 2 credits")
      (is (not (core/rezzed? (refresh ec2))) "Second Eve Campaign was derezzed")
      (is (= 32 (get-counters (refresh ec1) :credit)) "First Eve gained 16  more credits on rez"))))
