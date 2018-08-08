(ns game-test.cards.operations.building-blocks
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest building-blocks
  ;; Building Blocks - install and rez a barrier from HQ at no cost
  (testing "Basic behavior"
    (do-game
      (new-game {:corp {:deck ["Building Blocks" "Ice Wall"]}})
      (core/gain state :corp :credit 1)
      (is (= 6 (:credit (get-corp))) "Corp starts with 6 credits")
      (play-from-hand state :corp "Building Blocks")
      (is (= 1 (:credit (get-corp))) "Spent 5 credits on Building Blocks")
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [iw (get-ice state :remote1 0)]
        (is (= 1 (:credit (get-corp))) "Corp spent no credits installing ice")
        (is (:rezzed (refresh iw)) "Ice Wall is installed and rezzed"))))
  (testing "Select invalid card"
    (do-game
      (new-game {:corp {:deck ["Building Blocks" "Hedge Fund" "Cortex Lock"]}})
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Building Blocks")
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Starting prompt is correct")
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Cannot select non-ICE")
      (click-card state :corp (find-card "Cortex Lock" (:hand (get-corp))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Cannot select non-barrier ICE"))))
