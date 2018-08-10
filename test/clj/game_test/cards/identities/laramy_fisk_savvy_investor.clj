(ns game-test.cards.identities.laramy-fisk-savvy-investor
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest laramy-fisk-savvy-investor
  ;; Laramy Fisk
  (testing "installing a Shard should still give option to force Corp draw"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3)]}
                 :runner {:id "Laramy Fisk: Savvy Investor"
                          :deck ["Eden Shard"]}})
      (starting-hand state :corp ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Eli 1.0" "Eli 1.0"])
      (take-credits state :corp)
      (run-on state "R&D")
      (core/no-action state :corp nil)
      ;; at Successful Run stage -- click Eden Shard to install
      (play-from-hand state :runner "Eden Shard")
      (is (= 5 (:credit (get-runner))) "Eden Shard install was free")
      (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
      (is (= "Identity" (-> (get-runner) :prompt first :card :type)) "Fisk prompt showing")
      (click-prompt state :runner "Yes")
      (is (not (:run @state)) "Run ended")
      (is (= 6 (count (:hand (get-corp)))) "Corp forced to draw"))))
