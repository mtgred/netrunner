(ns game-test.cards.programs.hyperdriver
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Hyperdriver"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hyperdriver")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (:runner-phase-12 @state) "Runner in Step 1.2")
      (let [hyp (get-program state 0)]
        (card-ability state :runner hyp 0)
        (core/end-phase-12 state :runner nil)
        (is (= 7 (:click (get-runner))) "Gained 3 clicks")
        (is (= 1 (count (:rfg (get-runner)))) "Hyperdriver removed from game"))))
  (testing "triggering a Dhegdeered Hyperdriver should not grant +3 MU"
    (do-game
      (new-game {:runner {:deck ["Hyperdriver" "Dhegdheer"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dhegdheer")
      (let [dheg (get-program state 0)]
        (card-ability state :runner dheg 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "0 MU used by Hyperdriver hosted on Dhegdheer")
        (is (= 2 (:click (get-runner))) "2 clicks used")
        (is (= 3 (:credit (get-runner))) "2 credits used")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (let [hyp (first (:hosted (refresh dheg)))]
          (card-ability state :runner hyp 0)
          (core/end-phase-12 state :runner nil)
          (is (= 7 (:click (get-runner))) "Used Hyperdriver")
          (is (= 4 (core/available-mu state)) "Still 0 MU used after Hyperdriver removed from game"))))))
