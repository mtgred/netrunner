(ns game-test.cards.resources.temujin-contract
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest temujin-contract
  ;; Temüjin Contract
  (testing "Multiple times in one turn. Issue #1952"
    (do-game
      (new-game {:runner {:id "Silhouette: Stealth Operative"
                          :deck ["Temüjin Contract"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "Archives")
      (run-empty-server state "Archives")
      (is (= 5 (:credit (get-runner))) "Gained 4cr")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "Temjin has 12 credits remaining"))))
