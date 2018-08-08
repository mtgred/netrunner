(ns game-test.cards.events.diversion-of-funds
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest diversion-of-funds
  ;; Diversion of Funds
  (testing "Use ability"
    (do-game
      (new-game {:runner {:deck [(qty "Diversion of Funds" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Diversion of Funds, use ability
      (play-from-hand state :runner "Diversion of Funds")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= 9 (:credit (get-runner))) "Runner netted 4 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")
      (is (not (:run @state)) "Run finished")))
  (testing "Access"
    (do-game
      (new-game {:runner {:deck [(qty "Diversion of Funds" 3)]}})
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Diversion, do not use ability
      (play-from-hand state :runner "Diversion of Funds")
      (run-successful state)
      (click-prompt state :runner "Access cards")
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "Prompt is closed")
      (is (= 4 (:credit (get-runner))) "Runner is down a credit")
      (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")
      (is (not (:run @state)) "Run finished"))))
