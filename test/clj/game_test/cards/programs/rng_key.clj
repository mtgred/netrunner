(ns game-test.cards.programs.rng-key
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rng-key
  ;; RNG Key - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
  (testing "Basic behaviour - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 5) "Hedge Fund"]}
                 :runner {:deck ["RNG Key" (qty "Paperclip" 2)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (starting-hand state :runner ["RNG Key"])
      (take-credits state :corp)
      (testing "Gain 3 credits"
        (play-from-hand state :runner "RNG Key")
        (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "5")
        (click-prompt state :runner "Gain 3 [Credits]")
        (is (= 8 (:credit (get-runner))) "Gained 3 credits")
        (click-prompt state :runner "No action"))
      (testing "Do not trigger on second successful run"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (take-credits state :corp))
      (testing "Do not trigger on archives"
        (run-on state "Archives")
        (run-successful state))
      (testing "Do not get choice if trigger declined"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "No")
        (click-prompt state :runner "No action"))
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Do not gain credits / cards if guess incorrect"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner "No action"))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Gain 2 cards"
        (is (zero? (count (:hand (get-runner)))) "Started with 0 cards")
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "3")
        (click-prompt state :runner "Draw 2 cards")
        (click-prompt state :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Gained 2 cards")
        (is (zero? (count (:deck (get-runner)))) "Cards came from stack"))))
  (testing "Do not pay out if accessing an upgrade first -- regression test for #3150"
    (do-game
      (new-game {:corp {:deck ["Hokusai Grid" "Hedge Fund"]}
                 :runner {:deck ["RNG Key"]}})
      (play-from-hand state :corp "Hokusai Grid" "HQ")
      (take-credits state :corp)
      (testing "Gain 3 credits"
        (play-from-hand state :runner "RNG Key")
        (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner "Unrezzed upgrade in HQ")
        (is (= "You accessed Hokusai Grid." (-> (get-runner) :prompt first :msg))
            "No RNG Key prompt, straight to access prompt")
        (is (= 5 (:credit (get-runner))) "Gained no credits")))))
