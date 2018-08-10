(ns game-test.cards.events.account-siphon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest account-siphon
  ;; Account Siphon
  (testing "Use ability"
    (do-game
      (new-game {:runner {:deck [(qty "Account Siphon" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Account Siphon, use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :runner "Replacement effect")
      (is (= 2 (:tag (get-runner))) "Runner took 2 tags")
      (is (= 15 (:credit (get-runner))) "Runner gained 10 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))
  (testing "Access"
    (do-game
      (new-game {:runner {:deck [(qty "Account Siphon" 3)]}})
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play another Siphon, do not use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :runner "Access cards")
      (is (zero? (:tag (get-runner))) "Runner did not take any tags")
      (is (= 5 (:credit (get-runner))) "Runner did not gain any credits")
      (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")))
  (testing "New Angeles City Hall interaction"
    ;; Account Siphon - Access
    (do-game
      (new-game {:runner {:deck ["Account Siphon"
                                 "New Angeles City Hall"]}})
      (core/gain state :corp :bad-publicity 1)
      (is (= 1 (:bad-publicity (get-corp))) "Corp has 1 bad publicity")
      (core/lose state :runner :credit 1)
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      (play-from-hand state :runner "New Angeles City Hall")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let [nach (get-resource state 0)]
        (play-run-event state (first (get-in @state [:runner :hand])) :hq)
        (click-prompt state :runner "Replacement effect")
        (is (= 4 (:credit (get-runner))) "Runner still has 4 credits due to BP")
        (card-ability state :runner nach 0)
        (is (= 2 (:credit (get-runner))) "Runner has 2 credits left")
        (card-ability state :runner nach 0)
        (is (zero? (:credit (get-runner))) "Runner has no credits left")
        (click-prompt state :runner "Done"))
      (is (zero? (:tag (get-runner))) "Runner did not take any tags")
      (is (= 10 (:credit (get-runner))) "Runner gained 10 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits"))))
