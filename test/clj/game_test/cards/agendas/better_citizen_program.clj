(ns game-test.cards.agendas.better-citizen-program
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest better-citizen-program
  ;; Better Citizen Program
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Better Citizen Program"]}
                 :runner {:deck [(qty "The Maker's Eye" 2)
                                 (qty "Wyrm" 2)]}})
      (play-and-score state "Better Citizen Program")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (is (zero? (:tag (get-runner))) "Runner starts with 0 tags")
      (play-from-hand state :runner "The Maker's Eye")
      (click-prompt state :corp "Yes")
      (is (= 1 (:tag (get-runner))) "Runner takes 1 tag for playing a Run event")
      (run-successful state)
      (play-from-hand state :runner "Wyrm")
      (is (empty? (-> (get-corp) :prompt)) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (= 1 (:tag (get-runner))) "Runner doesn't gain a tag from installing an icebreaker after playing a Run event")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Wyrm")
      (click-prompt state :corp "Yes")
      (is (= 2 (:tag (get-runner))) "Runner gains 1 tag for installing an Icebreaker")
      (play-from-hand state :runner "The Maker's Eye")
      (is (empty? (-> (get-corp) :prompt)) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (= 2 (:tag (get-runner))) "Runner doesn't gain a tag from playing a Run event after installing an Icebreaker")
      (run-successful state)))
  (testing "Should only trigger on Run events. #3619"
    (do-game
      (new-game {:corp {:deck ["Better Citizen Program"]}
                 :runner {:deck ["Mining Accident"]}})
      (play-and-score state "Better Citizen Program")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Mining Accident")
      (click-prompt state :corp "Pay 5 [Credits]")
      (is (empty? (-> (get-corp) :prompt)) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (zero? (:tag (get-runner))) "Runner should not gain a tag from playing a non-Run event"))))
