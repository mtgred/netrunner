(ns game-test.cards.assets.student-loans
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest student-loans
  ;; Student Loans - costs Runner 2c extra to play event if already same one in discard
  (do-game
    (new-game {:corp {:deck ["Student Loans" (qty "Hedge Fund" 2)]}})
    (core/gain state :corp :credit 2)
    (play-from-hand state :corp "Student Loans" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 5 (:credit (get-corp))) "Corp has 5c")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 9 (:credit (get-corp))) "Corp has 9c - no penalty from Student Loans")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Corp has 13c - no penalty from Student Loans")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (is (= 9 (:credit (get-runner))) "1st Gamble played for 4c")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 11 (:credit (get-runner))) "2nd Gamble played for 2c")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 13 (:credit (get-runner))) "3rd Gamble played for 2c")))
