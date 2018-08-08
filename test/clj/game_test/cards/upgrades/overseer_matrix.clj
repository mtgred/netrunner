(ns game-test.cards.upgrades.overseer-matrix
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest overseer-matrix
  ;; Overseer Matrix - corp takes a tag when trashing a card in this server
  (testing "Basic functionality"
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" "Red Herrings"]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp om)
        (run-successful state)
        (is (zero? (:tag (get-runner))) "Runner starts with no tags")
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 1 (:tag (get-runner))) "Runner takes a tag")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 2 (:tag (get-runner))) "Runner takes a tag"))))
  (testing "Effect persists after trash"
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" (qty "Red Herrings" 3)]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp om)
        (run-successful state)
        (is (zero? (:tag (get-runner))) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 1 (:tag (get-runner))) "Runner takes a tag")
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 2 (:tag (get-runner))) "Runner takes a tag"))))
  (testing "Effect ends after current run"
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" (qty "Red Herrings" 3)]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp om)
        (run-successful state)
        (is (zero? (:tag (get-runner))) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 1 (:tag (get-runner))) "Runner takes a tag")
        (click-card state :runner rh)
        (click-prompt state :runner "No action")
        (is (= 1 (:tag (get-runner))) "Runner doesn't take a tag")
        (run-on state "Server 1")
        (run-successful state)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (is (empty? (:prompt (get-corp))) "No prompt for Overseer Matrix")
        (is (= 1 (:tag (get-runner))) "Runner doesn't take a tag")))))
