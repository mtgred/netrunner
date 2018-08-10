(ns game-test.cards.hardware.patchwork
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest patchwork
  ;; Patchwork
  (testing "Play event"
    (do-game
      (new-game {:runner {:deck ["Patchwork" (qty "Sure Gamble" 2) "Easy Mark"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (card-ability state :runner (get-hardware state 0) 0)
      (play-from-hand state :runner "Sure Gamble")
      (is (= 5 (:credit (get-runner))) "Runner has not been charged credits yet")
      (is (empty? (:discard (get-runner))) "Sure Gamble is not in heap yet")
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 11 (:credit (get-runner))) "Runner was only charge 3 credits to play Sure Gamble")
      (is (= 2 (count (:discard (get-runner)))) "2 cards now in heap")
      (play-from-hand state :runner "Sure Gamble")
      (is (= 15 (:credit (get-runner))) "Patchwork is once-per-turn")))
  (testing "Install a card"
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Easy Mark" "Cyberfeeder"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (card-ability state :runner (get-hardware state 0) 0)
      (play-from-hand state :runner "Cyberfeeder")
      (is (= 5 (:credit (get-runner))) "Runner has not been charged credits yet")
      (is (empty? (:discard (get-runner))) "Cyberfeeder is not in heap yet")
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 5 (:credit (get-runner))) "Runner was charged 0 credits to play Cyberfeeder"))))
