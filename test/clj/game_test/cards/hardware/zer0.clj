(ns game-test.cards.hardware.zer0
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest zer0
  ;; Zer0
  (testing "Basic ability"
    (do-game
      (new-game {:runner {:deck ["Zer0" "Corroder" (qty "Sure Gamble" 2)]}})
      (starting-hand state :runner ["Zer0" "Corroder"])
      (take-credits state :corp)
      (play-from-hand state :runner "Zer0")
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (let  [z (get-hardware state 0)]
        (card-ability state :runner z 0)
        (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
        (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards")
        (is (find-card "Corroder" (:discard (get-runner))) "Corroder is in heap"))))
  (testing "With Titanium Ribs"
    (do-game
      (new-game {:runner {:deck ["Zer0" "Titanium Ribs" (qty "Sure Gamble" 5)]}})
      (starting-hand state :runner ["Zer0" "Titanium Ribs" "Sure Gamble" "Sure Gamble" "Sure Gamble"])
      (take-credits state :corp)
      (play-from-hand state :runner "Zer0")
      (play-from-hand state :runner "Titanium Ribs")
      (click-card state :runner (first (:hand (get-runner))))
      (click-card state :runner (second (:hand (get-runner))))
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let  [z (get-hardware state 0)]
        (card-ability state :runner z 0)
        (is (= 3 (:credit (get-runner))) "Zer0 has not yet resolved because Ribs prompt is open")
        (is (= 1 (count (:hand (get-runner)))) "Zer0 has not yet resolved because Ribs prompt is open")
        (click-card state :runner (first (:hand (get-runner))))
        (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
        (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards"))))
  (testing "With Respirocytes"
    (do-game
      (new-game {:runner {:deck ["Zer0" "Titanium Ribs" "Respirocytes"(qty "Sure Gamble" 7)]}})
      (starting-hand state :runner ["Zer0" "Titanium Ribs" "Respirocytes" "Sure Gamble" "Sure Gamble" "Sure Gamble" "Sure Gamble"])
      (take-credits state :corp)
      (play-from-hand state :runner "Zer0")
      (play-from-hand state :runner "Titanium Ribs")
      (click-card state :runner (second (:hand (get-runner))))
      (click-card state :runner (nth (:hand (get-runner)) 2))
      (play-from-hand state :runner "Respirocytes")
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      ;; Now 1 Gamble in hand
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let  [z (get-hardware state 0)]
        (card-ability state :runner z 0)
        (is (= 3 (:credit (get-runner))) "Zer0 has not yet resolved because Ribs prompt is open")
        (is (= 1 (count (:hand (get-runner)))) "Zer0 has not yet resolved because Ribs prompt is open")
        (click-card state :runner (first (:hand (get-runner))))
        (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
        (is (= 3 (count (:hand (get-runner)))) "Runner has 3 cards")))))
