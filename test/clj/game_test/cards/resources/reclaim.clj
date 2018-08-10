(ns game-test.cards.resources.reclaim
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reclaim
  ;; Reclaim - trash Reclaim, trash card from grip, install program, hardware, or virtual resource from heap
  (testing "Basic behavior"
    (do-game
      (new-game {:runner {:deck ["Reclaim" "Mimic" "Clone Chip"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Mimic" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Reclaim")
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner (find-card "Clone Chip" (:hand (get-runner))))
      (click-prompt state :runner (find-card "Mimic" (:discard (get-runner))))
      (is (= 1 (count (get-program state))) "1 Program installed")
      (is (= 2 (:credit (get-runner))) "Runner paid install cost")))
  (testing "No cards in hand"
    (do-game
      (new-game {:runner {:deck ["Reclaim"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (card-ability state :runner (get-resource state 0) 0)
      (is (empty? (:prompt (get-runner))) "No Reclaim prompt")))
  (testing "Can install trashed card"
    (do-game
      (new-game {:runner {:deck ["Reclaim" "Mimic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner (find-card "Mimic" (:hand (get-runner))))
      (click-prompt state :runner (find-card "Mimic" (:discard (get-runner))))
      (is (= 1 (count (get-program state))) "1 Program installed")
      (is (= 2 (:credit (get-runner))) "Runner paid install cost")))
  (testing "Can't afford to install card"
    (do-game
      (new-game {:runner {:deck ["Reclaim" "Alpha"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (card-ability state :runner (get-resource state 0) 0)
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner (find-card "Alpha" (:hand (get-runner))))
      (is (empty? (get-program state)) "Did not install program")
      (is (= 5 (:credit (get-runner))) "Runner did not spend credits"))))
