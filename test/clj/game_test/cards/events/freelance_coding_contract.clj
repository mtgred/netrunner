(ns game-test.cards.events.freelance-coding-contract
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest freelance-coding-contract
  ;; Freelance Coding Contract - Gain 2 credits per program trashed from Grip
  (do-game
    (new-game {:runner {:deck ["Freelance Coding Contract"
                               "Paricia" "Cloak" "Inti"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Freelance Coding Contract")
    (click-card state :runner (find-card "Cloak" (:hand (get-runner))))
    (click-card state :runner (find-card "Paricia" (:hand (get-runner))))
    (click-card state :runner (find-card "Inti" (:hand (get-runner))))
    (click-prompt state :runner "Done")
    (is (= 3 (count (filter #(= (:type %) "Program") (:discard (get-runner)))))
        "3 programs in Heap")
    (is (= 11 (:credit (get-runner))) "Gained 6 credits from 3 trashed programs")))
