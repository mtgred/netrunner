(ns game-test.cards.assets.breached-dome
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest breached-dome
  ;; Breached Dome
  (do-game
    (new-game {:corp {:deck [(qty "Breached Dome" 10)]}
               :runner {:deck [(qty "Sure Gamble" 10)]}})
    (trash-from-hand state :corp "Breached Dome")
    (play-from-hand state :corp "Breached Dome" "New remote")
    (take-credits state :corp)
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (is (= 4 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 4 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 2 (count (:discard (get-runner)))) "Runner's discard grew by 2")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "No action")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 3 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 4 (count (:discard (get-runner)))) "Runner's discard grew by 2")
    (run-empty-server state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 2 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 6 (count (:discard (get-runner)))) "Runner's discard grew by 2")))
