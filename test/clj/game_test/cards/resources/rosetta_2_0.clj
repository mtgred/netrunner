(ns game-test.cards.resources.rosetta-2-0
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rosetta-2-0
  ;; Rosetta 2.0 remove an installed program from the game and install one from the heap lower install cost
  (do-game
    (new-game {:runner {:deck ["Rosetta 2.0" "Corroder" "Gordian Blade"]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Rosetta 2.0" "Corroder"])
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Rosetta 2.0")
    (play-from-hand state :runner "Corroder")
    (is (= 3 (core/available-mu state)) "Corrder cost 1 mu")
    (is (= 2 (:credit (get-runner))) "Starting with 2 credits")
    (card-ability state :runner (get-resource state 0) 0)
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner (find-card "Gordian Blade" (:deck (get-runner))))
    (is (= 3 (core/available-mu state)) "Gordian cost 1 mu, Corroder freed")
    (is (zero? (:credit (get-runner))) "Ending with 0 credits")
    (is (= 1 (count (:rfg (get-runner)))) "Corroder removed from game")
    (is (= 1 (count (get-program state))) "One program installed")
    (is (= "Gordian Blade" (:title (get-program state 0))) "Gordian installed")))
