(ns game-test.cards.operations.foxfire
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest foxfire
  ;; Foxfire
  (do-game
    (new-game {:corp {:deck [(qty "Foxfire" 2)]}
               :runner {:deck ["Dyson Mem Chip" "Ice Carver"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Dyson Mem Chip")
    (play-from-hand state :runner "Ice Carver")
    (take-credits state :runner)
    (play-from-hand state :corp "Foxfire")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-hardware state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Corp should trash Dyson Mem Chip from winning Foxfire trace")
    (play-from-hand state :corp "Foxfire")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (is (= 2 (-> (get-runner) :discard count)) "Corp should trash Ice Carver from winning Foxfire trace")))
