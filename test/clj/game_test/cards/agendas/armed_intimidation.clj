(ns game-test.cards.agendas.armed-intimidation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest armed-intimidation
  ;; Armed Intimidation
  (do-game
    (new-game {:corp {:deck [(qty "Armed Intimidation" 2)]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 2)]}})
    (play-and-score state "Armed Intimidation")
    (click-prompt state :runner "Take 2 tags")
    (is (= 2 (:tag (get-runner))) "Runner took 2 tags from Armed Intimidation tag choice")
    (play-and-score state "Armed Intimidation")
    (is (= 5 (count (:hand (get-runner)))) "Runner has 5 cards before Armed Intimidation meat damage")
    (click-prompt state :runner "Suffer 5 meat damage")
    (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards after Armed Intimidation meat damage")))
