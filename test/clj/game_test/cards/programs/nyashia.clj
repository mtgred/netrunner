(ns game-test.cards.programs.nyashia
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nyashia
  ;; Nyashia
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
               :runner {:deck ["Nyashia"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Nyashia")
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :runner "Yes")
    (is (= 2 (+ (get-in @state [:runner :rd-access]) (:access-bonus (:run @state) 0))))))
