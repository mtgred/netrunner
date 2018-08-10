(ns game-test.cards.operations.self-growth-program
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest self-growth-program
  ;; Self-Growth Program - Add 2 installed cards to grip if runner is tagged
  (do-game
    (new-game {:corp {:deck ["Self-Growth Program"]}
               :runner {:deck ["Clone Chip" "Inti"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Inti")
    (take-credits state :runner)
    (play-from-hand state :corp "Self-Growth Program")
    (is (= 3 (:click (get-corp))) "Self-Growth Program precondition not met; card not played")
    (core/gain state :runner :tag 1)
    (is (zero? (count (:hand (get-runner)))) "Runner hand is empty")
    (let [inti (get-program state 0)
          cc (get-hardware state 0)]
      (play-from-hand state :corp "Self-Growth Program")
      (click-card state :corp inti)
      (click-card state :corp cc))
    (is (= 2 (count (:hand (get-runner)))) "2 cards returned to hand")
    (is (zero? (count (get-program state))) "No programs installed")
    (is (zero? (count (get-hardware state))) "No hardware installed")))
