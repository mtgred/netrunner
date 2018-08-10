(ns game-test.cards.resources.aesop-s-pawnshop
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest aesop-s-pawnshop
  ;; Tests use cases for Aesop's Pawnshop
  (do-game
    (new-game {:runner {:deck ["Aesop's Pawnshop" "Cache"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Aesop's Pawnshop")
    (play-from-hand state :runner "Cache")
    (let [orig-credits (:credit (get-runner))
          ap (get-resource state 0)
          cache (get-program state 0)]
      (card-ability state :runner ap 0)
      (click-card state :runner cache)
      (card-ability state :runner ap 0)
      (click-card state :runner ap)
      (let [ap (get-resource state 0)
            cache (get-in @state [:runner :discard 0])]
        (is (= (+ 3 orig-credits) (:credit (get-runner))) "Should have only gained 3 credits")
        (is (not= cache nil) "Cache should be in Heap")
        (is (not= ap nil) "Aesops should still be installed")))))
