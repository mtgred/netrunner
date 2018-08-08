(ns game-test.cards.resources.lewi-guilherme
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest lewi-guilherme
  ;; Lewi Guilherme - lower corp hand size by 1, pay 1 credit when turn begins or trash
  (do-game
    (new-game {:runner {:deck [(qty "Lewi Guilherme" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Lewi Guilherme")
    (is (= -1 (get-in (get-corp) [:hand-size :mod])) "Corp hand size reduced by 1")
    (take-credits state :runner)
    (core/lose state :runner :credit 6)
    (is (= 2 (:credit (get-runner))) "Credits are 2")
    (take-credits state :corp)
    (click-prompt state :runner "Yes")
    (is (= 1 (:credit (get-runner))) "Lost a credit from Lewi")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "No")
    (is (= 1 (count (:discard (get-runner)))) "First Lewi trashed")
    (is (zero? (get-in (get-corp) [:hand-size :mod])) "Corp hand size normal again")
    (play-from-hand state :runner "Lewi Guilherme")
    (take-credits state :runner)
    (core/lose state :runner :credit 8)
    (is (zero? (:credit (get-runner))) "Credits are 0")
    (take-credits state :corp)
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:discard (get-runner)))) "Second Lewi trashed due to no credits")))
