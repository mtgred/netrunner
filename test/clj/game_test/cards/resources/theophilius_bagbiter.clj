(ns game-test.cards.resources.theophilius-bagbiter
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest theophilius-bagbiter
  ;; Theophilius Bagbiter - hand size is equal to credit pool
  (do-game
    (new-game {:runner {:deck ["Theophilius Bagbiter"]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Runner starts with 5c")
    (play-from-hand state :runner "Theophilius Bagbiter")
    (is (zero? (:credit (get-runner))) "Runner loses all credits on install")
    (is (= 1 (count (get-resource state))) "Theophilius Bagbiter installed")
    (is (zero? (core/hand-size state :runner)) "Max hand size is 0")
    (core/gain state :runner :credit 7)
    (is (= 7 (:credit (get-runner))) "Runner has 7c")
    (is (= 7 (core/hand-size state :runner)) "Max hand size is 7")
    (core/trash-resource state :runner nil)
    (click-card state :runner (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))) "Theo is trashed")
    (is (empty? (get-resource state)) "No resources installed")
    (is (= 5 (core/hand-size state :runner)) "Max hand size is reset to default")))
