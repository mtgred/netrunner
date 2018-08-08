(ns game-test.cards.resources.miss-bones
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest miss-bones
  ;; Miss Bones - credits for trashing installed cards, trash when empty
  (do-game
    (new-game {:runner {:deck ["Miss Bones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Miss Bones")
    (let [mb (get-resource state 0)]
      (is (= 12 (get-counters (refresh mb) :credit)) "Miss Bones starts with 12 credits")
      (is (= 3 (:credit (get-runner))) "Runner starts with 3 credits")
      (card-ability state :runner mb 0)
      (is (= 11 (get-counters (refresh mb) :credit)) "Miss Bones loses a credit")
      (is (= 4 (:credit (get-runner))) "Runner gains a credit")
      (dotimes [_ 11]
        (card-ability state :runner mb 0))
      (is (= 1 (count (:discard (get-runner)))) "Miss Bones in discard pile")
      (is (empty? (get-resource state)) "Miss Bones not installed")
      (is (= 15 (:credit (get-runner))) "Runner gained all 12 credits from Miss Bones"))))
