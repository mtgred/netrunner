(ns game-test.cards.resources.stim-dealer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest stim-dealer
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game {:runner {:deck ["Stim Dealer" "Sure Gamble" "Feedback Filter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "Stim Dealer")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [sd (get-resource state 0)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (get-counters (refresh sd) :power)) "Lost all counters")
      (is (empty? (:prompt (get-runner))) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
      (is (= 4 (:click (get-runner))) "Didn't gain extra click"))))
