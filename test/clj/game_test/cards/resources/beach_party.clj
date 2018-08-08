(ns game-test.cards.resources.beach-party
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest beach-party
  ;; Beach Party - Lose 1 click when turn begins; hand size increased by 5
  (do-game
    (new-game {:runner {:deck ["Beach Party"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Beach Party")
    (is (= 10 (core/hand-size state :runner)) "Max hand size increased by 5")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))) "Lost 1 click at turn start")))
