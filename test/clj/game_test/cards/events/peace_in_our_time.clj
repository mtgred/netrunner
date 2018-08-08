(ns game-test.cards.events.peace-in-our-time
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest peace-in-our-time
  ;; Peace in Our Time - runner gains 10, corp gains 5. No runs allowed during turn.
  (do-game
    (new-game {:runner {:deck ["Peace in Our Time"]}})
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-from-hand state :runner "Peace in Our Time")
    (is (= 13 (:credit (get-corp))) "Corp gains 5 credits")
    (is (= 14 (:credit (get-runner))) "Runner gains 10 credits")
    (run-on state "HQ")
    (is (not (:run @state)) "Not allowed to make a run")))
