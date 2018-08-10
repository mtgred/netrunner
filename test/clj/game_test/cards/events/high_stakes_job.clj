(ns game-test.cards.events.high-stakes-job
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest high-stakes-job
  ;; High Stakes Job - run on server with at least 1 piece of unrezzed ice, gains 12 credits if successful
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["High-Stakes Job"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 1)
    (is (= 6 (:credit (get-runner))) "Runner starts with 6 credits")
    (play-from-hand state :runner "High-Stakes Job")
    (click-prompt state :runner "HQ")
    (run-successful state)
    (is (= 12 (:credit (get-runner))) "Runner gains 12 credits")))
