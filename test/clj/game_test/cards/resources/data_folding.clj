(ns game-test.cards.resources.data-folding
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest data-folding
  ;; Data Folding - Gain 1c at start of turn if 2+ unused MU
  (do-game
    (new-game {:runner {:deck ["Data Folding" "Hyperdriver"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Data Folding")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (core/available-mu state)) "At least 2 unused MU")
    (is (= 6 (:credit (get-runner))) "Gained 1c at turn start")
    (play-from-hand state :runner "Hyperdriver")
    (take-credits state :runner)
    (is (= 1 (core/available-mu state)) "Only 1 unused MU")
    (is (= 8 (:credit (get-runner))))
    (take-credits state :corp)
    (is (= 8 (:credit (get-runner))) "No credits gained at turn start")))
