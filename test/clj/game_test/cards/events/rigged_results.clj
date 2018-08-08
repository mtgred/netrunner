(ns game-test.cards.events.rigged-results
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rigged-results
  ;; Rigged Results - success and failure
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Rigged Results" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Rigged Results")
    (click-prompt state :runner "0")
    (click-prompt state :corp "0")
    (is (empty? (:prompt (get-runner))) "Rigged Results failed for runner")
    (is (empty? (:prompt (get-corp))) "Rigged Results failed for runner")
    (play-from-hand state :runner "Rigged Results")
    (click-prompt state :runner "2")
    (click-prompt state :corp "1")
    (click-card state :runner (get-ice state :hq 0))
    (is (= [:hq] (:server (:run @state))) "Runner is running on HQ")
    (is (= 3 (:credit (get-runner))) "Rigged results spends credits")))
