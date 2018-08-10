(ns game-test.cards.assets.plan-b
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest plan-b
  ;; Plan B - score agenda with adv cost <= # of adv counters
  (do-game
    (new-game {:corp {:deck ["Plan B"
                             "Braintrust"
                             "The Future Perfect"
                             "Mushin No Shin"]}})
    (play-from-hand state :corp "Mushin No Shin")
    (click-card state :corp (find-card "Plan B" (:hand (get-corp))))
    (take-credits state :corp)
    (run-empty-server state :remote1)
    ;; prompt for corp to use Plan B
    (click-prompt state :corp "Yes")
    ;; Pick TFP, does not score
    (click-card state :corp (find-card "The Future Perfect" (:hand (get-corp))))
    (is (find-card "The Future Perfect" (:hand (get-corp))) "TFP is not scored")
    ;; Pick Brain Trust, scores
    (click-card state :corp (find-card "Braintrust" (:hand (get-corp))))
    (is (find-card "Braintrust" (:scored (get-corp))) "Braintrust is scored")))
