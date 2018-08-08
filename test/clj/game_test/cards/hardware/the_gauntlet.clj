(ns game-test.cards.hardware.the-gauntlet
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-gauntlet
  (testing "Access additional cards on run on HQ, not with Gang Sign. Issue #2749"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"
                               (qty "Hedge Fund" 3)]}
                 :runner {:deck ["The Gauntlet"
                                 "Gang Sign"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "The Gauntlet")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      ;; Gang Sign should trigger, without The Gauntlet pop-up
      (let [gs (get-resource state 0)]
        (prompt-is-card? state :runner gs))
      ;; This will throw error if The Gauntlet triggers.
      (click-prompt state :runner "Card from hand"))))
