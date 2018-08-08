(ns game-test.cards.upgrades.underway-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest underway-grid
  ;; Underway Grid - prevent expose of cards in server
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"
                             "Underway Grid"]}
               :runner {:deck ["Drive By"]}})
    (play-from-hand state :corp "Underway Grid" "New remote")
    (play-from-hand state :corp "Eve Campaign" "Server 1")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :remote1 0))
    (let [eve1 (get-content state :remote1 1)]
      (play-from-hand state :runner "Drive By")
      (click-card state :runner eve1)
      (is (empty? (:discard (get-corp))) "Expose and trash prevented"))))
