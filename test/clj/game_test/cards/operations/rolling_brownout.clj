(ns game-test.cards.operations.rolling-brownout
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rolling-brownout
  ;; Rolling Brownout - Increase cost of events/operations by 1, gain 1c on first Runner event of turn
  (do-game
    (new-game {:corp {:deck ["Rolling Brownout" "Beanstalk Royalties"
                             "Domestic Sleepers"]}
               :runner {:deck [(qty "Easy Mark" 3)]}})
    (play-from-hand state :corp "Rolling Brownout")
    (play-from-hand state :corp "Beanstalk Royalties")
    (is (= 5 (:credit (get-corp))) "Beanstalk netted only 2c")
    (play-from-hand state :corp "Domestic Sleepers" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Easy Mark")
    (is (= 7 (:credit (get-runner))) "Easy Mark netted only 2c")
    (is (= 6 (:credit (get-corp))) "Corp gained 1c from Brownout")
    (play-from-hand state :runner "Easy Mark")
    (is (= 6 (:credit (get-corp))) "No Corp credit gain from 2nd event")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner "Easy Mark")
    (is (= 12 (:credit (get-runner))) "Easy Mark netted 3c after Brownout trashed")))
