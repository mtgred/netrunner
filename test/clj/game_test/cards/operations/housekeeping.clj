(ns game-test.cards.operations.housekeeping
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest housekeeping
  ;; Housekeeping - Runner must trash a card from Grip on first install of a turn
  (do-game
    (new-game {:corp {:deck ["Housekeeping"]}
               :runner {:deck [(qty "Cache" 2) "Fall Guy" "Mr. Li"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Fall Guy")
    (take-credits state :runner)
    (play-from-hand state :corp "Housekeeping")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (click-card state :runner (find-card "Mr. Li" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "Fall Guy prevention didn't trigger")
    (is (= 1 (count (:discard (get-runner)))) "Card trashed")
    (play-from-hand state :runner "Cache")
    (is (empty? (:prompt (get-runner))) "Housekeeping didn't trigger on 2nd install")))
