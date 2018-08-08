(ns game-test.cards.operations.election-day
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest election-day
  (do-game
    (new-game {:corp {:deck [(qty "Election Day" 7)]}})
    (is (= 6 (count (:hand (get-corp)))) "Corp starts with 5 + 1 cards")
    (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Election Day")
    (is (= 1 (count (:hand (get-corp)))) "Could not play Election Day")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-corp)))) "Corp has now 1 + 1 cards before Election Day")
    (play-from-hand state :corp "Election Day")
    (is (= 5 (count (:hand (get-corp)))) "Corp has now 5 cards due to Election Day")))
