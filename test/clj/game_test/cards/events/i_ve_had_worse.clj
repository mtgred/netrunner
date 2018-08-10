(ns game-test.cards.events.i-ve-had-worse
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest i-ve-had-worse
  ;; I've Had Worse - Draw 3 cards when lost to net/meat damage; don't trigger if flatlined
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Scorched Earth" 3) (qty "Pup" 3)]}
                 :runner {:deck [(qty "I've Had Worse" 2) (qty "Sure Gamble" 3) (qty "Imp" 2)]}})
      (core/gain state :runner :tag 1)
      (core/gain state :corp :credit 5)
      (starting-hand state :runner ["I've Had Worse"])
      (play-from-hand state :corp "Pup" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (card-subroutine state :corp (get-ice state :hq 0) 0)
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 3 (count (:hand (get-runner)))) "I've Had Worse triggered and drew 3 cards")
      (starting-hand state :runner ["I've Had Worse" "Imp" "Imp"])
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")
      (is (= 4 (count (:discard (get-runner)))) "All 3 cards in Grip trashed by Scorched Earth")
      (is (= 3 (count (:deck (get-runner)))) "No cards drawn from I've Had Worse")))
  (testing "Will save you if you apocalypse away a lot of cards vs Hostile Infrastructure"
    (do-game
      (new-game {:corp {:deck ["Hostile Infrastructure" (qty "Ice Wall" 2)]}
                 :runner {:deck [(qty "I've Had Worse" 3) (qty "Sure Gamble" 3) (qty "Apocalypse" 2)]}})
      (starting-hand state :runner ["I've Had Worse" "Apocalypse"])
      (starting-hand state :corp ["Hostile Infrastructure" "Ice Wall" "Ice Wall"])
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (play-from-hand state :runner "Apocalypse")
      (is (not (= "Flatline" (:reason @state))) "Win condition does not report flatline"))))
