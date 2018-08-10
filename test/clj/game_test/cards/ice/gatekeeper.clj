(ns game-test.cards.ice.gatekeeper
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gatekeeper
  ;; Gatekeeper
  (do-game
    (new-game {:corp {:deck ["Gatekeeper" "Posted Bounty"
                             (qty "Hostile Takeover" 2) (qty "Ice Wall" 10)]}})
    ;; Set up
    (starting-hand state :corp ["Gatekeeper" "Ice Wall" "Ice Wall"
                                "Posted Bounty" "Hostile Takeover" "Hostile Takeover"])
    (trash-from-hand state :corp "Ice Wall")
    (trash-from-hand state :corp "Ice Wall")
    (trash-from-hand state :corp "Hostile Takeover")
    ;; Actual test
    (play-from-hand state :corp "Gatekeeper" "New remote")
    (take-credits state :corp)
    (let [gate (get-ice state :remote1 0)
          hand (-> (get-corp) :hand count)
          deck (-> (get-corp) :deck count)
          num-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))
          hostile (find-card "Hostile Takeover" (:hand (get-corp)))]
      (run-on state "Server 1")
      (core/rez state :corp gate)
      (is (= 6 (:current-strength (refresh gate))))
      (card-subroutine state :corp gate 0)
      (click-prompt state :corp "3")
      (is (= (+ 3 hand) (-> (get-corp) :hand count)) "Corp should draw 3 cards")
      (click-card state :corp hostile)
      (click-card state :corp (find-card "Hostile Takeover" (:discard (get-corp))))
      (click-card state :corp (find-card "Posted Bounty" (:hand (get-corp))))
      (is (= deck (-> (get-corp) :deck count)) "R&D should have same number of cards as start")
      (is (= (inc num-shuffles) (count (core/turn-events state :corp :corp-shuffle-deck)))
          "Corp should shuffle R&D")
      (is (core/in-deck? (core/find-latest state hostile)) "Hostile Takeover should be in deck now")
      (card-subroutine state :corp gate 1)
      (is (not (:run @state)) "Gatekeeper subroutine should end the run")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (:current-strength (refresh gate))) "Gatekeeper strength should be reset"))))
