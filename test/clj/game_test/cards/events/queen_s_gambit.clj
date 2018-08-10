(ns game-test.cards.events.queen-s-gambit
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest queen-s-gambit
  ;; Check that Queen's Gambit prevents access of card #1542
  (do-game
    (new-game {:corp {:deck [(qty "PAD Campaign" 2)]}
               :runner {:deck ["Queen's Gambit"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Queen's Gambit")
    (let [pad (get-content state :remote1 0)
          runner-creds (:credit (get-runner))]
      (click-prompt state :runner "3")
      (click-card state :runner pad)
      (is (= (+ runner-creds 6) (:credit (get-runner))) "Gained 6 credits from Queen's Gambit")
      (is (= 3 (get-counters (refresh pad) :advancement)) "3 advancement counters placed on PAD Campaign by Queen's Gambit")
      (is (not (core/can-access? state :runner (refresh pad))) "Cannot access PAD Campgain")
      (run-empty-server state "Server 1")
      (is (not (:run @state)) "Run ended since no cards could be accessed"))
    (let [other-pad (get-content state :remote2 0)]
      (is (core/can-access? state :runner other-pad)) "Not prevented from accessing other cards")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [pad (get-content state :remote1 0)
          runner-creds (:credit (get-runner))]
      (run-empty-server state "Server 1")
      (is (core/can-access? state :runner (refresh pad)) "Can access PAD Campgain next turn")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (is (= (- runner-creds 4) (:credit (get-runner))) "Paid 4 credits to trash PAD Campaign"))))
