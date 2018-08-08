(ns game-test.cards.programs.surfer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest surfer
  ;; Surfer - Swap position with ice before or after when encountering a Barrier ICE
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Quandary"]}
               :runner {:deck ["Surfer"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Surfer")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits to install Surfer")
    (core/rez state :corp (get-ice state :hq 1))
    (run-on state "HQ")
    (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
    (let [surf (get-program state 0)]
      (card-ability state :runner surf 0)
      (click-card state :runner (get-ice state :hq 0))
      (is (= 1 (:credit (get-runner))) "Paid 2 credits to use Surfer")
      (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall now at position 1"))))
