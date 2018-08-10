(ns game-test.cards.operations.successful-demonstration
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest successful-demonstration
  ;; Successful Demonstration - Play if only Runner made unsuccessful run last turn; gain 7 credits
  (do-game
    (new-game {:corp {:deck ["Successful Demonstration"]}})
    (play-from-hand state :corp "Successful Demonstration")
    (is (and (= 3 (:click (get-corp)))
             (= 5 (:credit (get-runner))))
        "Successful Demonstration precondition not met; card not played")
    (take-credits state :corp)
    (run-on state "R&D")
    (run-jack-out state)
    (take-credits state :runner)
    (play-from-hand state :corp "Successful Demonstration")
    (is (= 13 (:credit (get-corp))) "Paid 2 to play event; gained 7 credits")))
