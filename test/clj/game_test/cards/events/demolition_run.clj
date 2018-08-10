(ns game-test.cards.events.demolition-run
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest demolition-run
  ;; Demolition Run - Trash at no cost
  (do-game
    (new-game {:corp {:deck ["False Lead"
                             "Shell Corporation"
                             (qty "Hedge Fund" 3)]}
               :runner {:deck ["Demolition Run"]}})
    (core/move state :corp (find-card "False Lead" (:hand (get-corp))) :deck) ; put False Lead back in R&D
    (play-from-hand state :corp "Shell Corporation" "R&D") ; install upgrade with a trash cost in root of R&D
    (take-credits state :corp 2) ; pass to runner's turn by taking credits
    (play-from-hand state :runner "Demolition Run")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits for the event")
    (click-prompt state :runner "R&D")
    (is (= [:rd] (get-in @state [:run :server])) "Run initiated on R&D")
    (run-successful state)
    (click-prompt state :runner "Unrezzed upgrade in R&D")
    (click-prompt state :runner "[Demolition Run]: Trash card")
    (is (= 3 (:credit (get-runner))) "Trashed Shell Corporation at no cost")
    (click-prompt state :runner "Card from deck")
    (click-prompt state :runner "[Demolition Run]: Trash card")
    (is (zero? (:agenda-point (get-runner))) "Didn't steal False Lead")
    (is (= 2 (count (:discard (get-corp)))) "2 cards in Archives")
    (is (empty? (:prompt (get-runner))) "Run concluded")))
