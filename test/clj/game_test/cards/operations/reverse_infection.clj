(ns game-test.cards.operations.reverse-infection
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reverse-infection
  ;; Reverse Infection - purge and trash 1 card from stack for every 3 counters purged - or gain 2 credits
  (do-game
    (new-game {:corp {:deck [(qty "Reverse Infection" 2)]}
               :runner {:deck ["Virus Breeding Ground" "Datasucker" (qty "Sure Gamble" 3)]}})
    (starting-hand state :runner ["Virus Breeding Ground" "Datasucker"])
    (play-from-hand state :corp "Reverse Infection")
    (click-prompt state :corp "Gain 2 [Credits]")
    (is (= 7 (:credit (get-corp))) "Corp gained 2 credits")
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Datasucker")
    (take-credits state :runner)
    (core/add-counter state :runner (get-resource state 0) :virus 4)
    (core/add-counter state :runner (get-program state 0) :virus 3)
    (play-from-hand state :corp "Reverse Infection")
    (click-prompt state :corp "Purge virus counters.")
    (is (= 9 (:credit (get-corp))) "Corp did not gain credits")
    (is (zero? (get-counters (get-resource state 0) :virus)) "Viruses purged from VBG")
    (is (zero? (get-counters (get-program state 0) :virus)) "Viruses purged from Datasucker")
    (is (= 2 (count (:discard (get-runner)))) "Two cards trashed from stack")))
