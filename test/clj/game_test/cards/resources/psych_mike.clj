(ns game-test.cards.resources.psych-mike
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest psych-mike
  ;; Psych Mike
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 100)]}
                 :runner {:deck ["Psych Mike" "Deep Data Mining"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Psych Mike")
      (let [credits (:credit (get-runner))]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= (inc credits) (:credit (get-runner))) "Psych Mike should give 1 credit for accessing 1 card"))
      (let [credits (:credit (get-runner))]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= credits (:credit (get-runner))) "Psych Mike should give 0 credits for second run of the turn"))
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Deep Data Mining")
      (let [credits (:credit (get-runner))]
        (run-successful state)
        (dotimes [_ 5]
          (click-prompt state :runner "Card from deck")
          (click-prompt state :runner "No action"))
        (is (= (+ credits 5) (:credit (get-runner))) "Psych Mike should give 5 credits for DDM accesses"))))
  (testing "vs upgrades"
    (do-game
      (new-game {:corp {:deck ["Bryan Stinson" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Psych Mike"]}})
      (starting-hand state :corp ["Bryan Stinson"])
      (play-from-hand state :corp "Bryan Stinson" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Psych Mike")
      (let [credits (:credit (get-runner))]
        (run-empty-server state "R&D")
        (click-prompt state :runner "Card from deck")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Unrezzed upgrade in R&D")
        (click-prompt state :runner "No action")
        (is (= (inc credits) (:credit (get-runner))) "Psych Mike should give 1 credit for accessing 1 card")))))
