(ns game-test.cards.agendas.award-bait
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest award-bait
  ;; Award Bait
  (do-game
    (new-game {:corp {:deck [(qty "Award Bait" 2) "Ice Wall"]}})
    (core/move state :corp (find-card "Award Bait" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (play-from-hand state :corp "Award Bait" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :corp "2")
      (click-card state :corp "Ice Wall")
      (click-prompt state :runner "Steal")
      (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens")
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :corp "2")
      (click-card state :corp (refresh iw))
      (click-prompt state :runner "Steal")
      (is (= 4 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens"))))
