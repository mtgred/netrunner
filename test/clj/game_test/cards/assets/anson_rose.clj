(ns game-test.cards.assets.anson-rose
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest anson-rose
  ;; Anson Rose
  (do-game
    (new-game {:corp {:deck ["Anson Rose" "Ice Wall"]}})
    (play-from-hand state :corp "Anson Rose" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ar (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp (refresh ar))
      (is (zero? (get-counters (refresh ar) :advancement)) "Anson Rose should start with 0 advancement counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 1 (get-counters (refresh ar) :advancement)) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should still have 0 counters so far")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 2 (get-counters (refresh ar) :advancement)) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should still have 0 counters so far")
      (core/rez state :corp (refresh iw))
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "2")
      (is (zero? (get-counters (refresh ar) :advancement)) "Anson Rose should lose all advancement counters")
      (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement counter"))))
