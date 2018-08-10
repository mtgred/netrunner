(ns game-test.cards.agendas.hollywood-renovation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hollywood-renovation
  ;; Hollywood Renovation
  (do-game
    (new-game {:corp {:deck ["Hollywood Renovation" "Ice Wall"]}})
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hollywood Renovation" "New remote")
    (let [hr (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh hr) :advancement)) "Hollywood Renovation should start with 0 advancement tokens")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (dotimes [n 5]
        (advance state (refresh hr))
        (click-card state :corp (refresh iw)))
      (is (= 5 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 5 advancement tokens")
      (is (= 5 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 5 advancement tokens")
      (advance state (refresh hr))
      (click-card state :corp (refresh iw))
      (is (= 6 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 1 from 5 to 6 advancement tokens")
      (is (= 7 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 from 5 to 7 advancement tokens"))))
