(ns game-test.cards.agendas.superior-cyberwalls
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest superior-cyberwalls
  ;; Superior Cyberwalls
  (do-game
    (new-game {:corp {:deck ["Superior Cyberwalls" "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (core/rez state :corp iw)
      (is (= 1 (:current-strength (refresh iw))) "Should start with base strength of 1")
      (is (= 4 (:credit (get-corp))) "Should have 4 credits after rez")
      (play-and-score state "Superior Cyberwalls")
      (is (= 2 (:current-strength (refresh iw))) "Should gain 1 strength from 1 to 2")
      (is (= 5 (:credit (get-corp))) "Should gain 1 credit for rezzed barrier"))))
