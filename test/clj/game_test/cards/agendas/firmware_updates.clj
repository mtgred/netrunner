(ns game-test.cards.agendas.firmware-updates
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest firmware-updates
  ;; Firmware Updates
  (do-game
    (new-game {:corp {:deck ["Firmware Updates"
                             "Ice Wall"]}})
    (play-and-score state "Firmware Updates")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [fu (get-scored state :corp 0)
          iw (get-ice state :hq 0)]
      (is (= 3 (get-counters (refresh fu) :agenda)) "Firmware Updates should start with 3 agenda counters")
      (core/rez state :corp iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (card-ability state :corp fu 0)
      (click-card state :corp (refresh iw))
      (is (= 2 (get-counters (refresh fu) :agenda)) "Firmware Updates should now have 2 agenda counters")
      (is (= 1 (get-counters (refresh iw) :advancement)) "Ice Wall should have 1 advancement token"))))
