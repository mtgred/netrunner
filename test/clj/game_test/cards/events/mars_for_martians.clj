(ns game-test.cards.events.mars-for-martians
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mars-for-martians
  ;; Mars for Martians - Full test
  (do-game
    (new-game {:runner {:deck ["Mars for Martians" "Clan Vengeance" "Counter Surveillance"
                               "Jarogniew Mercs" (qty "Sure Gamble" 3)]}})
    (starting-hand state :runner ["Mars for Martians" "Clan Vengeance" "Counter Surveillance" "Jarogniew Mercs"])
    (take-credits state :corp)
    (play-from-hand state :runner "Clan Vengeance")
    (play-from-hand state :runner "Counter Surveillance")
    (play-from-hand state :runner "Jarogniew Mercs")
    (play-from-hand state :runner "Mars for Martians")
    (is (= 1 (:click (get-runner))) "Mars for Martians not played, priority event")
    (take-credits state :runner)
    (take-credits state :corp)
    (core/gain state :runner :tag 4)
    (is (= 5 (:tag (get-runner))) "+1 tag from Jarogniew Mercs")
    (is (= 1 (count (:hand (get-runner)))))
    (is (= 2 (:credit (get-runner))))
    (play-from-hand state :runner "Mars for Martians")
    (is (= 3 (count (:hand (get-runner)))) "3 clan resources, +3 cards but -1 for playing Mars for Martians")
    (is (= 7 (:credit (get-runner))) "5 tags, +5 credits")))
