(ns game-test.cards.operations.lag-time
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest lag-time
  (do-game
    (new-game {:corp {:deck ["Lag Time" "Vanilla" "Lotus Field"]}})
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Lotus Field" "R&D")
    (play-from-hand state :corp "Lag Time")
    (core/rez state :corp (get-ice state :hq 0))
    (core/rez state :corp (get-ice state :rd 0))
    (is (= 1 (:current-strength (get-ice state :hq 0))) "Vanilla at 1 strength")
    (is (= 5 (:current-strength (get-ice state :rd 0))) "Lotus Field at 5 strength")))
