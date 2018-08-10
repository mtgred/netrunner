(ns game-test.cards.ice.meru-mati
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest meru-mati
  (do-game
    (new-game {:corp {:deck [(qty "Meru Mati" 2)]}})
    (play-from-hand state :corp "Meru Mati" "HQ")
    (play-from-hand state :corp "Meru Mati" "R&D")
    (core/rez state :corp (get-ice state :hq 0))
    (core/rez state :corp (get-ice state :rd 0))
    (is (= 4 (:current-strength (get-ice state :hq 0))) "HQ Meru Mati at 4 strength")
    (is (= 1 (:current-strength (get-ice state :rd 0))) "R&D at 0 strength")))
