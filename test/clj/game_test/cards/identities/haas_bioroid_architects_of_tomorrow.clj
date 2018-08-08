(ns game-test.cards.identities.haas-bioroid-architects-of-tomorrow
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest haas-bioroid-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to rez after passing bioroid
  (do-game
    (new-game {:corp {:id "Haas-Bioroid: Architects of Tomorrow"
                      :deck [(qty "Eli 1.0" 2) "Pup"]}})
    (core/gain state :corp :credit 3)
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (play-from-hand state :corp "Pup" "Archives")
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (take-credits state :corp)
    (run-on state "Archives")
    (core/rez state :corp (get-ice state :archives 1))
    (run-continue state)
    (core/rez state :corp (get-ice state :archives 0))
    (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (click-card state :corp (get-ice state :hq 0))
    (is (= 3 (:credit (get-corp))) "Corp not charged for Architects of Tomorrow rez of Eli 1.0")))
