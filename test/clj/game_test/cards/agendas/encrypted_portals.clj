(ns game-test.cards.agendas.encrypted-portals
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest encrypted-portals
  ;; Encrypted Portals
  (do-game
    (new-game {:corp {:deck ["Encrypted Portals" "Lotus Field"]}})
    (play-from-hand state :corp "Lotus Field" "HQ")
    (let [lf (get-ice state :hq 0)]
      (core/rez state :corp lf)
      (is (= 4 (:current-strength (refresh lf))) "Should start with base strength of 4")
      (is (zero? (:credit (get-corp))) "Should have 0 credits after rez")
      (play-and-score state "Encrypted Portals")
      (is (= 5 (:current-strength (refresh lf))) "Should gain 1 strength from 4 to 5")
      (is (= 1 (:credit (get-corp))) "Should gain 1 credit for rezzed code gate"))))
