(ns game-test.cards.agendas.eden-fragment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest eden-fragment
  ;; Test that Eden Fragment ignores the install cost of the first ice
  (do-game
    (new-game {:corp {:deck [(qty "Eden Fragment" 3) (qty "Ice Wall" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-and-score state "Eden Fragment")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (some? (get-ice state :hq 1)) "Corp has two ice installed on HQ")
    (is (= 6 (:credit (get-corp))) "Corp does not pay for installing the first ICE of the turn")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (some? (get-ice state :hq 2)) "Corp has three ice installed on HQ")
    (is (= 4 (:credit (get-corp))) "Corp pays for installing the second ICE of the turn")))
