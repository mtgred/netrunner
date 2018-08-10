(ns game-test.cards.ice.minelayer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest minelayer
  ;; Minelayer - Install a piece of ICE in outermost position of Minelayer's server at no cost
  (do-game
    (new-game {:corp {:deck ["Minelayer" "Fire Wall"]}})
    (play-from-hand state :corp "Minelayer" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (core/rez state :corp (get-ice state :hq 0))
    (is (= 6 (:credit (get-corp))))
    (card-subroutine state :corp (get-ice state :hq 0) 0)
    (click-card state :corp (find-card "Fire Wall" (:hand (get-corp))))
    (is (= 2 (count (get-in @state [:corp :servers :hq :ices]))) "2 ICE protecting HQ")
    (is (= 6 (:credit (get-corp))) "Didn't pay 1 credit to install as second ICE")))
