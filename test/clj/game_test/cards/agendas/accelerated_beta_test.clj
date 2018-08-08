(ns game-test.cards.agendas.accelerated-beta-test
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest accelerated-beta-test
  ;; Accelerated Beta Test
  (do-game
    (new-game {:corp {:deck ["Accelerated Beta Test" "Enigma" (qty "Hedge Fund" 2)]}})
    ;; Set up
    (starting-hand state :corp ["Accelerated Beta Test"])
    (play-and-score state "Accelerated Beta Test")
    (click-prompt state :corp "Yes")
    (click-card state :corp (find-card "Enigma" (get-in @state [:corp :play-area])))
    (click-prompt state :corp "HQ")
    (is (some? (get-ice state :hq 0)))
    (is (= 2 (count (:discard (get-corp)))))
    (core/move state :corp (find-card "Accelerated Beta Test" (:scored (get-corp))) :hand)
    (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :deck)
    (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :deck)
    (play-and-score state "Accelerated Beta Test")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "I have no regrets")
    (is (= 2 (count (:discard (get-corp)))))))
