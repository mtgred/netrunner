(ns game-test.cards.ice.mind-game
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mind-game
  ;; Mind game - PSI redirect to different server
  (do-game
    (new-game {:corp {:deck ["Mind Game"]}})
    (play-from-hand state :corp "Mind Game" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [mindgame (get-ice state :hq 0)]
      (core/rez state :corp mindgame)
      (card-subroutine state :corp mindgame 0))
    (click-prompt state :corp "1 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (is (= (set ["R&D" "Archives"]) (set (:choices (prompt-map :corp)))) "Corp cannot choose server Runner is on")
    (click-prompt state :corp "Archives")
    (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")))
