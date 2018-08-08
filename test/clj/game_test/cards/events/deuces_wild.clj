(ns game-test.cards.events.deuces-wild
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest deuces-wild
  ;; Deuces Wild
  (do-game
    (new-game {:corp {:deck ["Wraparound"
                             "The Future Perfect"]}
               :runner {:deck [(qty "Deuces Wild" 2) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Wraparound" "New remote")
    (take-credits state :corp)
    (starting-hand state :runner ["Deuces Wild" "Deuces Wild"])
    (play-from-hand state :runner "Deuces Wild")
    (click-prompt state :runner "Gain 3 [Credits]")
    (is (= 6 (:credit (get-runner))) "Gained 1 net credit")
    (click-prompt state :runner "Draw 2 cards")
    (is (= 3 (count (:hand (get-runner)))) "Drew 2 cards")
    (is (empty? (:prompt (get-runner))) "Deuces Wild not showing a third choice option")
    (play-from-hand state :runner "Deuces Wild")
    (click-prompt state :runner "Expose 1 ice and make a run")
    (click-card state :runner (get-ice state :remote1 0))
    (click-prompt state :runner "HQ")
    (is (empty? (:prompt (get-runner))) "Deuces prompt not queued")
    (run-continue state)
    (run-successful state)
    (is (= 2 (count (:prompt (get-runner)))) "Deuces prompt not queued")
    (click-prompt state :corp "0 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "TFP stolen")
    (core/gain state :runner :tag 1)
    (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
    (click-prompt state :runner "Remove 1 tag")
    (is (zero? (:tag (get-runner))))))
