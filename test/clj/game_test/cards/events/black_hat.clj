(ns game-test.cards.events.black-hat
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest black-hat
  ;; Black Hat
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                 :runner {:deck [(qty "Black Hat" 3)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Black Hat")
      (click-prompt state :corp "0")
      (click-prompt state :runner "4")
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Card from deck")))
  (testing "Kitsune interaction"
    (do-game
      (new-game {:corp {:deck [(qty "Kitsune" 10)]}
                 :runner {:deck [(qty "Black Hat" 3)]}})
      (starting-hand state :corp ["Kitsune" "Kitsune" "Kitsune" "Kitsune" "Kitsune"])
      (play-from-hand state :corp "Kitsune" "R&D")
      (let [kitsune (get-ice state :rd 0)]
        (core/rez state :corp kitsune)
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (play-from-hand state :runner "Black Hat")
        (click-prompt state  :corp "0")
        (click-prompt state :runner "4")
        (run-on state :rd)
        (card-subroutine state :corp kitsune 0)
        (click-card state :corp (find-card "Kitsune" (:hand (get-corp))))
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")))))
