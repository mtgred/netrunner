(ns game-test.cards.events.the-maker-s-eye
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-maker-s-eye
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 5)]}
               :runner {:deck ["The Maker's Eye"]}})
    (dotimes [_ 5] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (play-from-hand state :runner "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "1st quandary")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "2nd quandary")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "3rd quandary")
    (click-prompt state :runner "No action")
    (is (not (:run @state)))))
