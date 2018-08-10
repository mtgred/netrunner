(ns game-test.cards.identities.skorpios-defense-systems-persuasive-power
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest skorpios-defense-systems-persuasive-power
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game {:corp {:id "Skorpios Defense Systems: Persuasive Power"
                      :deck ["Hedge Fund" (qty "Quandary" 4)]}
               :runner {:deck ["The Maker's Eye" "Lucky Find"]}})
    (play-from-hand state :corp "Hedge Fund")
    (dotimes [_ 4] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (play-from-hand state :runner "Lucky Find")
    (play-from-hand state :runner "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-corp) :prompt first :choices))) "No Maker's Eye choice")
    (click-prompt state :corp "Cancel")
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
    (is (not (:run @state)))
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (click-prompt state :corp (find-card "The Maker's Eye" (:discard (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rfg]))) "One card RFGed")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (:prompt (get-corp))) "Cannot use Skorpios twice")))
