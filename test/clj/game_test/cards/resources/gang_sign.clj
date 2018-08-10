(ns game-test.cards.resources.gang-sign
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gang-sign
  ;; Gang Sign
  (testing "accessing from HQ, not including root. Issue #2113"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 3) (qty "Braintrust" 2) "Crisium Grid"]}
                 :runner {:deck [(qty "Gang Sign" 2) "HQ Interface"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "HQ Interface")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Gang Sign") ; simultaneous effect resolution
      (let [gs1 (-> (get-runner) :prompt first)]
        (is (= (:choices gs1) ["Card from hand"]) "Gang Sign does not let Runner access upgrade in HQ root")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "Steal")
        (is (= (:card gs1) (-> (get-runner) :prompt first :card)) "Second access from first Gang Sign triggered")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "Steal")
        (is (not= (:card gs1) (-> (get-runner) :prompt first :card)) "First access from second Gang Sign triggered")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "Steal")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "Steal"))))
  (testing "accessing from HQ, not including root. Issue #2113"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Snare!"]}
                 :runner {:deck ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (click-prompt state :runner "Card from hand")
      ;; Runner has "wait for Snare, wait for on-access" prompts.
      (is (= 2 (count (:prompt (get-runner)))) "Runner only has the Waiting prompt, not Snare!'s pay-prompt")
      ;; Core has "pay for Snare, wait for agenda-scored" prompts.
      (is (= 2 (count (:prompt (get-corp)))) "Corp has the prompt to use Snare!"))))
