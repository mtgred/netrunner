(ns game-test.cards.resources.find-the-truth
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest find-the-truth
  ;; Find the Truth
  (testing "Basic test - On successful run see the top card from R&D before access"
    (do-game
      (new-game {:corp {:deck [(qty "Restructure" 10)]}
                 :runner {:deck ["Find the Truth"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Find the Truth")
      (run-on state "HQ")
      (run-successful state)
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :runner :prompt first :msg)) "FTT prompt")
      (click-prompt state :runner "Yes")
      (is (= "The top card of R&D is Restructure" (-> @state :runner :prompt first :msg)) "FTT shows card on R&D")
      (click-prompt state :runner "OK")))
  (testing "Equivocation & FTT - should get order of choice"
    (do-game
      (new-game {:corp {:deck [(qty "Restructure" 10)]}
                 :runner {:deck ["Equivocation" "Find the Truth"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Equivocation")
      (play-from-hand state :runner "Find the Truth")
      (run-empty-server state :rd)
      (click-prompt state :runner "Find the Truth")
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :runner :prompt first :msg)) "FTT prompt")
      (click-prompt state :runner "Yes")
      (is (= "The top card of R&D is Restructure" (-> @state :runner :prompt first :msg)) "FTT shows card")
      (click-prompt state :runner "OK") ; Equivocation prompt
      (is (= "Reveal the top card of R&D?" (-> @state :runner :prompt first :msg)) "Equivocation Prompt")
      (click-prompt state :runner "Yes")))
  (testing "Find The Truth should completed before Marilyn trash is forced"
    (do-game
      (new-game {:corp {:deck ["Marilyn Campaign" (qty "Vanilla" 10)]}
                 :runner {:deck ["Find the Truth" "Neutralize All Threats"]}})
      (starting-hand state :corp ["Marilyn Campaign"])
      (play-from-hand state :corp "Marilyn Campaign" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 3 (:credit (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Find the Truth")
      (play-from-hand state :runner "Neutralize All Threats")
      (run-on state :remote1)
      (run-successful state)
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :runner :prompt first :msg)) "FTT prompt")
      (is (= "Waiting for Runner to resolve successful-run triggers" (-> @state :corp :prompt first :msg)) "No Marilyn Shuffle Prompt")
      (click-prompt state :runner "Yes")
      (is (= "The top card of R&D is Vanilla" (-> @state :runner :prompt first :msg)) "FTT shows card")
      (is (= "Waiting for Runner to resolve successful-run triggers" (-> @state :corp :prompt first :msg)) "No Marilyn Shuffle Prompt")
      (click-prompt state :runner "OK")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= "Waiting for Corp to use Marilyn Campaign" (-> @state :runner :prompt first :msg)) "Now Corp gets shuffle choice")
      (is (= "Shuffle Marilyn Campaign into R&D?" (-> @state :corp :prompt first :msg)) "Now Corp gets shuffle choice")
      (is (= 2 (:credit (get-runner)))) #_ trashed_marilyn)))
