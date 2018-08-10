(ns game-test.cards.resources.salsette-slums
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest salsette-slums
  ;; Salsette Slums - Once per turn, when the trash cost of a card is paid, optionally remove from the game
  (do-game
    (new-game {:corp {:deck ["Hostile Infrastructure" "Tech Startup" "Thomas Haas"
                             (qty "Hedge Fund" 3)]}
               :runner {:deck [(qty "Salsette Slums" 2) (qty "Sure Gamble" 3)]}})
    ;; Use Hostile Infrastructure to ensure on-trash effects don't fire.
    (core/move state :corp (find-card "Hostile Infrastructure" (:deck (get-corp))) :hand)
    (core/move state :corp (find-card "Tech Startup" (:deck (get-corp))) :hand)
    (core/move state :corp (find-card "Thomas Haas" (:deck (get-corp))) :hand)
    (play-from-hand state :corp "Tech Startup" "New remote")
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (play-from-hand state :corp "Thomas Haas" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Salsette Slums")
    (play-from-hand state :runner "Salsette Slums")
    (core/gain state :runner :credit 2)
    (core/gain state :runner :click 4)
    (let [ts1 (get-content state :remote1 0)
          hostile2 (get-content state :remote2 0)
          th3 (get-content state :remote3 0)
          salsette1 (get-resource state 0)
          salsette2 (get-resource state 1)]
      (is (= 3 (count (:hand (get-runner)))) "Runner started this part with three cards in hand")
      (core/rez state :corp hostile2)
      (run-empty-server state "Server 1")
      (is (not (empty? (:prompt (get-runner)))) "Prompting to trash.")
      (card-ability state :runner salsette1 0)
      (is (empty? (:prompt (get-runner))) "All prompts done")
      (is (= 3 (count (:hand (get-runner)))) "On-trash ability of other Hostile didn't fire")
      (is (= (:cid ts1) (:cid (last (:rfg (get-corp))))) "Tech Startup was removed from game")
      (is (= 2 (:credit (get-runner))) "Runner paid the trash cost.")
      (is (not (:run @state)) "Run is over")
      (run-empty-server state :remote2)
      (is (not (empty? (:prompt (get-runner)))) "Prompting to trash")
      ;; Only able to use the ability once per turn
      (card-ability state :runner salsette1 0)
      (is (not (empty? (:prompt (get-runner)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      ;; Can't use the ability if you can't afford to trash
      (card-ability state :runner salsette2 0)
      (is (not (empty? (:prompt (get-runner)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      (click-prompt state :runner "No action")
      ;; Test the "oops I forgot" ability (runner feels bad that they forgot to use Slums when a Hostile is out)
      (run-empty-server state :remote3)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      ;; Can only use that first Slums once
      (card-ability state :runner salsette1 1)
      (is (empty? (:prompt (get-runner))) "Not prompting the runner")
      (is (not (= (:cid th3) (:cid (last (:rfg (get-corp)))))) "Card was not removed from the game")
      (card-ability state :runner salsette2 1)
      (is (not (empty? (:prompt (get-runner)))) "Prompting the runner to choose a card")
      (click-card state :runner (find-card "Thomas Haas" (:discard (get-corp))))
      (is (= (:cid th3) (:cid (last (:rfg (get-corp))))) "Card was removed from the game"))
    ;; Set things up so we can trash the Hostile and then make sure we can't "oops I forgot on a later turn"
    (core/gain state :runner :credit 5)
    (run-empty-server state :remote2)
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [salsette1 (get-resource state 0)
          hostile2 (get-content state :remote2 0)]
      (card-ability state :runner salsette1 1)
      (click-card state :runner (find-card "Hostile Infrastructure" (:discard (get-corp))))
      (is (not (= (:cid hostile2) (:cid (last (:rfg (get-corp)))))) "Did not remove card from game"))))
