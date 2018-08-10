(ns game-test.cards.hardware.maya
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 2) (qty "Snare!" 2) "Hostile Takeover" "Scorched Earth"]}
                 :runner {:deck ["Maya" (qty "Sure Gamble" 3)]}})
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "Maya")
      (let [maya (get-hardware state 0)
            accessed (first (:deck (get-corp)))]
        (run-empty-server state :rd)
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
        (card-ability state :runner maya 0)
        (is (empty? (:prompt (get-runner))) "No more prompts for runner")
        (is (not (:run @state)) "Run is ended")
        (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
        (take-credits state :runner)
        (core/draw state :corp)
        (take-credits state :corp)
        (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
        (let [accessed (first (:deck (get-corp)))]
          (run-empty-server state :rd)
          (click-prompt state :corp "Yes")
          (is (zero? (count (:hand (get-runner)))) "Runner took Snare! net damage")
          (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
          (card-ability state :runner maya 0)
          (is (empty? (:prompt (get-runner))) "No more prompts for runner")
          (is (not (:run @state)) "Run is ended")
          (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")))))
  (testing "Does not interrupt multi-access"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)]}
                 :runner {:deck ["Maya" (qty "Sure Gamble" 3) "R&D Interface"]}})
      (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Maya")
      (play-from-hand state :runner "R&D Interface")
      (let [maya (get-hardware state 0)
            accessed (first (:deck (get-corp)))]
        (run-empty-server state :rd)
        (click-prompt state :runner "Card from deck")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
        (card-ability state :runner maya 0)
        (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
        (is (:prompt (get-runner)) "Runner has next access prompt")))))
