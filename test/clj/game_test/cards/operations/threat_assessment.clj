(ns game-test.cards.operations.threat-assessment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest threat-assessment
  ;; Threat Assessment - play only if runner trashed a card last turn, move a card to the stack or take 2 tags
  (do-game
    (new-game {:corp {:deck [(qty "Threat Assessment" 3) "Adonis Campaign"]}
               :runner {:deck ["Desperado" "Corroder"]}})
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (take-credits state :corp)
    (run-on state :remote1)
    (run-successful state)
    (click-prompt state :runner "Pay 3 [Credits] to trash")
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Desperado")
    (play-from-hand state :runner "Corroder")
    (take-credits state :runner)
    (is (zero? (:tag (get-runner))) "Runner starts with 0 tags")
    (play-from-hand state :corp "Threat Assessment")
    (click-card state :corp (find-card "Desperado" (-> (get-runner) :rig :hardware)))
    (click-prompt state :runner "2 tags")
    (is (= 2 (:tag (get-runner))) "Runner took 2 tags")
    (is (= 1 (count (-> (get-runner) :rig :hardware))) "Didn't trash Desperado")
    (is (= "Threat Assessment" (:title (first (:rfg (get-corp))))) "Threat Assessment removed from game")
    (play-from-hand state :corp "Threat Assessment")
    (click-card state :corp (find-card "Corroder" (-> (get-runner) :rig :program)))
    (click-prompt state :runner "Move Corroder")
    (is (= 2 (:tag (get-runner))) "Runner didn't take tags")
    (is (= "Corroder" (:title (first (:deck (get-runner))))) "Moved Corroder to the deck")
    (is (= 2 (count (:rfg (get-corp)))))
    (take-credits state :runner)
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Threat Assessment")
    (is (empty? (:prompt (get-corp))) "Threat Assessment triggered with no trash")))
