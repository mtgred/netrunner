(ns game-test.cards.operations.psychokinesis
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest psychokinesis
  ;; Pyschokinesis - Terminal Event (end the turn); Look at R&D, install an Asset, Agenda, or Upgrade in a Remote Server
  (do-game
    (new-game {:corp {:deck [(qty "Psychokinesis" 3) "Caprice Nisei" "Adonis Campaign"
                             "Global Food Initiative" "Mwanza City Grid"]}})
    (starting-hand state :corp ["Psychokinesis" "Psychokinesis" "Psychokinesis"])
    ;; Test installing an Upgrade
    (play-from-hand state :corp "Psychokinesis")
    (is (not-any? #{"Mwanza City Grid"} (map :title (-> (get-corp) :prompt first :choices)))
        "Mwanza City Grid is not on the list of installable cards")
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Caprice Nisei" (:title (get-content state :remote1 0)))
        "Caprice Nisei installed by Psychokinesis")
    ;; Test installing an Asset
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Adonis Campaign" (:title (get-content state :remote2 0)))
        "Adonis Campaign installed by Psychokinesis")
    ;; Test installing an Agenda
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Global Food Initiative" (:title (get-content state :remote3 0)))
        "Global Food Initiative installed by Psychokinesis")
    ;; Test selecting "None"
    (core/gain state :corp :click 1)
    (core/move state :corp (find-card "Psychokinesis" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp "None")
    (is (= nil (:title (get-content state :remote4 0)))
        "Nothing is installed by Psychokinesis")))
