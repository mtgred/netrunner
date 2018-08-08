(ns game-test.cards.agendas.meteor-mining
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest meteor-mining
  ;; Meteor Mining
  (testing "when Meteor Mining is stolen"
    (do-game
      (new-game {:corp {:deck ["Meteor Mining"]}})
      (play-from-hand state :corp "Meteor Mining" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should score 2 points")))
  (testing "when Meteor Mining is scored"
    (letfn [(meteor-mining-test [[tags num-choices pick creds dmg]]
              (do-game
                (new-game {:corp {:deck ["Meteor Mining"]}
                           :runner {:deck [(qty "Sure Gamble" 7)]}})
                (starting-hand state :runner (repeat 7 "Sure Gamble"))
                (let [credits (:credit (get-corp))
                      grip (count (:hand (get-runner)))]
                  (core/gain state :runner :tag tags)
                  (play-and-score state "Meteor Mining")
                  (is (= num-choices (count (:choices (first (get-in @state [:corp :prompt]))))))
                  (click-prompt state :corp pick)
                  (is (= (+ credits creds) (:credit (get-corp)))
                      (str "Corp should have " (+ credits creds) " credits"))
                  (is (= (- grip dmg) (count (:hand (get-runner))))
                      (str "Runner should have " (- grip dmg) " cards in hand")))))]
      (doall (map meteor-mining-test
                  [[0 2 "No action" 0 0]
                   [0 2 "Gain 7 [Credits]" 7 0]
                   [1 2 "No action" 0 0]
                   [1 2 "Gain 7 [Credits]" 7 0]
                   [2 3 "No action" 0 0]
                   [2 3 "Gain 7 [Credits]" 7 0]
                   [2 3 "Do 7 meat damage" 0 7]
                   [3 3 "No action" 0 0]
                   [3 3 "Gain 7 [Credits]" 7 0]
                   [3 3 "Do 7 meat damage" 0 7]])))))
