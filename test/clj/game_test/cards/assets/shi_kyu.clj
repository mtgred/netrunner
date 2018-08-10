(ns game-test.cards.assets.shi-kyu
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest shi-kyu
  ;; Shi.Kyū
  (do-game
    (new-game {:corp {:deck ["Shi.Kyū"]}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Shi.Kyū" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "5")
    (is (= "Take 5 net damage" (-> (prompt-map :runner) :choices first)))
    (click-prompt state :runner "Take 5 net damage")
    (click-prompt state :runner "No action")
    (is (zero? (count (:hand (get-runner)))) "Runner took 5 net damage from Shi.Kyū")
    (run-empty-server state "Server 1")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "2")
    (is (= "Take 2 net damage" (-> (prompt-map :runner) :choices first)))
    (click-prompt state :runner "Add Shi.Kyū to score area")
    (is (empty? (prompt-map :runner)) "Runner shouldn't get the option to trash Shi.Kyū as it was added to agenda area")
    (is (= -1 (:agenda-point (get-runner))) "Runner should be at -1 agenda points after adding Shi.Kyū to agenda area")))
