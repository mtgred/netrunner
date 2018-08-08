(ns game-test.cards.agendas.chronos-project
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest chronos-project
  ;; Chronos Project
  (do-game
    (new-game {:corp {:deck ["Chronos Project"]}})
    (dotimes [_ 3]
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :discard))
    (is (= 3 (count (:discard (get-runner)))) "Runner should have 3 cards in heap")
    (play-and-score state "Chronos Project")
    (is (zero? (count (:discard (get-runner)))) "Runner should have 0 cards in heap")))
