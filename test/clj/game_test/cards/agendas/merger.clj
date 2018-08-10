(ns game-test.cards.agendas.merger
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest merger
  ;; Merger
  (do-game
    (new-game {:corp {:deck [(qty "Merger" 2)]}})
    (play-and-score state "Merger")
    (is (= 2 (:agenda-point (get-corp))) "Corp should score 2 points")
    (play-from-hand state :corp "Merger" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote2)
    (click-prompt state :runner "Steal")
    (is (= 3 (:agenda-point (get-runner))) "Runner should score 3 points")))
