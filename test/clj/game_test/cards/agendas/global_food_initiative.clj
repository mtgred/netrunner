(ns game-test.cards.agendas.global-food-initiative
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest global-food-initiative
  ;; Global Food Initiative
  (do-game
    (new-game {:corp {:deck [(qty "Global Food Initiative" 2)]}})
    (testing "Corp scores"
      (is (zero? (:agenda-point (get-runner))) "Runner should start with 0 agenda points")
      (is (zero? (:agenda-point (get-corp))) "Corp should start with 0 agenda points")
      (play-and-score state "Global Food Initiative")
      (is (= 3 (:agenda-point (get-corp))) "Corp should gain 3 agenda points"))
    (testing "Runner steals"
      (play-from-hand state :corp "Global Food Initiative" "New remote")
      (take-credits state :corp)
      (run-on state :remote2)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should gain 2 agenda points, not 3"))))
