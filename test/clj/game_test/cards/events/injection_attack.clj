(ns game-test.cards.events.injection-attack
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest injection-attack
  ;; Injection Attack
  (do-game
    (new-game {:corp {:deck ["Paper Wall"]}
               :runner {:deck ["Injection Attack" "Corroder"]}})
    (play-from-hand state :corp "Paper Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Injection Attack")
    (click-prompt state :runner "Archives")
    (is (= 2 (:current-strength (get-program state 0))) "Corroder at 2 strength")
    (click-card state :runner (get-program state 0))
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (run-successful state)
    (is (= 2 (:current-strength (get-program state 0))) "Corroder reset to 2 strength")))
