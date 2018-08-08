(ns game-test.cards.agendas.utopia-fragment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest utopia-fragment
  ;; Utopia Fragment
  (do-game
    (new-game {:corp {:deck ["Utopia Fragment"
                             "Hostile Takeover"]}})
    (play-and-score state "Utopia Fragment")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (advance state (get-content state :remote2 0))
    (take-credits state :corp)
    (run-on state :remote2)
    (run-successful state)
    (is (= ["Pay 2 [Credits] to steal" "No action"] (:choices (prompt-map :runner))))
    (click-prompt state :runner "Pay 2 [Credits] to steal")
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 3 (:credit (get-runner))))))
