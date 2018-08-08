(ns game-test.cards.agendas.improved-protein-source
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest improved-protein-source
  ;; Improved Protein Source
  (do-game
    (new-game {:corp {:deck [(qty "Improved Protein Source" 2)]}})
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-and-score state "Improved Protein Source")
    (is (= 9 (:credit (get-runner))) "Runner should gain 4 credits from Corp scoring")
    (play-from-hand state :corp "Improved Protein Source" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote2)
    (click-prompt state :runner "Steal")
    (is (= 13 (:credit (get-runner))) "Runner should gain 4 credits from Corp scoring")))
