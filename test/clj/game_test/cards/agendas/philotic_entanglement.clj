(ns game-test.cards.agendas.philotic-entanglement
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest philotic-entanglement
  ;; Philotic Entanglement
  (do-game
    (new-game {:corp {:deck ["Philotic Entanglement" (qty "House of Knives" 3)]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Cache" 2)]}})
    (play-from-hand state :corp "House of Knives" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (run-empty-server state :remote2)
    (click-prompt state :runner "Steal")
    (run-empty-server state :remote3)
    (click-prompt state :runner "Steal")
    (is (= 3 (count (:scored (get-runner)))))
    (take-credits state :runner)
    (play-and-score state "Philotic Entanglement")
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 3 (count (:discard (get-runner)))) "Dealt 3 net damage upon scoring")))
