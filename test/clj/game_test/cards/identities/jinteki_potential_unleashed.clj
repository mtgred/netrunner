(ns game-test.cards.identities.jinteki-potential-unleashed
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jinteki-potential-unleashed
  ;; Potential Unleashed - when the runner takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game {:corp {:id "Jinteki: Potential Unleashed"
                      :deck ["Philotic Entanglement" "Neural EMP" (qty "Braintrust" 3)]}
               :runner {:deck [(qty "Employee Strike" 10)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Philotic Entanglement" "New remote")
    (score-agenda state :corp (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-runner)))))
    (play-from-hand state :corp "Neural EMP")
    (is (= 5 (count (:discard (get-runner)))))))
