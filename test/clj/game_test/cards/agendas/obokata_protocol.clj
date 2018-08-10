(ns game-test.cards.agendas.obokata-protocol
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest obokata-protocol
  ;; Obotaka Protocol
  (do-game
    (new-game {:corp {:id "Jinteki: Personal Evolution"
                      :deck [(qty "Obokata Protocol" 10)]}
               :runner {:deck [(qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "Obokata Protocol" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :agenda-point 6)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 4 net damage to steal")
    (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 net damage")
    (is (= :runner (:winner @state)) "Runner wins")
    (is (= "Agenda" (:reason @state)) "Win condition reports agenda points")
    (is (last-log-contains? state "wins the game") "PE did not fire")))
