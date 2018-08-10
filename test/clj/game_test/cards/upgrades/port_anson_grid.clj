(ns game-test.cards.upgrades.port-anson-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest port-anson-grid
  ;; Port Anson Grid - Prevent the Runner from jacking out until they trash a program
  (do-game
    (new-game {:corp {:deck ["Port Anson Grid" "Data Raven"]}
               :runner {:deck ["Faerie" "Technical Writer"]}})
    (play-from-hand state :corp "Port Anson Grid" "New remote")
    (play-from-hand state :corp "Data Raven" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Technical Writer")
    (play-from-hand state :runner "Faerie")
    (let [pag (get-content state :remote1 0)
          fae (get-program state 0)
          tw (get-resource state 0)]
      (run-on state "Server 1")
      (core/rez state :corp pag)
      (is (:cannot-jack-out (get-in @state [:run])) "Jack out disabled for Runner") ; UI button greyed out
      (core/trash state :runner tw)
      (is (:cannot-jack-out (get-in @state [:run])) "Resource trash didn't disable jack out prevention")
      (core/trash state :runner fae)
      (is (nil? (:cannot-jack-out (get-in @state [:run]))) "Jack out enabled by program trash")
      (run-on state "Server 1")
      (is (:cannot-jack-out (get-in @state [:run])) "Prevents jack out when upgrade is rezzed prior to run"))))
