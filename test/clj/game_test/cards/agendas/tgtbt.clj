(ns game-test.cards.agendas.tgtbt
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tgtbt
  ;; TGTBT - Give the Runner 1 tag when they access
  ;; OHG still not working...
  (do-game
    (new-game {:corp {:deck [(qty "TGTBT" 2) "Old Hollywood Grid"]}})
    (play-from-hand state :corp "TGTBT" "New remote")
    (play-from-hand state :corp "Old Hollywood Grid" "Server 1")
    (play-from-hand state :corp "TGTBT" "New remote")
    (take-credits state :corp)
    (let [tg1 (get-content state :remote1 0)
          ohg (get-content state :remote1 1)]
      (run-on state "Server 1")
      (core/rez state :corp ohg)
      (run-successful state)
      (click-card state :runner tg1)
      ;; Accesses TGTBT but can't steal
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag from accessing without stealing")
      (click-prompt state :runner "No action")
      (click-card state :runner ohg))
    (click-prompt state :runner "Pay 4 [Credits] to trash") ;; Trashes OHG
    (run-empty-server state "Server 2")
    ;; Accesses TGTBT and can steal
    (click-prompt state :runner "Steal")
    (is (= 2 (:tag (get-runner))) "Runner took 1 tag from accessing and stealing")))
