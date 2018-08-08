(ns game-test.cards.upgrades.chilo-city-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest chilo-city-grid
  ;; ChiLo City Grid - Give 1 tag for successful traces during runs on its server
  (do-game
    (new-game {:corp {:deck [(qty "Caduceus" 2) "ChiLo City Grid"]}})
    (play-from-hand state :corp "ChiLo City Grid" "New remote")
    (play-from-hand state :corp "Caduceus" "Server 1")
    (take-credits state :corp)
    (let [chilo (get-content state :remote1 0)
          cad (get-ice state :remote1 0)]
      (run-on state "R&D")
      (core/rez state :corp cad)
      (core/rez state :corp chilo)
      (card-subroutine state :corp cad 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 3 (:credit (get-corp))) "Trace was successful")
      (is (zero? (:tag (get-runner))) "No tags given for run on different server")
      (run-successful state)
      (run-on state "Server 1")
      (card-subroutine state :corp cad 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 6 (:credit (get-corp))) "Trace was successful")
      (is (= 1 (:tag (get-runner)))
          "Runner took 1 tag given from successful trace during run on ChiLo server"))))
