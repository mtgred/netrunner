(ns game-test.cards.upgrades.calibration-testing
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest calibration-testing
  ;; Calibration Testing - advanceable / non-advanceable
  (do-game
    (new-game {:corp {:deck [(qty "Calibration Testing" 2) "Project Junebug" "PAD Campaign"]}})
    (core/gain state :corp :credit 10)
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Calibration Testing" "New remote")
    (play-from-hand state :corp "Project Junebug" "Server 1")
    (let [ct (get-content state :remote1 0)
          pj (get-content state :remote1 1)]
      (core/rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-card state :corp pj)
      (is (= 1 (get-counters (refresh pj) :advancement)) "Project Junebug advanced")
      (is (= 1 (count (:discard (get-corp)))) "Calibration Testing trashed"))
    (play-from-hand state :corp "Calibration Testing" "New remote")
    (play-from-hand state :corp "PAD Campaign" "Server 2")
    (let [ct (get-content state :remote2 0)
          pad (get-content state :remote2 1)]
      (core/rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-card state :corp pad)
      (is (= 1 (get-counters (refresh pad) :advancement)) "PAD Campaign advanced")
      (is (= 2 (count (:discard (get-corp)))) "Calibration Testing trashed"))))
