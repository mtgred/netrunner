(ns game-test.cards.upgrades.drone-screen
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest drone-screen
  ;; Drone Screen
  (do-game
    (new-game {:corp {:deck ["Drone Screen"]}})
    (play-from-hand state :corp "Drone Screen" "New remote")
    (let [drone (get-content state :remote1 0)]
      (core/rez state :corp drone)
      (core/gain state :runner :tag 1)
      (take-credits state :corp)
      (run-on state "Server 1")
      (is (zero? (-> (get-runner) :discard count)) "Heap should start empty")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should discard 1 card from meat damage from losing Drone Screen trace"))))
