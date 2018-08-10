(ns game-test.cards.hardware.doppelganger
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest doppelganger
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game {:runner {:deck ["Doppelgänger"]}})
    (core/gain state :corp :bad-publicity 1)
    (take-credits state :corp)
    (play-from-hand state :runner "Doppelgänger")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (is (zero? (:run-credit (get-runner))) "Runner lost BP credits")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-runner))) "Runner has 1 BP credit")))
