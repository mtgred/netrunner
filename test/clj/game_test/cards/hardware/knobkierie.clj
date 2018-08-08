(ns game-test.cards.hardware.knobkierie
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest knobkierie
  ;; Knobkierie - first successful run, place a virus counter on a virus program
  (do-game
    (new-game {:runner {:deck ["Knobkierie" "Hivemind" "Eater"]}})
    (core/gain state :runner :credit 20)
    (take-credits state :corp)
    (play-from-hand state :runner "Knobkierie")
    (play-from-hand state :runner "Eater")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :runner "No action")
    (is (empty? (:prompt (get-runner))) "No prompt if not virus program installed")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Hivemind")
    (let [hv (find-card "Hivemind" (get-program state))]
      (is (= 1 (get-counters (refresh hv) :virus)) "Hivemind starts with 1 virus counters")
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "Yes") ; gain virus counter
      (click-card state :runner (find-card "Hivemind" (get-program state)))
      (click-prompt state :runner "No action")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind gains a counter on successful run")
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "No prompt after first run")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind doesn't gain a counter after first run"))))
