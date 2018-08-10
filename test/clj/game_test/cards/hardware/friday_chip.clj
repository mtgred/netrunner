(ns game-test.cards.hardware.friday-chip
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest friday-chip
  ;; Friday Chip - gain counters for trashing cards, move a counter on turn start
  (do-game
    (new-game {:corp {:deck ["Adonis Campaign" "Hedge Fund"]}
               :runner {:deck ["Friday Chip" "Aumakua"]}})
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :credit 20)
    (play-from-hand state :runner "Friday Chip")
    (play-from-hand state :runner "Aumakua")
    (let [fc (get-hardware state 0)
          aum (get-program state 0)]
      (is (zero? (get-counters fc :virus)) "Friday Chip starts with 0 counters")
      (is (zero? (get-counters aum :virus)) "Auakua starts with 0 counters")
      (run-on state "Server 1")
      (run-successful state)
      (click-prompt state :runner "Pay 3 [Credits] to trash") ; trash Adonis Campaing
      (click-prompt state :runner "Yes") ; gain virus counter
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip gains a counter on trash")
      (is (zero? (get-counters (refresh aum) :virus)) "Aumakua doesn't gain a counter")
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip doesn't gain a counter on non-trash")
      (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gains a counter on non-trash")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-card state :runner aum)
      (is (= 2 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
      (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip lost 1 counter"))))
