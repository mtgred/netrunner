(ns game-test.cards.icebreakers.mammon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mammon
  ;; Mammon - Pay to add X power counters at start of turn, all removed at end of turn
  (do-game
    (new-game {:runner {:deck ["Mammon"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mammon")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [mam (get-program state 0)]
      (card-ability state :runner mam 0)
      (click-prompt state :runner "3")
      (is (= 2 (:credit (get-runner))) "Spent 3 credits")
      (is (= 3 (get-counters (refresh mam) :power)) "Mammon has 3 power counters")
      (take-credits state :runner)
      (is (zero? (get-counters (refresh mam) :power)) "All power counters removed"))))
