(ns game-test.cards.icebreakers.overmind
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest overmind
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game {:runner {:deck ["Overmind" (qty "Akamatsu Mem Chip" 2)]}})
    (take-credits state :corp)
    (take-credits state :runner 1)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 6 (core/available-mu state)))
    (play-from-hand state :runner "Overmind")
    (is (= 5 (core/available-mu state)))
    (let [ov (get-program state 0)]
      (is (= 5 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))
