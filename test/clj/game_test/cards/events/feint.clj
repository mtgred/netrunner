(ns game-test.cards.events.feint
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest feint
  ;; Feint - bypass 2 pieces of ice on HQ, but access no cards
  (do-game
    (new-game {:runner {:deck ["Feint"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Feint")
    (run-successful state)
    (click-prompt state :runner "OK")
    (is (not (:run @state)) "Run is over")))
