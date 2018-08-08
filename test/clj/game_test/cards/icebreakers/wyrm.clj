(ns game-test.cards.icebreakers.wyrm
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest wyrm
  ;; Wyrm reduces strength of ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Wyrm"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Wyrm")
    (run-on state "HQ")
    (let [ice-wall (get-ice state :hq 0)
          wyrm (get-program state 0)]
      (core/rez state :corp ice-wall)
      (card-ability state :runner wyrm 1)
      (is (zero? (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to 0")
      (card-ability state :runner wyrm 1)
      (is (= -1 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to -1"))))
