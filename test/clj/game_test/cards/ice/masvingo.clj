(ns game-test.cards.ice.masvingo
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest masvingo
  (do-game
    (new-game {:corp {:deck ["Masvingo"]}})
    (play-from-hand state :corp "Masvingo" "HQ")
    (let [mas (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh mas) :advancement)) "Should install with 0 counter")
      (core/rez state :corp (refresh mas))
      (is (= 1 (get-counters (refresh mas) :advancement)) "Should rez with 1 counter")
      (take-credits state :corp)
      (run-on state :hq)
      (card-subroutine state :corp mas 0)
      (is (not (:run @state)) "Run is ended"))))
