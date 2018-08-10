(ns game-test.cards.hardware.spinal-modem
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
    (new-game {:corp {:deck ["Caduceus"]}
               :runner {:deck ["Spinal Modem" "Sure Gamble"]}})
    (play-from-hand state :corp "Caduceus" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Spinal Modem")
    (let [cad (get-ice state :hq 0)
          sm (get-hardware state 0)]
      (is (= 5 (core/available-mu state)))
      (is (= 2 (get-counters (refresh sm) :recurring)))
      (run-on state :hq)
      (core/rez state :corp cad)
      (card-subroutine state :corp cad 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 4 (core/hand-size state :runner)) "Reduced hand size"))))
