(ns game-test.cards.operations.sub-boost
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sub-boost
  ;; Sub Boost - Give ICE Barrier
  (do-game
    (new-game {:corp {:deck ["Sub Boost" "Quandary"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (let [qu (get-ice state :hq 0)]
      (core/rez state :corp qu)
      (is (not (core/has-subtype? (refresh qu) "Barrier")) "Quandry starts without Barrier")
      (is (= 1 (count (:subroutines (refresh qu)))) "Quandry has 1 subroutine")
      (play-from-hand state :corp "Sub Boost")
      (click-card state :corp (refresh qu))
      (is (core/has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (core/has-subtype? (refresh qu) "Barrier") "Quandary ICE Barrier")
      (is (= 2 (count (:subroutines (refresh qu)))) "Quandry gains a subroutine"))))
