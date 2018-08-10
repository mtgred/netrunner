(ns game-test.cards.icebreakers.snowball
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest snowball
  ;; Snowball - Strength boost until end of run when used to break a subroutine
  (do-game
    (new-game {:corp {:deck ["Spiderweb" "Fire Wall" "Hedge Fund"]}
               :runner {:deck ["Snowball"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Fire Wall" "HQ")
    (play-from-hand state :corp "Spiderweb" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Snowball")
    (let [sp (get-ice state :hq 1)
          fw (get-ice state :hq 0)
          snow (get-program state 0)]
      (run-on state "HQ")
      (core/rez state :corp sp)
      (core/rez state :corp fw)
      (card-ability state :runner snow 1) ; match strength
      (is (= 2 (:current-strength (refresh snow))))
      (card-ability state :runner snow 0) ; strength matched, break a sub
      (card-ability state :runner snow 0) ; break a sub
      (is (= 4 (:current-strength (refresh snow))) "Broke 2 subs, gained 2 more strength")
      (run-continue state)
      (is (= 3 (:current-strength (refresh snow))) "Has +2 strength until end of run; lost 1 per-encounter boost")
      (card-ability state :runner snow 1)
      (card-ability state :runner snow 1) ; match strength
      (is (= 5 (:current-strength (refresh snow))) "Matched strength, gained 2")
      (card-ability state :runner snow 0) ; strength matched, break a sub
      (is (= 6 (:current-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
      (run-continue state)
      (is (= 4 (:current-strength (refresh snow))) "+3 until-end-of-run strength")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh snow))) "Back to default strength"))))
