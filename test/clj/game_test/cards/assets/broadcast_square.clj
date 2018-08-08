(ns game-test.cards.assets.broadcast-square
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest broadcast-square
  ;; Broadcast Square - Trace 3: Prevent all bad publicity
  (do-game
    (new-game {:corp {:deck ["Profiteering" "Hostile Takeover" "Broadcast Square"]}})
    (play-from-hand state :corp "Broadcast Square" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 3 (:credit (get-corp))) "Corp should have spent 2 credits")
    (play-from-hand state :corp "Profiteering" "New remote")
    (score-agenda state :corp (get-content state :remote2 0))
    (click-prompt state :corp "3")  ;; Take 3 bad publicity from Profiteering, gain 15 (if bad publicity actually taken)
    (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
    (click-prompt state :runner "0")  ;; Runner doesn't pump trace; loses trace
    (is (= 1 (:agenda-point (get-corp))) "Corp should score a 1-point agenda")
    (is (zero? (:bad-publicity (get-corp))) "Corp should gain 0 bad publicity")
    (is (= 3 (:credit (get-corp))) "Corp should gain 0 credits")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (score-agenda state :corp (get-content state :remote3 0))
    (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
    (click-prompt state :runner "3")  ;; Runner pumps trace; wins trace
    (is (= 2 (:agenda-point (get-corp))) "Corp should score a 1-point agenda")
    (is (= 1 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity from failed trace")
    (is (= 10 (:credit (get-corp))) "Corp should gain 7 credits")))
