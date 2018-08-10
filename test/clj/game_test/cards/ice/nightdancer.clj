(ns game-test.cards.ice.nightdancer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nightdancer
  ;; Nightdancer - Runner loses a click if able, corp gains a click on next turn
  (do-game
    (new-game {:corp {:deck ["Nightdancer"]}})
    (play-from-hand state :corp "Nightdancer" "HQ")
    (take-credits state :corp)
    (let [nd (get-ice state :hq 0)]
      (core/rez state :corp nd)
      (run-on state "HQ")
      (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
      (card-subroutine state :corp nd 0)
      (is (= 2 (:click (get-runner))) "Runner lost 1 click")
      (card-subroutine state :corp nd 0)
      (is (= 1 (:click (get-runner))) "Runner lost 1 click")
      (run-jack-out state)
      (take-credits state :runner)
      (is (= 5 (:click (get-corp))) "Corp has 5 clicks"))))
