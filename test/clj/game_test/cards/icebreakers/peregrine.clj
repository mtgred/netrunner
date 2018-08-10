(ns game-test.cards.icebreakers.peregrine
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest peregrine
  ;; Peregrine - 2c to return to grip and derez an encountered code gate
  (do-game
    (new-game {:corp {:deck ["Paper Wall" (qty "Bandwidth" 2)]}
               :runner {:deck ["Peregrine"]}})
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Paper Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Peregrine")
    (let [bw1 (get-ice state :archives 0)
          pw (get-ice state :archives 2)
          per (get-program state 0)]
      (run-on state "Archives")
      (core/rez state :corp pw)
      (core/rez state :corp bw1)
      (card-ability state :runner per 2)
      (is (and (= 2 (:credit (get-runner))) (empty? (:hand (get-runner)))) "Can't use Peregrine on a barrier")
      (run-continue state)
      (card-ability state :runner per 2)
      (is (and (= 2 (:credit (get-runner))) (empty? (:hand (get-runner)))) "Can't use Peregrine on unrezzed code gate")
      (run-continue state)
      (card-ability state :runner per 2)
      (is (zero? (:credit (get-runner))) "Spent 2 credits")
      (is (= 1 (count (:hand (get-runner)))) "Peregrine returned to grip")
      (is (not (:rezzed (refresh bw1))) "Bandwidth derezzed"))))
