(ns game-test.cards.resources.the-source
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-source
  ;; The Source - Increase advancement requirement of agendas by 1; 3c additional cost to steal
  (do-game
    (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
               :runner {:deck [(qty "The Source" 2) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "The Source")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay 3 [Credits] to steal") ; pay 3c extra to steal
    (is (= 4 (:credit (get-runner))) "Paid 3c to steal")
    (is (= 2 (count (:discard (get-runner)))) "The Source is trashed")
    (play-from-hand state :runner "The Source")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote2 0)]
      (core/advance state :corp {:card (refresh ht)})
      (core/advance state :corp {:card (refresh ht)})
      (core/score state :corp {:card (refresh ht)})
      (is (empty? (:scored (get-corp))) "Hostile Takeover can't be scored with 2 adv")
      (core/gain state :corp :click 1)
      (core/advance state :corp {:card (refresh ht)})
      (core/score state :corp {:card (refresh ht)})
      (is (= 1 (:agenda-point (get-corp))) "Hostile Takeover scored with 3 adv")
      (is (= 3 (count (:discard (get-runner)))) "The Source is trashed"))))
