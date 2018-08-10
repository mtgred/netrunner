(ns game-test.cards.assets.c-i-fund
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest c-i-fund
  ;; C.I. Fund
  (do-game
    (new-game {:corp {:deck ["C.I. Fund" "Hedge Fund"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "C.I. Fund" "New remote")
    (take-credits state :corp)
    (let [ci (get-content state :remote1 0)]
      (core/rez state :corp ci)
      (take-credits state :runner)
      (card-ability state :corp ci 0)
      (click-prompt state :corp "3")
      (is (= 3 (get-counters (refresh ci) :credit)))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp ci 0)
      (click-prompt state :corp "3")
      (is (= 6 (get-counters (refresh ci) :credit)))
      (core/end-phase-12 state :corp nil)
      (is (= 8 (get-counters (refresh ci) :credit)))
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 10 (get-counters (refresh ci) :credit)))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp ci 1)
        (is (= 8 (- (:credit (get-corp)) credits)))
        (is (zero? (get-counters (refresh ci) :credit)))))))
