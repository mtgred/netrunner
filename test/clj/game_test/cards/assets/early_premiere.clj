(ns game-test.cards.assets.early-premiere
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest early-premiere
  ;; Early Premiere - Pay 1c at start of turn to place an advancement on a card in a server
  (do-game
    (new-game {:corp {:deck ["Early Premiere" "Ice Wall"
                             "Ghost Branch" "Blacklist"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Early Premiere" "New remote")
    (play-from-hand state :corp "Blacklist" "New remote")
    (play-from-hand state :corp "Ghost Branch" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ep (get-content state :remote1 0)
          bl (get-content state :remote2 0)
          gb (get-content state :remote3 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp ep)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp ep 0)
      (click-card state :corp iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall can't targeted, not in server")
      (click-card state :corp bl)
      (is (zero? (get-counters (refresh bl) :advancement)) "Blacklist can't targeted, can't be advanced")
      (click-card state :corp gb)
      (is (= 1 (get-counters (refresh gb) :advancement)) "1 advancement on Ghost Branch")
      (is (= 4 (:credit (get-corp)))))))
