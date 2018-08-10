(ns game-test.cards.icebreakers.inversificator
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest inversificator
  ;; Inversificator shouldn't hook up events for unrezzed ice
  (do-game
    (new-game {:corp {:deck ["Turing" "Kakugo"]}
               :runner {:deck ["Inversificator" "Sure Gamble"]}})
    (play-from-hand state :corp "Kakugo" "HQ")
    (play-from-hand state :corp "Turing" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Inversificator")
    (let [inv (get-program state 0)
          tur (get-ice state :hq 1)]
      (is (= 1 (count (:hand (get-runner)))) "Runner starts with 1 card in hand")
      (run-on state :hq)
      (core/rez state :corp (refresh tur))
      (run-continue state)
      (card-ability state :runner (refresh inv) 0)
      (click-card state :runner (get-ice state :hq 1))
      (click-card state :runner (get-ice state :hq 0))
      (run-jack-out state)
      (is (= 1 (count (:hand (get-runner)))) "Runner still has 1 card in hand")
      (run-on state :hq)
      (run-continue state)
      (is (= 1 (count (:hand (get-runner)))) "Kakugo doesn't fire when unrezzed"))))
