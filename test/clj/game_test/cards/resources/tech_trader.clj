(ns game-test.cards.resources.tech-trader
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tech-trader
  ;; Basic test
  (do-game
    (new-game {:runner {:deck ["Tech Trader" "Fall Guy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Tech Trader")
    (play-from-hand state :runner "Fall Guy")
    (is (= 4 (:credit (get-runner))))
    (let [fall (get-resource state 1)]
      (card-ability state :runner fall 1)
      (is (= 7 (:credit (get-runner)))))))
