(ns game-test.cards.agendas.self-destruct-chips
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest self-destruct-chips
  ;; Self-Destruct Chips
  (do-game
    (new-game {:corp {:deck ["Self-Destruct Chips"]}})
    (is (= 5 (get-hand-size :runner)) "Runner's hand size starts at 5")
    (play-and-score state "Self-Destruct Chips")
    (is (= 4 (get-hand-size :runner)) "By scoring Self-Destruct Chips, Runner's hand size is reduced by 1")))
