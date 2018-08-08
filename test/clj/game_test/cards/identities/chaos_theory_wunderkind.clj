(ns game-test.cards.identities.chaos-theory-wunderkind
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest chaos-theory-wunderkind
  ;; Chaos Theory, start with +1 MU
  (do-game
    (new-game {:runner {:id "Chaos Theory: Wünderkind"}})
    (is (= 5 (core/available-mu state)) "Chaos Theory starts the game with +1 MU")))
