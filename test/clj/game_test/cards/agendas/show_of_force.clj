(ns game-test.cards.agendas.show-of-force
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest show-of-force
  ;; Show of Force
  (do-game
    (new-game {:corp {:deck ["Show of Force"]}})
    (is (= 3 (count (:hand (get-runner)))) "Runner should start with 3 cards in hand")
    (play-and-score state "Show of Force")
    (is (= 1 (count (:hand (get-runner)))) "Runner should have 1 card in hand")
    (is (= 2 (count (:discard (get-runner)))) "Runner should have discarded 2 cards")))
