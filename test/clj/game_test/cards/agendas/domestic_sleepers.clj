(ns game-test.cards.agendas.domestic-sleepers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest domestic-sleepers
  ;; Domestic Sleepers
  (do-game
    (new-game {:corp {:deck ["Domestic Sleepers"]}})
    (play-and-score state "Domestic Sleepers")
    (core/gain state :corp :click 3)
    (let [ds_scored (get-scored state :corp 0)]
      (is (zero? (get-counters (refresh ds_scored) :agenda)) "Should start with 0 agenda counters")
      (is (zero? (:agenda-point (get-corp))) "Should provide 0 agenda points initially")
      (card-ability state :corp ds_scored 0)
      (is (= 1 (get-counters (refresh ds_scored) :agenda)) "Should gain 1 agenda counter")
      (is (= 1 (:agenda-point (get-corp))) "Should provide 1 agenda point after ability use"))))
