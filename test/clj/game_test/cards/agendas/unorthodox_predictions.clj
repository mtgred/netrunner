(ns game-test.cards.agendas.unorthodox-predictions
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest unorthodox-predictions
  ;; Unorthodox Predictions
  (do-game
    (new-game {:corp {:deck ["Unorthodox Predictions"]}})
    (play-and-score state "Unorthodox Predictions")
    (click-prompt state :corp "Barrier")
    (is (last-log-contains? state "Barrier"))))
