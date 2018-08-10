(ns game-test.cards.agendas.remote-data-farm
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest remote-data-farm
  ;; Remote Data Farm
  (do-game
    (new-game {:corp {:deck ["Remote Data Farm"]}})
    (is (= 5 (get-hand-size :corp)))
    (play-and-score state "Remote Data Farm")
    (is (= 7 (get-hand-size :corp)))))
