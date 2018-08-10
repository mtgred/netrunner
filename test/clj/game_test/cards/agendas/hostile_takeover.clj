(ns game-test.cards.agendas.hostile-takeover
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hostile-takeover
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover"]}})
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))) "Gain 7 credits")
    (is (= 1 (:bad-publicity (get-corp))) "Take 1 bad publicity")))
