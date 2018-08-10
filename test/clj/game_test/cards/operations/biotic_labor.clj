(ns game-test.cards.operations.biotic-labor
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest biotic-labor
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game {:corp {:deck ["Biotic Labor"]}})
    (play-from-hand state :corp "Biotic Labor")
    (is (= 1 (:credit (get-corp))))
    (is (= 4 (:click (get-corp))) "Spent 1 click to gain 2 additional clicks")))
