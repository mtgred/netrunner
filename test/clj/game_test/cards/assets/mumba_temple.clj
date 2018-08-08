(ns game-test.cards.assets.mumba-temple
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mumba-temple
  ;; Mumba Temple
  (do-game
    (new-game {:corp {:deck ["Mumba Temple"]}})
    (play-from-hand state :corp "Mumba Temple" "New remote")
    (let [mumba (get-content state :remote1 0)]
      (core/rez state :corp mumba)
      (is (= 2 (get-counters (refresh mumba) :recurring)) "Should have 2 recurring credits"))))
