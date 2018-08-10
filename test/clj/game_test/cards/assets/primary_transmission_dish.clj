(ns game-test.cards.assets.primary-transmission-dish
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest primary-transmission-dish
  ;; Primary Transmission Dish
  (do-game
    (new-game {:corp {:deck ["Primary Transmission Dish"]}})
    (play-from-hand state :corp "Primary Transmission Dish" "New remote")
    (let [dish (get-content state :remote1 0)]
      (core/rez state :corp dish)
      (is (= 3 (get-counters (refresh dish) :recurring)) "Should have 3 recurring credits"))))
