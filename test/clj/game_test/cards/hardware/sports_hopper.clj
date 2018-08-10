(ns game-test.cards.hardware.sports-hopper
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sports-hopper
  ;; Sports Hopper
  (do-game
    (new-game {:runner {:deck [(qty "Sports Hopper" 3) (qty "Sure Gamble" 3)]}})
    (starting-hand state :runner ["Sports Hopper"])
    (take-credits state :corp)
    (play-from-hand state :runner "Sports Hopper")
    (is (= 1 (:link (get-runner))) "Gained 1 link")
    (card-ability state :runner (get-hardware state 0) 0)
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (zero? (:link (get-runner))) "Lost link")))
