(ns game-test.cards.resources.donut-taganes
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest donut-taganes
  ;; Donut Taganes - add 1 to play cost of Operations & Events when this is in play
  (do-game
    (new-game {:runner {:deck ["Donut Taganes" "Easy Mark"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Donut Taganes")
    (is (= 2 (:credit (get-runner))) "Donut played for 3c")
    (play-from-hand state :runner "Easy Mark")
    (is (= 4 (:credit (get-runner))) "Easy Mark only gained 2c")
    (take-credits state :runner)
    (is (= 8 (:credit (get-corp))) "Corp has 8c")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 11 (:credit (get-corp))) "Corp has 11c")))
