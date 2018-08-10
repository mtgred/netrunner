(ns game-test.cards.events.guinea-pig
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest guinea-pig
  ;; Guinea Pig
  (do-game
    (new-game {:runner {:deck ["Guinea Pig" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Guinea Pig")
    (is (= 11 (:credit (get-runner))) "Gained +6 credits from playing Guinea Pig")
    (is (empty? (:hand (get-runner))) "No cards left in grip, trashed all cards due to Guinea Pig")
    (is (= 4 (count (:discard (get-runner)))) "3 cards trashed from Guinea Pig + Guinea Pig itself")))
