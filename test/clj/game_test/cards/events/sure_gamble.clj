(ns game-test.cards.events.sure-gamble
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sure-gamble
  ;; Sure Gamble
  (do-game
    (new-game {:runner {:deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "Sure Gamble")
    (is (= 9 (:credit (get-runner))))))
