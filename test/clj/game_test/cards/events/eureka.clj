(ns game-test.cards.events.eureka
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest eureka
  ;; Eureka! - Install the program but trash the event
  (do-game
    (new-game {:runner {:deck [(qty "Eureka!" 2) "Torch" "Sure Gamble"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 1)
    (core/move state :runner (find-card "Torch" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Eureka!")
    (click-prompt state :runner "Yes")
    (is (= 3 (:credit (get-runner))))
    (is (= 1 (count (get-program state))))
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Eureka!")
    (is (zero? (:credit (get-runner))))
    (is (= 3 (count (:discard (get-runner)))))))
