(ns game-test.cards.icebreakers.shiv
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest shiv
  ;; Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link
  (do-game
    (new-game {:runner {:id "Nasir Meidan: Cyber Explorer"
                        :deck ["Shiv" (qty "Inti" 2)
                               "Access to Globalsec"]}})
    (is (= 1 (:link (get-runner))) "1 link")
    (take-credits state :corp)
    (play-from-hand state :runner "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :runner "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :runner "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (core/available-mu state)) "3 MU consumed")
      (play-from-hand state :runner "Access to Globalsec")
      (is (= 2 (:link (get-runner))) "2 link")
      (is (= 2 (core/available-mu state)) "Shiv stops using MU when 2+ link"))))
