(ns game-test.cards.hardware.net-ready-eyes
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest net-ready-eyes
  ;; Net-Ready Eyes
  (do-game
    (new-game {:runner {:deck [(qty "Sure Gamble" 3) "Net-Ready Eyes" "Peacock"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Peacock")
    (play-from-hand state :runner "Net-Ready Eyes")
    (is (= 3 (count (:discard (get-runner)))) "Took 2 damage on NRE install")
    (run-on state "HQ")
    (let [pea (get-program state 0)]
      (click-card state :runner pea)
      (is (= 3 (:current-strength (refresh pea))) "Peacock strength boosted")
      (run-continue state)
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (= 2 (:current-strength (refresh pea))) "Peacock strength back to default"))))
