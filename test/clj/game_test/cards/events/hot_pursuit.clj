(ns game-test.cards.events.hot-pursuit
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hot-pursuit
  ;; Hot Pursuit
  (do-game
    (new-game {:runner {:deck ["Hot Pursuit"]}})
    (take-credits state :corp)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= (+ 5 -2 9) (:credit (get-runner))) "Gained 9 credits on successful run")
    (is (= 1 (:tag (get-runner))) "Took 1 tag on successful run")
    (is (prompt-map :runner) "Still have access prompt")))
