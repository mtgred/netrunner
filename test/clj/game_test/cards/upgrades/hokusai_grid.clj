(ns game-test.cards.upgrades.hokusai-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hokusai-grid
  ;; Hokusai Grid - Do 1 net damage when run successful on its server
  (do-game
    (new-game {:corp {:deck ["Hokusai Grid"]}})
    (play-from-hand state :corp "Hokusai Grid" "HQ")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :hq 0))
    (run-empty-server state :rd)
    (is (empty? (:discard (get-runner))) "No net damage done for successful run on R&D")
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-runner)))) "1 net damage done for successful run on HQ")))
