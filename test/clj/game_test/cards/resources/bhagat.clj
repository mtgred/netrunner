(ns game-test.cards.resources.bhagat
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bhagat
  ;; Bhagat - only trigger on first run
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3) (qty "Architect" 3)]}
               :runner {:deck ["Bhagat"]}})
    (starting-hand state :corp [])
    (take-credits state :corp)
    (run-empty-server state :hq)
    (play-from-hand state :runner "Bhagat")
    (run-empty-server state :hq)
    (is (empty? (:discard (get-corp))) "Bhagat did not trigger on second successful run")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-corp)))) "Bhagat milled one card")))
