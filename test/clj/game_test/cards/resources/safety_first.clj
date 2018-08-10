(ns game-test.cards.resources.safety-first
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest safety-first
  ;; Safety First - Reduce hand size by 2, draw 1 at turn end if below maximum
  (do-game
    (new-game {:runner {:deck [(qty "Safety First" 3) (qty "Cache" 3)]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Safety First" "Safety First" "Cache"])
    (play-from-hand state :runner "Safety First")
    (is (= 3 (core/hand-size state :runner)) "Max hand size reduced by 2")
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Drew 1 card at end of turn")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Drew no cards, at maximum")))
