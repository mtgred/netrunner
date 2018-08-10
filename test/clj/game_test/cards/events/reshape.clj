(ns game-test.cards.events.reshape
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reshape
  ;; Reshape - Swap 2 pieces of unrezzed ICE
  (do-game
    (new-game {:corp {:deck [(qty "Vanilla" 2) "Paper Wall"]}
               :runner {:deck ["Reshape"]}})
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Vanilla" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Reshape")
    (click-card state :runner (get-ice state :rd 0))
    (click-card state :runner (get-ice state :hq 0))
    (is (:prompt (get-runner)) "Can't target rezzed Vanilla, prompt still open")
    (click-card state :runner (get-ice state :hq 1))
    (is (empty? (:prompt (get-runner))))
    (is (= "Vanilla" (:title (get-ice state :rd 0))) "Vanilla swapped to R&D")
    (is (= "Paper Wall" (:title (get-ice state :hq 1))) "Paper Wall swapped to HQ outer position")))
