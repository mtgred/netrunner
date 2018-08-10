(ns game-test.cards.events.game-day
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest game-day
  ;; Game Day - draw until at handsize
  (do-game
    (new-game {:runner {:deck [(qty "Game Day" 3)
                               (qty "Public Sympathy" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Easy Mark" 3)]}})
    (take-credits state :corp)
    ;; move needed cards to hand -- in case they were not drawn
    (core/move state :runner (find-card "Game Day" (:deck (get-runner))) :hand)
    (core/move state :runner (find-card "Public Sympathy" (:deck (get-runner))) :hand)
    (play-from-hand state :runner "Public Sympathy")
    (is (= 7 (core/hand-size state :runner)) "Runner hand size is 7")
    (play-from-hand state :runner "Game Day")
    (is (= 7 (count (:hand (get-runner)))) "Drew up to 7 cards")))
