(ns game-test.cards.operations.snatch-and-grab
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest snatch-and-grab
  ;; Snatch and Grab
  (do-game
    (new-game {:corp {:deck [(qty "Snatch and Grab" 2)]}
               :runner {:deck ["Scrubber"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scrubber")
    (take-credits state :runner)
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (play-from-hand state :corp "Snatch and Grab")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (click-prompt state :runner "Yes")
    (is (= 1 (:tag (get-runner))) "Runner should get 1 tag from losing Snatch and Grab trace and opting to take the tag")
    (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
    (play-from-hand state :corp "Snatch and Grab")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (click-prompt state :runner "No")
    (is (= 1 (-> (get-runner) :discard count)) "Scrubber should be in Runner's heap after losing Snatch and Grab trace")))
