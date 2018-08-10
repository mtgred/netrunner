(ns game-test.cards.operations.hellion-alpha-test
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hellion-alpha-test
  ;; Hellion Alpha Test
  (do-game
    (new-game {:corp {:deck [(qty "Hellion Alpha Test" 2)]}
               :runner {:deck ["Daily Casts"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (play-from-hand state :corp "Hellion Alpha Test")
    (is (zero? (-> (get-runner) :deck count)) "Runner should have no cards in Stack")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (is (= 1 (-> (get-runner) :deck count)) "Runner should have 1 card in Stack from losing Hellion Alpha Test trace")
    (is (= "Daily Casts" (-> (get-runner) :deck first :title))
        "Runner should have Daily Casts on top of Stack from losing Hellion Alpha Test trace")
    (take-credits state :corp)
    (core/draw state :runner)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (play-from-hand state :corp "Hellion Alpha Test")
    (is (zero? (:bad-publicity (get-corp))) "Corp should start with 0 bad publicity")
    (click-prompt state :corp "0")
    (click-prompt state :runner "2")
    (is (= 1 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity from losing Hellion Alpha Test trace")))
