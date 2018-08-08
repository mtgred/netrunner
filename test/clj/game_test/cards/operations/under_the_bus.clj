(ns game-test.cards.operations.under-the-bus
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest under-the-bus
  ;; Under the Bus
  (do-game
    (new-game {:corp {:deck ["Under the Bus"]}
               :runner {:deck ["Film Critic"]}})
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Film Critic")
    (take-credits state :runner)
    (is (= 1 (count (get-resource state))) "Runner has 1 resource installed")
    (is (zero? (:bad-publicity (get-corp))) "Corp has no bad pub")
    (play-from-hand state :corp "Under the Bus")
    (click-card state :corp (get-resource state 0))
    (is (empty? (get-resource state)) "Runner has no resource installed")
    (is (= 1 (count (:discard (get-runner)))) "Runner has 1 trashed card")
    (is (= 1 (:bad-publicity (get-corp))) "Corp takes 1 bad pub")))
