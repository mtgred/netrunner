(ns game-test.cards.operations.hard-hitting-news
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hard-hitting-news
  ;; Hard-Hitting News
  (do-game
    (new-game {:corp {:deck ["Hard-Hitting News"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should start with 3 clicks")
    (play-from-hand state :corp "Hard-Hitting News")
    (is (zero? (:click (get-corp))) "Playing Hard-Hitting News should lose all remaining clicks")
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 4 (:tag (get-runner))) "Runner should gain 4 tags from losing Hard-Hitting News trace")))
