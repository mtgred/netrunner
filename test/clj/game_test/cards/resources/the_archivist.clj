(ns game-test.cards.resources.the-archivist
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-archivist
  ;; The Archivist
  (do-game
    (new-game {:corp {:deck ["Global Food Initiative" "Private Security Force"]}
               :runner {:deck ["The Archivist"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Archivist")
    (is (zero? (:bad-publicity (get-corp))) "Corp should start with 0 bad publicity")
    (take-credits state :runner)
    (play-and-score state "Global Food Initiative")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:bad-publicity (get-corp))) "Corp should get 1 bad publicity from The Archivist")
    (play-and-score state "Private Security Force")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 2 (:bad-publicity (get-corp))) "Corp should get 1 bad publicity from The Archivist")))
