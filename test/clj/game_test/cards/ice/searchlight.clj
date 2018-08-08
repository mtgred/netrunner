(ns game-test.cards.ice.searchlight
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest searchlight
  ;; Searchlight - Trace bace equal to advancement counters
  (do-game
    (new-game {:corp {:deck ["Searchlight"]}})
    (play-from-hand state :corp "Searchlight" "HQ")
    (let [searchlight (get-ice state :hq 0)]
      (core/rez state :corp searchlight)
      (card-subroutine state :corp (refresh searchlight) 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (zero? (:tag (get-runner))) "Trace failed with 0 advancements")
      (advance state searchlight 1)
      (card-subroutine state :corp (refresh searchlight) 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (:tag (get-runner))) "Trace succeeds with 1 advancement"))))
