(ns game-test.cards.upgrades.keegan-lane
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest keegan-lane
  ;; Keegan Lane - Trash self and remove 1 Runner tag to trash a program
  (do-game
    (new-game {:corp {:deck ["Keegan Lane"]}
               :runner {:deck ["Corroder"]}})
    (play-from-hand state :corp "Keegan Lane" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state :hq)
    (let [keeg (get-content state :hq 0)]
      (core/rez state :corp keeg)
      (card-ability state :corp keeg 0)
      (is (= 1 (count (get-content state :hq))) "Keegan didn't fire, Runner has no tags")
      (core/gain state :runner :tag 2)
      (card-ability state :corp keeg 0)
      (click-card state :corp (get-program state 0))
      (is (= 1 (:tag (get-runner))) "1 tag removed")
      (is (= 1 (count (:discard (get-corp)))) "Keegan trashed")
      (is (= 1 (count (:discard (get-runner)))) "Corroder trashed"))))
