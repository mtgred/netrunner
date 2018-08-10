(ns game-test.cards.programs.false-echo
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest false-echo
  ;; False Echo - choice for Corp
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
               :runner {:deck [(qty "False Echo" 3)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "False Echo")
    (play-from-hand state :runner "False Echo")
    (run-on state "Archives")
    (run-continue state)
    (let [echo1 (get-program state 0)
          echo2 (get-program state 1)]
      (card-ability state :runner echo1 0)
      (click-prompt state :corp "Add to HQ")
      (is (= 2 (count (:hand (get-corp)))) "Ice Wall added to HQ")
      (is (= 1 (count (:discard (get-runner)))) "False Echo trashed")
      (run-continue state)
      (card-ability state :runner echo2 0)
      (click-prompt state :corp "Rez")
      (is (:rezzed (get-ice state :archives 0)) "Ice Wall rezzed")
      (is (= 2 (count (:discard (get-runner)))) "False Echo trashed"))))
