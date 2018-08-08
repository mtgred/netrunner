(ns game-test.cards.hardware.dorm-computer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game {:corp {:deck ["Snare!"]}
               :runner {:deck ["Dorm Computer"]}})
    (play-from-hand state :corp "Snare!" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (let [dorm (get-hardware state 0)]
      (card-ability state :runner dorm 0)
      (click-prompt state :runner "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (is (zero? (:tag (get-runner))) "Runner has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))))
