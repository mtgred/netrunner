(ns game-test.cards.agendas.sentinel-defense-program
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sentinel-defense-program
  ;; Sentinel Defense Program - Doesn't fire if brain damage is prevented
  (do-game
    (new-game {:corp {:deck ["Sentinel Defense Program" "Viktor 1.0"]}
               :runner {:deck ["Feedback Filter" (qty "Sure Gamble" 3)]}})
    (play-and-score state "Sentinel Defense Program")
    (play-from-hand state :corp "Viktor 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (let [viktor (get-ice state :hq 0)
          ff (get-hardware state 0)]
      (run-on state "HQ")
      (core/rez state :corp viktor)
      (card-subroutine state :corp viktor 0)
      (click-prompt state :runner "Done")  ;; Don't prevent the brain damage
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 1 (:brain-damage (get-runner))))
      (click-prompt state :runner "Done")  ;; So we take the net, but don't prevent it either
      (is (= 2 (count (:discard (get-runner)))))
      (card-subroutine state :corp viktor 0)
      (card-ability state :runner ff 1)  ;; Prevent the brain damage this time
      (click-prompt state :runner "Done")
      (is (= 3 (count (:discard (get-runner)))) "Feedback filter trashed, didn't take another net damage")
      (is (= 1 (:brain-damage (get-runner)))))))
