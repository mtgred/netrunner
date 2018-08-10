(ns game-test.cards.operations.wake-up-call
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest wake-up-call
  ;; Wake Up Call
  (testing "should fire after using En Passant to trash ice"
    (do-game
      (new-game {:corp {:deck ["Enigma" "Wake Up Call"]}
                 :runner {:deck ["En Passant" "Maya"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Maya")
      (run-on state :hq)
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "Corp starts with no discards")
      (play-from-hand state :runner "En Passant")
      (click-card state :runner (get-ice state :hq 0))
      (is (= 1 (count (:discard (get-corp)))) "Corp trashes installed ice")
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))) "Runner starts with 1 trashed card (En Passant)")
      (play-from-hand state :corp "Wake Up Call")
      (click-card state :corp (get-hardware state 0))
      (click-prompt state :runner "Trash Maya")
      (is (= 2 (count (:discard (get-runner)))) "Maya is trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Wake Up Call is removed from the game"))))
