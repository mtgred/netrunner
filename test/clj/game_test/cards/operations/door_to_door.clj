(ns game-test.cards.operations.door-to-door
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest door-to-door
  ;; Door to Door
  (do-game
    (new-game {:corp {:deck ["Door to Door"]}})
    (play-from-hand state :corp "Door to Door")
    (take-credits state :corp)
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (is (= 3 (-> (get-runner) :hand count)) "Runner should start with 3 cards in hand")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:tag (get-runner))) "Runner should gain 1 tag from Door to Door")
    (is (= 3 (-> (get-runner) :hand count)) "Runner should start with 3 cards in hand")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:tag (get-runner))) "Runner should still have 1 tag")
    (is (= 2 (-> (get-runner) :hand count)) "Runner should take 1 meat damage from Door to Door")))
