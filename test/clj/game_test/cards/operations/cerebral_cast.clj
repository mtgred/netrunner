(ns game-test.cards.operations.cerebral-cast
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cerebral-cast
  ;; Cerebral Cast
  (testing "Runner wins"
    (do-game
      (new-game {:corp {:deck ["Cerebral Cast"]}})
      (play-from-hand state :corp "Cerebral Cast")
      (is (= 3 (:click (get-corp))) "Cerebral Cast precondition not met; card not played")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (zero? (count (:discard (get-runner)))) "Runner took no damage")
      (is (zero? (:tag (get-runner))) "Runner took no tags")))
  (testing "Corp wins"
    (do-game
      (new-game {:corp {:deck [(qty "Cerebral Cast" 2)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "1 brain damage")
      (is (= 1 (count (:discard (get-runner)))) "Runner took a brain damage")
      (is (zero? (:tag (get-runner))) "Runner took no tags from brain damage choice")
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "1 tag")
      (is (= 1 (count (:discard (get-runner)))) "Runner took no additional damage")
      (is (= 1 (:tag (get-runner))) "Runner took a tag from Cerebral Cast choice"))))
