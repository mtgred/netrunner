(ns game-test.cards.hardware.replicator
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest replicator
  ;; Replicator
  (testing "interaction with Bazaar. Issue #1511"
    (do-game
      (new-game {:runner {:deck ["Replicator" "Bazaar" (qty "Spy Camera" 6)]}})
      (letfn [(count-spy [n] (= n (count (filter #(= "Spy Camera" (:title %)) (-> (get-runner) :rig :hardware)))))]
        (take-credits state :corp)
        (starting-hand state :runner ["Replicator" "Bazaar" "Spy Camera"])
        (play-from-hand state :runner "Replicator")
        (play-from-hand state :runner "Bazaar")
        (play-from-hand state :runner "Spy Camera") ; 1 installed
        (is (count-spy 1) "1 Spy Cameras installed")
        (click-prompt state :runner "Yes") ; for now, choosing Replicator then shows its optional Yes/No
        (click-prompt state :runner "Yes") ; Bazaar triggers, 2 installed
        (is (count-spy 2) "2 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 3 installed
        (is (count-spy 3) "3 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 4 installed
        (is (count-spy 4) "4 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 5 installed
        (is (count-spy 5) "5 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 6 installed
        (is (count-spy 6) "6 Spy Cameras installed")))))
