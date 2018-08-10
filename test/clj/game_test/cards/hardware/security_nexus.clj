(ns game-test.cards.hardware.security-nexus
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest security-nexus
  ;; Security Nexus
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Security Nexus"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Security Nexus")
    (let [nexus (get-hardware state 0)]
      (run-on state :rd)
      (card-ability state :runner nexus 0)
      (is (zero? (:tag (get-runner))) "Runner should have no tags to start")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (not (:run @state)) "Run should end from losing Security Nexus trace")
      (is (= 1 (:tag (get-runner))) "Runner should take 1 tag from losing Security Nexus trace")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :rd)
      (card-ability state :runner nexus 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (:run @state) "Run should still be going on from winning Security Nexus trace")
      (is (= 1 (:tag (get-runner))) "Runner should still only have 1 tag"))))
