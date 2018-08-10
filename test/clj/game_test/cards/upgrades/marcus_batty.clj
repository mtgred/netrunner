(ns game-test.cards.upgrades.marcus-batty
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest marcus-batty
  ;; Marcus Batty
  (testing "Simultaneous Interaction with Security Nexus"
    (do-game
      (new-game {:corp {:deck ["Marcus Batty" "Enigma"]}
                 :runner {:deck ["Security Nexus"]}})
      (play-from-hand state :corp "Marcus Batty" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 8)
      (play-from-hand state :runner "Security Nexus")
      (let [mb (get-content state :hq 0)
            en (get-ice state :hq 0)
            sn (-> @state :runner :rig :hardware first)]
        (run-on state "HQ")
        (core/rez state :corp mb)
        (core/rez state :corp en)
        (card-ability state :corp mb 0)
        (card-ability state :runner sn 0)
        ;; both prompts should be on Batty
        (is (prompt-is-card? state :corp mb) "Corp prompt is on Marcus Batty")
        (is (prompt-is-card? state :runner mb) "Runner prompt is on Marcus Batty")
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (prompt-is-card? state :corp sn) "Corp prompt is on Security Nexus")
        (is (prompt-is-type? state :runner :waiting) "Runner prompt is waiting for Corp")))))
