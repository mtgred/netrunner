(ns game-test.cards.ice.holmegaard
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest holmegaard
  ;; Holmegaard - Stop Runner from accessing cards if win trace
  (do-game
    (new-game {:corp {:deck ["Holmegaard" "Hostile Takeover"]}
               :runner {:deck ["Cache" "Inti"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Holmegaard" "HQ")
    (let [holm (get-ice state :hq 0)]
      (core/rez state :corp holm)
      (take-credits state :corp)
      (play-from-hand state :runner "Inti")
      (play-from-hand state :runner "Cache")
      (run-on state "HQ")
      (card-subroutine state :corp holm 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (card-subroutine state :corp holm 1)
      (click-card state :corp "Cache")
      (is (empty? (:discard (get-runner))) "Can't target non-icebreaker program")
      (click-card state :corp "Inti")
      (is (= 1 (count (:discard (get-runner)))) "Inti trashed")
      (run-continue state)
      (run-successful state)
      ;; Prompt for "you cannot access any card this run"
      (click-prompt state :runner "OK")
      (is (not (accessing state "Hostile Takeover"))))))
