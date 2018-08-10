(ns game-test.cards.ice.envelope
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest envelope
  ;; Envelope - do 1 net damage, end the run
  (do-game
    (new-game {:corp {:deck ["Envelope"]}})
    (play-from-hand state :corp "Envelope" "HQ")
    (take-credits state :corp)
    (let [envl (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp envl)
      (is (zero? (count (:discard (get-runner)))) "No discarded cards")
      (card-subroutine state :corp envl 0)
      (is (= 1 (count (:discard (get-runner)))) "1 card in discard pile")
      (is (:run @state) "Run still ongoing")
      (card-subroutine state :corp envl 1)
      (is (not (:run @state)) "Run ended"))))
