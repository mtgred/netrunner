(ns game-test.cards.agendas.jumon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jumon
  ;; Jumon
  (do-game
    (new-game {:corp {:deck ["Jumon" "Ice Wall" "Crisium Grid" "Project Atlas"]}})
    (play-and-score state "Jumon")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Project Atlas" "Server 2")
    (core/end-turn state :corp nil)
    (let [pa (get-content state :remote2 0)
          iw (get-ice state :remote2 0)]
      (is (zero? (get-counters (refresh pa) :advancement)) "Project Atlas starts with no counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall starts with no counters")
      (click-card state :corp iw)
      (click-card state :corp pa)
      (is (= 2 (get-counters (refresh pa) :advancement)) "Project Atlas gains 2 counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall doesn't gain any counters")
      (core/start-turn state :runner nil)
      (take-credits state :runner)
      (play-from-hand state :corp "Crisium Grid" "Server 2")
      (let [cg (get-content state :remote2 1)]
        (is (zero? (get-counters (refresh cg) :advancement)) "Crisium Grid starts with no counters")
        (core/end-turn state :corp nil)
        (click-card state :corp cg)
        (is (= 2 (get-counters (refresh cg) :advancement)) "Crisium Grid gains 2 counters")))))
