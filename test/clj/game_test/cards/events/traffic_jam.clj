(ns game-test.cards.events.traffic-jam
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest traffic-jam
  ;; Traffic Jam - Increase adv requirement based on previously scored copies
  (do-game
    (new-game {:corp {:deck [(qty "TGTBT" 3)]}
               :runner {:deck ["Traffic Jam"]}})
    (play-from-hand state :corp "TGTBT" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "TGTBT" "New remote")
    (score-agenda state :corp (get-content state :remote2 0))
    (play-from-hand state :corp "TGTBT" "New remote")
    (take-credits state :corp)
    (let [tg (get-content state :remote3 0)]
      (play-from-hand state :runner "Traffic Jam")
      (take-credits state :runner)
      (core/gain state :corp :click 2)
      (advance state tg 3)
      (core/score state :corp {:card (refresh tg)})
      (is (= 2 (:agenda-point (get-corp))) "Last TGTBT not scored")
      (is (= 1 (count (get-content state :remote3))))
      (advance state (refresh tg) 1)
      (is (= 4 (get-counters (refresh tg) :advancement)))
      (core/score state :corp {:card (refresh tg)})
      (is (= 2 (:agenda-point (get-corp))) "Not scored with 4 advancements")
      (advance state (refresh tg) 1)
      (is (= 5 (get-counters (refresh tg) :advancement)))
      (core/score state :corp {:card (refresh tg)})
      (is (= 3 (:agenda-point (get-corp))) "Took 5 advancements to score"))))
