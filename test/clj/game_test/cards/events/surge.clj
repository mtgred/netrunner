(ns game-test.cards.events.surge
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest surge
  ;; Surge - Add counters if target is a virus and had a counter added this turn
  (testing "Valid target"
    (do-game
      (new-game {:runner {:deck ["Imp" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
        (play-from-hand state :runner "Surge")
        (click-card state :runner imp)
        (is (= 4 (get-counters (refresh imp) :virus)) "Imp has 4 counters after surge"))))
  (testing "Don't fire surge if target is not a virus"
    (do-game
      (new-game {:runner {:deck ["Security Testing" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (let [st (get-resource state 0)]
        (play-from-hand state :runner "Surge")
        (click-card state :runner st)
        (is (not (contains? st :counter)) "Surge does not fire on Security Testing"))))
  (testing "Don't fire surge if target does not have virus counter flag set"
    (do-game
      (new-game {:runner {:deck ["Imp" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (play-from-hand state :runner "Surge")
        (click-card state :runner imp)
        (is (= 2 (get-counters (refresh imp) :virus)) "Surge does not fire on Imp turn after install"))))
  (testing "Don't allow surging Gorman Drip, since it happens on the corp turn"
    (do-game
      (new-game {:runner {:deck ["Gorman Drip v1" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gorman Drip v1")
      (let [gd (get-program state 0)]
        (is (zero? (get-counters gd :virus)) "Gorman Drip starts without counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh gd) :virus))
            "Gorman Drip gains 3 counters after Corp clicks 3 times for credits")
        (play-from-hand state :runner "Surge")
        (click-card state :runner gd)
        (is (= 3 (get-counters (refresh gd) :virus)) "Surge does not trigger on Gorman Drip")))))
