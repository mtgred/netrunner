(ns game-test.cards.events.contaminate
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest contaminate
  ;; Contaminate - add 3 virus counters to an installed runner card with no virus counters
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Yusuf" "Chrome Parlor" (qty "Contaminate" 3)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5 :click 2)
      (play-from-hand state :runner "Yusuf")
      (play-from-hand state :runner "Chrome Parlor")
      (let [yus (get-program state 0)
            cp (get-resource state 0)]
        (is (zero? (get-counters (refresh yus) :virus)) "Yusuf starts with 0 virus counters")
        (is (zero? (get-counters (refresh cp) :virus)) "Chrome Parlor starts with 0 virus counters")
        (play-from-hand state :runner "Contaminate")
        (click-card state :runner (refresh yus))
        (is (= 3 (get-counters (refresh yus) :virus)) "Yusuf has 3 counters after Contaminate")
        (play-from-hand state :runner "Contaminate")
        (click-card state :runner (refresh cp))
        (is (= 3 (get-counters (refresh cp) :virus)) "Chrome Parlor has 3 counters after Contaminate")
        (play-from-hand state :runner "Contaminate")
        (click-card state :runner (refresh yus))
        (click-prompt state :runner "Done")
        (is (= 3 (get-counters (refresh cp) :virus)) "Yusuf isn't selectable by Contaminate"))))
  (testing "Hivemind makes virus programs act like they have a virus counter"
    (do-game
      (new-game {:runner {:deck ["Aumakua" "Friday Chip" "Hivemind" "Contaminate"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5 :click 2)
      (play-from-hand state :runner "Aumakua")
      (play-from-hand state :runner "Hivemind")
      (play-from-hand state :runner "Friday Chip")
      (let [aum (get-program state 0)
            fc (get-hardware state 0)]
        (is (zero? (get-counters (refresh aum) :virus)) "Aumakua starts with 0 virus counters (not counting Hivemind)")
        (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip starts with 0 virus counters")
        (play-from-hand state :runner "Contaminate")
        (click-card state :runner (refresh aum))
        (click-card state :runner (refresh fc))
        (is (= 3 (get-counters (refresh fc) :virus)) "Friday Chip has 3 counters after Contaminate")
        (is (zero? (get-counters (refresh aum) :virus)) "Aumakua ends with 0 virus counters (not counting Hivemind)")))))
