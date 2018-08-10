(ns game-test.cards.resources.virus-breeding-ground
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest virus-breeding-ground
  ;; Virus Breeding Ground - Gain counters
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (let [vbg (get-resource state 0)]
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn"))))
  (testing "Can move to programs pumped by Hivemind"
    (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground" "Hivemind" "Aumakua"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Hivemind")
      (play-from-hand state :runner "Aumakua")
      (let [aum (get-program state 1)
            vbg (get-resource state 0)]
        (is (zero? (get-counters aum :virus)) "Aumakua starts with 0 counters (excluding Hivemind)")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner vbg 0)
        (click-card state :runner aum)
        (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))
  (testing "Move counters"
    (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Hivemind")
      (let [hive (get-program state 0)
            vbg (get-resource state 0)]
        (is (= 1 (get-counters hive :virus)) "Hivemind starts with 1 counter")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner vbg 0)
        (click-card state :runner hive)
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))
  (testing "Move counters to a non-virus resource"
    (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground" "Crypt"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Crypt")
      (let [vbg (get-resource state 0)
            crypt (get-resource state 1)]
        (is (zero? (get-counters crypt :virus)) "Crypt starts with 0 counters")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner (refresh vbg) 0)
        (click-card state :runner (refresh crypt))
        (click-prompt state :runner "Done")
        (is (zero? (get-counters (refresh crypt) :virus)) "Crypt doesn't gain a counter")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground doesn't lose a counter")
        (run-on state "Archives")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (is (= 1 (get-counters (refresh crypt) :virus)) "Crypt gained a counter")
        (card-ability state :runner (refresh vbg) 0)
        (click-card state :runner (refresh crypt))
        (is (= 2 (get-counters (refresh crypt) :virus)) "Crypt gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))
