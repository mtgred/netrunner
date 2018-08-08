(ns game-test.cards.programs.consume
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest consume
  ;; Consume - gain virus counter for trashing corp card. click to get 2c per counter.
  (testing "Trash and cash out"
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (let [c (get-program state 0)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters"))))
  (testing "Hivemind interaction"
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume" "Hivemind"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 3)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind retains counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 4 (:credit (get-runner))) "Runner gains 4 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters"))))
  (testing "Hivemind counters only"
    (do-game
      (new-game {:runner {:deck ["Consume" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters")))))
