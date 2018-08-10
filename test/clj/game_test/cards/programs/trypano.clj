(ns game-test.cards.programs.trypano
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest trypano
  (testing "Hivemind and Architect interactions"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 2)]}
                 :runner {:deck [(qty "Trypano" 2) "Hivemind"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-rezzed (get-ice state :hq 0)
            architect-unrezzed (get-ice state :rd 0)]
        (core/rez state :corp architect-rezzed)
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (game.core/get-card state architect-rezzed))
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (= 2 (core/available-mu state)) "Trypano consumes 1 MU"))
      ;; wait 4 turns to make both Trypanos have 4 counters on them
      (dotimes [n 4]
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes"))
      (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
      (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect is not trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect is not trashed")
      (play-from-hand state :runner "Hivemind") ; now Hivemind makes both Trypanos have 5 counters
      (is (zero? (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect was trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect was not trashed")
      (is (= 1 (count (:discard (get-runner)))) "Trypano went to discard")))
  (testing "Fire when Hivemind gains counters"
    (do-game
      (new-game {:corp {:deck ["Architect"]}
                 :runner {:deck ["Trypano" "Hivemind" (qty "Surge" 2)]}})
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-unrezzed (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
        (is (= 1 (count (get-ice state :rd))) "Unrezzed Architect is not trashed")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-program state 0)]
          (is (= 1 (get-counters (refresh hive) :virus)) "Hivemind starts with 1 virus counter")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 3 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 5 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters (now at 5)")
          (is (zero? (count (get-ice state :rd))) "Unrezzed Architect was trashed")
          (is (= 3 (count (:discard (get-runner)))) "Trypano went to discard"))))))
