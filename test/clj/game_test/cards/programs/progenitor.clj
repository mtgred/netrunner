(ns game-test.cards.programs.progenitor
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest progenitor
  ;; Progenitor
  (testing "Hosting Hivemind, using Virus Breeding Ground. Issue #738"
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Virus Breeding Ground" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (play-from-hand state :runner "Virus Breeding Ground")
      (is (= 4 (core/available-mu state)))
      (let [prog (get-program state 0)
            vbg (get-resource state 0)]
        (card-ability state :runner prog 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "No memory used to host on Progenitor")
        (let [hive (first (:hosted (refresh prog)))]
          (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
          (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
          (is (zero? (:credit (get-runner))) "Full cost to host on Progenitor")
          (take-credits state :runner 1)
          (take-credits state :corp)
          (card-ability state :runner vbg 0) ; use VBG to transfer 1 token to Hivemind
          (click-card state :runner hive)
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
          (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))
  (testing "Keep MU the same when hosting or trashing hosted programs"
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (let [pro (get-program state 0)]
        (card-ability state :runner pro 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not deducted from available MU")
        ;; Trash Hivemind
        (core/move state :runner (find-card "Hivemind" (:hosted (refresh pro))) :discard)
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not added to available MU")))))
