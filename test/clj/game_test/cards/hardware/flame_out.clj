(ns game-test.cards.hardware.flame-out
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest flame-out
  ;; Flame-out - start with 9 credits, use for hosted program, trash hosted program at end of turn when credits used
  (testing "Basic behavior"
    (do-game
      (new-game {:runner {:deck ["Flame-out" "Mimic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (click-card state :runner (find-card "Mimic" (:hand (get-runner))))
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (card-ability state :runner fo 0)
        (is (= 3 (:credit (get-runner))) "Runner gains 1 credit")
        (is (= 8 (get-counters (refresh fo) :credit)) "Took 1 credit from Flame-out")
        (take-credits state :runner)
        (is (empty? (:hosted (refresh fo))) "Mimic trashed")
        (is (= 1 (count (:discard (get-runner)))) "Mimic in trash"))))
  (testing "Corp turn usage"
    (do-game
      (new-game {:runner {:deck ["Flame-out" "Mimic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (click-card state :runner (find-card "Mimic" (:hand (get-runner))))
        (take-credits state :runner)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic hosted")
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (card-ability state :runner fo 1)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 11 (:credit (get-runner))) "Runner gains 9 credit")
        (is (zero? (get-counters (refresh fo) :credit)) "Took all credits from Flame-out")
        (take-credits state :corp)
        (is (empty? (:hosted (refresh fo))) "Mimic trashed")))))
