(ns game-test.cards.programs.scheherazade
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a program
  (do-game
    (new-game {:runner {:deck ["Scheherazade" "Cache"
                               "Inti" "Fall Guy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scheherazade")
    (let [sch (get-program state 0)]
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Inti" (:hand (get-runner))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-runner))) "Spent 1 click to install and host")
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (is (= 3 (core/available-mu state)) "Programs hosted on Scheh consume MU")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Cache" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Fall Guy" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-program")
      (is (= 1 (count (:hand (get-runner))))))))
