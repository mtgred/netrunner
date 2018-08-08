(ns game-test.cards.events.test-run
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest test-run
  ;; Test Run
  (testing "Make sure program remains installed if Scavenged"
    (do-game
      (new-game {:runner {:deck ["Test Run" "Morning Star"
                                 "Scavenge" "Inti"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Morning Star" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Test Run")
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (click-prompt state :runner "Heap")
        (click-prompt state :runner ms)
        (is (= 2 (:credit (get-runner))) "Program installed for free")
        (let [ms (get-program state 0)]
          (play-from-hand state :runner "Scavenge")
          (click-card state :runner ms)
          (click-card state :runner (find-card "Morning Star" (:discard (get-runner))))
          (take-credits state :runner)
          (is (empty? (:deck (get-runner))) "Morning Star not returned to Stack")
          (is (= "Morning Star" (:title (get-program state 0))) "Morning Star still installed"))))))
