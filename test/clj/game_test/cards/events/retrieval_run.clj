(ns game-test.cards.events.retrieval-run
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest retrieval-run
  ;; Retrieval Run - Run Archives successfully and install a program from Heap for free
  (do-game
    (new-game {:runner {:deck ["Retrieval Run" "Morning Star"]}})
    (take-credits state :corp)
    (trash-from-hand state :runner "Morning Star")
    (play-from-hand state :runner "Retrieval Run")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (click-prompt state :runner "Replacement effect")
    (let [ms (first (:discard (get-runner)))]
      (click-prompt state :runner ms)
      (is (= "Morning Star" (:title (first (get-program state))))
          "Morning Star installed")
      (is (= 2 (:credit (get-runner))) "Morning Star installed at no cost")
      (is (= 2 (core/available-mu state)) "Morning Star uses 2 memory"))))
