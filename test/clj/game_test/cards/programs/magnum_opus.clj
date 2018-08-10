(ns game-test.cards.programs.magnum-opus
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest magnum-opus
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game {:runner {:deck ["Magnum Opus"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 2 (core/available-mu state)))
    (is (zero? (:credit (get-runner))))
    (let [mopus (get-program state 0)]
      (card-ability state :runner mopus 0)
      (is (= 2 (:credit (get-runner))) "Gain 2cr"))))
