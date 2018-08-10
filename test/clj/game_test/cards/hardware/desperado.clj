(ns game-test.cards.hardware.desperado
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game {:runner {:deck [(qty "Desperado" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (core/available-mu state)) "Gain 1 memory")
    (is (= 3 (:credit (get-runner))) "Got 1c for successful run on Desperado")))
