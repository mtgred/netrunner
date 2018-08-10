(ns game-test.cards.operations.biased-reporting
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest biased-reporting
  ;; Biased Reporting
  (do-game
    (new-game {:corp {:deck ["Biased Reporting"]}
               :runner {:deck [(qty "Fan Site" 5)]}})
    (take-credits state :corp)
    (starting-hand state :runner (repeat 5 "Fan Site"))
    (core/gain state :runner :click 10)
    (dotimes [_ 5]
      (play-from-hand state :runner "Fan Site"))
    (take-credits state :runner)
    (play-from-hand state :corp "Biased Reporting")
    (let [cc (:credit (get-corp))
          rc (:credit (get-runner))]
      (click-prompt state :corp "Resource")
      (click-card state :runner (get-resource state 0))
      (click-prompt state :runner "Done")
      (is (= (inc rc) (:credit (get-runner))) "Runner should gain 1 credit for trashing a Fan Site")
      (is (= (+ (* 4 2) cc) (:credit (get-corp))) "Corp should gain 8 credits for remaining 4 Fan Sites"))))
