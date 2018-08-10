(ns game-test.cards.agendas.medical-breakthrough
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest medical-breakthrough
  ;; Medical Breakthrough
  (do-game
    (new-game {:corp {:deck [(qty "Medical Breakthrough" 3) (qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (let [mb2 (get-content state :remote2 0)]
      (advance state mb2 3)
      (core/score state :corp {:card (refresh mb2)})
      (is (= 2 (:agenda-point (get-corp))) "Only needed 3 advancements to score"))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (let [mb3 (get-content state :remote3 0)]
      (advance state mb3 2)
      (core/score state :corp {:card (refresh mb3)})
      (is (= 4 (:agenda-point (get-corp))) "Only needed 2 advancements to score"))))
