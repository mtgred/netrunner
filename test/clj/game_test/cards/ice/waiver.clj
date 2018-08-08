(ns game-test.cards.ice.waiver
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest waiver
  ;; Waiver - Trash Runner cards in grip with play/install cost <= trace exceed
  (do-game
    (new-game {:corp {:deck ["Waiver"]}
               :runner {:deck ["Corroder" "Dean Lister" "Ubax" "Caldera"]}})
    (play-from-hand state :corp "Waiver" "HQ")
    (let [waiv (get-ice state :hq 0)]
      (core/rez state :corp waiv)
      (card-subroutine state :corp waiv 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "3")
      (is (empty? (filter #(= "Ubax" (:title %)) (:discard (get-runner)))) "Ubax not trashed")
      (is (empty? (filter #(= "Caldera" (:title %)) (:discard (get-runner)))) "Caldera not trashed")
      (is (= 2 (count (:discard (get-runner)))) "2 cards trashed"))))
