(ns game-test.cards.agendas.priority-requisition
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest priority-requisition
  ;; Priority Requisition
  (do-game
    (new-game {:corp {:deck ["Priority Requisition" "Archer"]}})
    (play-from-hand state :corp "Archer" "HQ")
    (let [arc (get-ice state :hq 0)]
      (play-and-score state "Priority Requisition")
      (click-card state :corp arc)
      (is (:rezzed (refresh arc))))))
