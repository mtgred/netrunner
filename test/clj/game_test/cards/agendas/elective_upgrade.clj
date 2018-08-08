(ns game-test.cards.agendas.elective-upgrade
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest elective-upgrade
  ;; Elective Upgrade
  (do-game
    (new-game {:corp {:deck ["Elective Upgrade"]}})
    (play-and-score state "Elective Upgrade")
    (let [eu-scored (get-scored state :corp 0)]
      (is (= 2 (get-counters (refresh eu-scored) :agenda)) "Should start with 2 agenda counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 3 (:click (get-corp))) "Should start with 4 clicks")
      (card-ability state :corp eu-scored 0)
      (card-ability state :corp eu-scored 0)
      (is (= 4 (:click (get-corp))) "Should gain 2 clicks, not 3")
      (is (= 1 (get-counters (refresh eu-scored) :agenda)) "Should still have 1 agenda counter"))))
