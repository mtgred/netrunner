(ns game-test.cards.agendas.high-risk-investment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest high-risk-investment
  ;; High-Risk Investment
  (do-game
    (new-game {:corp {:deck ["High-Risk Investment"]}})
    (play-and-score state "High-Risk Investment")
    (let [hri-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh hri-scored) :agenda)) "Has 1 agenda counter")
      (take-credits state :corp)
      (is (= 7 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 9 (:credit (get-runner))))
      (card-ability state :corp hri-scored 0)
      (is (= 16 (:credit (get-corp))) "Gained 9 credits")
      (is (= 2 (:click (get-corp))) "Spent 1 click")
      (is (zero? (get-counters (refresh hri-scored) :agenda)) "Spent agenda counter"))))
