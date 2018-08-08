(ns game-test.cards.agendas.corporate-sales-team
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest corporate-sales-team
  ;; Corporate Sales Team
  (do-game
    (new-game {:corp {:deck [(qty "Corporate Sales Team" 2)]}})
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "Corporate Sales Team")
    (let [scored-cst (get-scored state :corp 0)]
      (core/end-turn state :corp nil)
      (core/start-turn state :runner nil)
      (is (= 6 (:credit (get-corp))) "Increments at runner's start of turn")
      (is (= 9 (get-counters (refresh scored-cst) :credit)))
      (core/end-turn state :runner nil)
      (core/start-turn state :corp nil)
      (is (= 7 (:credit (get-corp))) "Increments at corp's start of turn")
      (is (= 8 (get-counters (refresh scored-cst) :credit))))))
