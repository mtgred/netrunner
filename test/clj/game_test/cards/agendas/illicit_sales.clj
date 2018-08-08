(ns game-test.cards.agendas.illicit-sales
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest illicit-sales
  ;; Illicit Sales
  (letfn [(illicit-sales-test [[starting-bp answer credits-gained]]
            (testing (str "starting with " starting-bp " and answering " answer " and gaining " credits-gained)
              (do-game
                (new-game {:corp {:deck ["Illicit Sales"]}})
                (let [credits (:credit (get-corp))]
                  (core/gain state :corp :bad-publicity starting-bp)
                  (play-and-score state "Illicit Sales")
                  (click-prompt state :corp answer)
                  (is (= (:credit (get-corp)) (+ credits credits-gained)))))))]
    (doall (map illicit-sales-test
                [[0 "No" 0]
                 [0 "Yes" 3]
                 [1 "No" 3]
                 [1 "Yes" 6]
                 [2 "No" 6]
                 [2 "Yes" 9]
                 [3 "No" 9]
                 [3 "Yes" 12]]))))
