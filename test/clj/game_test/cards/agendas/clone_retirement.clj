(ns game-test.cards.agendas.clone-retirement
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest clone-retirement
  ;; Clone Retirement
  (do-game
    (new-game {:corp {:deck [(qty "Clone Retirement" 2) "Hostile Takeover"]}})
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))))
    (is (= 1 (:bad-publicity (get-corp))))
    (play-and-score state "Clone Retirement")
    (is (zero? (:bad-publicity (get-corp))))
    (play-from-hand state :corp "Clone Retirement" "New remote")
    (take-credits state :corp)
    (run-on state "Server 3")
    (run-successful state)
    (click-prompt state :runner "Steal")
    (is (= 1 (:bad-publicity (get-corp))))))
