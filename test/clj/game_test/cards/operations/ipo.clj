(ns game-test.cards.operations.ipo
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ipo
  ;; IPO - credits with Terminal operations
  (do-game
    (new-game {:corp {:deck ["IPO"]}})
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "IPO")
    (is (= 13 (:credit (get-corp))))
    (is (zero? (:click (get-corp))) "Terminal ends turns")))
