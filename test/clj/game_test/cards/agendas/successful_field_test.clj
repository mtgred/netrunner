(ns game-test.cards.agendas.successful-field-test
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest successful-field-test
  ;; Successful Field Test
  (do-game
    (new-game {:corp {:deck ["Successful Field Test" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp (vec (cons "Successful Field Test" (repeat 10 "Ice Wall"))))
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (play-and-score state "Successful Field Test")
    (dotimes [n 10]
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-prompt state :corp "HQ"))
    (is (= 5 (:credit (get-corp))) "Should still have 5 credits")
    (is (some? (get-ice state :hq 9)))))
