(ns game-test.cards.agendas.restructured-datapool
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest restructured-datapool
  ;; Restructured Datapool
  (do-game
    (new-game {:corp {:deck ["Restructured Datapool"]}})
    (is (zero? (:tag (get-runner))) "Runner should start with no tags")
    (play-and-score state "Restructured Datapool")
    (let [rd-scored (get-scored state :corp 0)]
      (card-ability state :corp rd-scored 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (:tag (get-runner))) "Runner should gain a tag from Restructured Datapool ability"))))
