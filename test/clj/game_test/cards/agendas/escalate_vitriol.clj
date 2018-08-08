(ns game-test.cards.agendas.escalate-vitriol
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest escalate-vitriol
  ;; Escalate Vitriol
  (do-game
    (new-game {:corp {:deck ["Escalate Vitriol"]}})
    (core/lose state :corp :credit 5)
    (play-and-score state "Escalate Vitriol")
    (let [ev-scored (get-scored state :corp 0)]
      (dotimes [tag 10]
        (is (zero? (:tag (get-runner))) "Should start with 0 tags")
        (is (zero? (:credit (get-corp))) "Should start with 0 credits")
        (core/gain state :runner :tag tag)
        (card-ability state :corp ev-scored 0)
        (is (= tag (:credit (get-corp))) (str "Should gain " tag " credits"))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/lose state :corp :credit (:credit (get-corp)))
        (core/lose state :runner :tag tag)))))
