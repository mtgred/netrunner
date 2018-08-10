(ns game-test.cards.agendas.false-lead
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest false-lead
  ;; False Lead
  (do-game
    (new-game {:corp {:deck ["False Lead"]}})
    (play-and-score state "False Lead")
    (is (= 1 (count (:scored (get-corp)))) "Corp should have 1 agenda point")
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner should start turn with 4 clicks")
    (card-ability state :corp (get-scored state :corp 0) 0)
    (is (= 2 (:click (get-runner))) "Runner should lose 2 clicks from False Lead")))
