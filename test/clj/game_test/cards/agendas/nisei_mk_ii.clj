(ns game-test.cards.agendas.nisei-mk-ii
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nisei-mk-ii
  ;; Nisei MK II - Remove hosted counter to ETR, check this works in 4.3
  (do-game
    (new-game {:corp {:deck ["Nisei MK II"]}})
    (play-and-score state "Nisei MK II")
    (let [scored-nisei (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has one counter")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-phase-43 state)
      (card-ability state :corp (refresh scored-nisei) 0)
      (click-prompt state :corp "Done") ; close 4.3 corp
      (is (not (:run @state)) "Run ended by using Nisei counter")
      (is (zero? (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has no counters"))))
