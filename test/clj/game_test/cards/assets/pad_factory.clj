(ns game-test.cards.assets.pad-factory
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest pad-factory
  ;; PAD Factory - Click to place an advancement, cannot score target until next turn
  (do-game
    (new-game {:corp {:deck ["PAD Factory" "15 Minutes"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "PAD Factory" "New remote")
    (play-from-hand state :corp "15 Minutes" "New remote")
    (let [pf (get-content state :remote1 0)
          fif (get-content state :remote2 0)]
      (core/rez state :corp pf)
      (card-ability state :corp (refresh pf) 0)
      (click-card state :corp fif)
      (card-ability state :corp (refresh pf) 0)
      (click-card state :corp (refresh fif))
      (is (zero? (:click (get-corp))) "Spent 2 clicks using PAD Factory twice")
      (is (= 2 (get-counters (refresh fif) :advancement)) "Agenda has 2 advancements")
      (core/score state :corp {:card (refresh fif)})
      (is (empty? (:scored (get-corp))) "Prevented from scoring this turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/score state :corp {:card (refresh fif)})
      (is (= 1 (count (:scored (get-corp)))) "Scored agenda"))))
