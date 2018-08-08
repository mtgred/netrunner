(ns game-test.cards.agendas.project-wotan
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest project-wotan
  ;; Project Wotan - Only checks if agenda counter is spent
  (do-game
    (new-game {:corp {:deck ["Project Wotan"
                             "Eli 1.0"
                             (qty "Hedge Fund" 3)]}})
    (starting-hand state :corp ["Project Wotan" "Eli 1.0"])
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (let [eli (get-ice state :hq 0)]
      (core/rez state :corp eli))
    (play-and-score state "Project Wotan")
    (take-credits state :corp)
    (let [wot-scored (get-scored state :corp 0)]
      (is (= 3 (get-counters (refresh wot-scored) :agenda)) "Wotan should start with 3 agenda counters")
      (run-on state "HQ")
      (card-ability state :corp wot-scored 0)
      (is (= 2 (get-counters (refresh wot-scored) :agenda))) "Wotan should only have 2 agenda counters")))
