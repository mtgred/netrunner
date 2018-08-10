(ns game-test.cards.assets.public-support
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest public-support
  ;; Public support scoring and trashing
  ;; TODO could also test for NOT triggering "when scored" events
  (do-game
    (new-game {:corp {:deck [(qty "Public Support" 2)]}})
    ;; Corp turn 1, install and rez public supports
    (play-from-hand state :corp "Public Support" "New remote")
    (play-from-hand state :corp "Public Support" "New remote")
    (let [publics1 (get-content state :remote1 0)
          publics2 (get-content state :remote2 0)]
      (core/rez state :corp (refresh publics1))
      (core/rez state :corp (refresh publics2))
      (take-credits state :corp)
      ;; Runner turn 1, creds
      (is (= 2 (:credit (get-corp))))
      (is (= 3 (get-counters (refresh publics1) :power)))
      (take-credits state :runner)
      ;; Corp turn 2, creds, check if supports are ticking
      (is (= 2 (get-counters (refresh publics1) :power)))
      (is (zero? (:agenda-point (get-corp))))
      (is (nil? (:agendapoints (refresh publics1))))
      (take-credits state :corp)
      ;; Runner turn 2, run and trash publics2
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash
      (is (= 5 (:credit (get-runner))))
      (take-credits state :runner)
      ;; Corp turn 3, check how publics1 is doing
      (is (= 1 (get-counters (refresh publics1) :power)))
      (is (zero? (:agenda-point (get-corp))))
      (take-credits state :corp)
      ;; Runner turn 3, boring
      (take-credits state :runner)
      ;; Corp turn 4, check the delicious agenda points
      (let [scored-pub (get-scored state :corp 0)]
        (is (= 1 (:agenda-point (get-corp))) "Gained 1 agenda point")
        (is (= "Public Support" (:title scored-pub)))
        (is (= 1 (:agendapoints scored-pub)))))))
