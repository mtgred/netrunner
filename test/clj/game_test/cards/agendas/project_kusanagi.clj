(ns game-test.cards.agendas.project-kusanagi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest project-kusanagi
  ;; Project Kusanagi
  (do-game
    (new-game {:corp {:deck [(qty "Project Kusanagi" 2) "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/gain state :corp :click 10 :credit 10)
    (testing "Should gain 0 counters"
      (play-and-score state "Project Kusanagi")
      (let [pk-scored (get-scored state :corp 0)]
        (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should start with 0 agenda counters")))
    (testing "Should gain 1 counter"
      (play-from-hand state :corp "Project Kusanagi" "New remote")
      (let [pk (get-content state :remote2 0)]
        (advance state pk 3)
        (is (= 3 (get-counters (refresh pk) :advancement)) "Kusanagi should have 3 advancement tokens")
        (core/score state :corp {:card (refresh pk)}))
      (let [pk-scored (get-scored state :corp 1)]
        (is (= 1 (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 1 agenda counter")
        (run-empty-server state :hq)
        (card-ability state :corp pk-scored 0)
        (is (last-log-contains? state "Do 1 net damage"))
        (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 0 agenda counters")))))
