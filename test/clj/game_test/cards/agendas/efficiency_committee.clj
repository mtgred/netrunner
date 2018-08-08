(ns game-test.cards.agendas.efficiency-committee
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest efficiency-committee
  ;; Efficiency Committee
  (do-game
    (new-game {:corp {:deck [(qty "Efficiency Committee" 3) (qty "Shipment from SanSan" 2)
                             "Ice Wall"]}})
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ec1 (get-content state :remote1 0)
          ec2 (get-content state :remote2 0)
          ec3 (get-content state :remote3 0)
          iw (get-ice state :hq 0)]
      (score-agenda state :corp ec1)
      (let [ec1_scored (get-scored state :corp 0)]
        (is (= 3 (get-counters (refresh ec1_scored) :agenda)))
        (is (= 2 (:agenda-point (get-corp))))
        ;; use token
        (is (= 3 (:click (get-corp))))
        (card-ability state :corp ec1_scored 0)
        (is (= 4 (:click (get-corp))))
        ;; try to advance Ice Wall
        (advance state iw)
        (is (= 4 (:click (get-corp))))
        (is (zero? (get-counters (refresh iw) :advancement)))
        ;; try to advance Efficiency Committee
        (advance state ec2)
        (is (= 4 (:click (get-corp))))
        (is (zero? (get-counters (refresh ec2) :advancement)))
        ;; advance with Shipment from SanSan
        (play-from-hand state :corp "Shipment from SanSan")
        (click-prompt state :corp "2")
        (click-card state :corp ec2)
        (is (= 2 (get-counters (refresh ec2) :advancement)))
        (play-from-hand state :corp "Shipment from SanSan")
        (click-prompt state :corp "2")
        (click-card state :corp ec2)
        (is (= 4 (get-counters (refresh ec2) :advancement)))
        (core/score state :corp {:card (refresh ec2)})
        (is (= 4 (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        ;; can advance again
        (advance state iw)
        (is (= 1 (get-counters (refresh iw) :advancement)))
        (advance state ec3)
        (is (= 1 (get-counters (refresh ec3) :advancement)))))))
