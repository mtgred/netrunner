(ns game-test.cards.agendas.geothermal-fracking
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest geothermal-fracking
  ;; Geothermal Fracking
  (testing "basic test"
    (do-game
      (new-game {:corp {:deck ["Geothermal Fracking"]}})
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
      (is (zero? (:bad-publicity (get-corp))) "Should start with 0 bad publicity")
      (let [gf-scored (get-scored state :corp 0)]
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :corp gf-scored 0)
        (is (= 1 (:click (get-corp))) "Should have 1 click left")
        (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
        (is (= 1 (:bad-publicity (get-corp))) "Should gain 1 bad publicity"))))
  (testing "prevented bad publicity shouldn't block credit gain"
    (do-game
      (new-game {:corp {:deck ["Geothermal Fracking" "Broadcast Square"]}})
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
      (is (zero? (:bad-publicity (get-corp))) "Should start with 0 bad publicity")
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (let [gf-scored (get-scored state :corp 0)
            bs (get-content state :remote2 0)]
        (core/rez state :corp bs)
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :corp gf-scored 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (zero? (:click (get-corp))) "Should have 0 click left")
        (is (= 10 (:credit (get-corp))) "Should gain 7 credits from 3 to 10")
        (is (zero? (:bad-publicity (get-corp))) "Should gain 0 bad publicity from prevention")))))
