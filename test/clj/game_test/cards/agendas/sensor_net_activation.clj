(ns game-test.cards.agendas.sensor-net-activation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sensor-net-activation
  ;; Sensor Net Activation
  (do-game
    (new-game {:corp {:deck [(qty "Sensor Net Activation" 2) "Enforcer 1.0" "Ash 2X3ZB9CY"]}})
    (play-from-hand state :corp "Enforcer 1.0" "HQ")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :corp 0)
          enf (get-ice state :hq 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (:rezzed (refresh enf))) "Enforcer 1.0 should start derezzed")
      (card-ability state :corp (refresh sna-scored) 0)
      (click-card state :corp enf)
      (is (:rezzed (refresh enf)) "Enforcer 1.0 should be rezzed")
      (is (= 1 (count (:scored (get-corp)))) "Enforcer 1.0 should be rezzed without forfeiting agenda")
      (take-credits state :corp)
      (is (not (:rezzed (refresh enf))) "Enforcer 1.0 should be derezzed"))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ash 2X3ZB9CY" "New remote")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :corp 1)
          ash (get-content state :remote2 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (:rezzed (refresh ash))) "Ash should start derezzed")
      (card-ability state :corp (refresh sna-scored) 0)
      (click-card state :corp ash)
      (is (:rezzed (refresh ash)) "Ash should be rezzed")
      (take-credits state :corp)
      (is (not (:rezzed (refresh ash))) "Ash should be derezzed"))))
