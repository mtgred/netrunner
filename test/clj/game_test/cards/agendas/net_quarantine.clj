(ns game-test.cards.agendas.net-quarantine
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest net-quarantine
  ;; Net Quarantine
  (do-game
    (new-game {:corp {:deck ["Net Quarantine"]}})
    (core/gain state :runner :link 1)
    (core/gain state :corp :click 3)
    (play-and-score state "Net Quarantine")
    (let [credits (:credit (get-corp))]
      (is (= credits (:credit (get-corp))) (str "Corp has " credits " credits"))
      (is (= 1 (:link (get-runner))) "Runner has 1 link")
      (core/init-trace state :corp {:title "/trace command" :side :corp} {:base 1})
      (click-prompt state :corp "0")
      (is (zero? (-> (get-runner) :prompt first :link)) "Runner has 0 link during first trace")
      (click-prompt state :runner "3")
      (is (= (+ credits 1) (:credit (get-corp))) "Corp gained a credit from NQ")
      ; second trace of turn - no link reduction
      (core/init-trace state :corp {:title "/trace command" :side :corp} {:base 1})
      (click-prompt state :corp "0")
      (is (= 1 (-> (get-runner) :prompt first :link)) "Runner has 1 link during later traces")
      (click-prompt state :runner "2")
      (is (= (+ credits 2) (:credit (get-corp))) "Corp gained a credit from NQ"))))
