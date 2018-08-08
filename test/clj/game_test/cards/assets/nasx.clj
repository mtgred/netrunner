(ns game-test.cards.assets.nasx
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nasx
  ;; NASX
  (do-game
    (new-game {:corp {:deck ["NASX"]}})
    (play-from-hand state :corp "NASX" "New remote")
    (let [nasx (get-content state :remote1 0)]
      (core/rez state :corp nasx)
      (take-credits state :corp)
      (let [credits (:credit (get-corp))]
        (take-credits state :runner)
        (is (= (+ credits 1) (:credit (get-corp))) "Corp should gain 1 credit at start of turn from NASX"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp nasx 1)
        (is (= (- credits 1) (:credit (get-corp))) "Corp should spend 1 credit on NASX ability")
        (is (= 1 (get-counters (refresh nasx) :power)) "NASX should gain 1 power counter from spent credits"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp nasx 2)
        (is (= (- credits 2) (:credit (get-corp))) "Corp should spend 2 credit on NASX ability")
        (is (= 3 (get-counters (refresh nasx) :power)) "NASX should gain 2 power counter from spent credits"))
      (let [credits (:credit (get-corp))
            counters (get-counters (refresh nasx) :power)]
        (card-ability state :corp nasx 3)
        (is (= (+ credits (* 2 counters)) (:credit (get-corp))) (str "Corp should gain " (* 2 counters) " from NASX trash ability"))
        (is (= 1 (-> (get-corp) :discard count)) "Corp should trash NASX for ability")
        (is (= "NASX" (-> (get-corp) :discard first :title)) "NASX should be in archives")))))
