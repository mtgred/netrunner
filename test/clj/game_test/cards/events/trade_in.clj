(ns game-test.cards.events.trade-in
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest trade-in
  ;; Trade-in - trash an installed Hardware, gain credits equal to half of install cost,
  ;;            search stack for Hardware and add to grip
  (do-game
    (new-game {:runner {:deck [(qty "Trade-In" 3) (qty "Astrolabe" 2) (qty "Sports Hopper" 2)]}
               :options {:start-as :runner}})
    (starting-hand state :runner ["Trade-In" "Trade-In" "Astrolabe" "Sports Hopper"])
    (core/gain state :runner :click 5 :credit 5)
    (play-from-hand state :runner "Astrolabe")
    (play-from-hand state :runner "Sports Hopper")
    (testing "Trade-in works with Hardware costing 0 or 1 credits (issue #3750)"
      (let [runner-credits (:credit (get-runner))]
        (play-from-hand state :runner "Trade-In")
        (click-card state :runner (get-hardware state 0))
        (is (= 2 (count (:discard (get-runner)))) "Trade-In and Astrolabe in discard")
        (is (= (- runner-credits 1) (:credit (get-runner)))
            "Paid 1 credit to play Trade-In and gained 0 credits from trashing Astrolabe")))
    (testing "Trade-In lets runner search for Hardware and add it to Grip"
      (is (= 1 (count (:hand (get-runner)))) "Only 1 Trade-In in Grip")
      ;; Add sports hopper to hand
      (click-prompt state :runner (-> (get-runner) :prompt first :choices first))
      (is (= 2 (count (:hand (get-runner)))) "Sports Hopper added to Grip"))
    (testing "Gain credits when install cost is greater than 1"
      (let [runner-credits (:credit (get-runner))]
        (play-from-hand state :runner "Trade-In")
        (click-card state :runner (get-hardware state 0))
        (is (= (+ runner-credits -1 1) (:credit (get-runner)))
            "Paid 1 credit to play Trade-In and gained 1 credits from trashing Sports Hopper")
        (is (= 4 (count (:discard (get-runner)))) "2 Trade-In, 1 Astrolabe and 1 Sports Hopper in discard")))))
