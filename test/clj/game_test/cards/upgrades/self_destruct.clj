(ns game-test.cards.upgrades.self-destruct
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest self-destruct
  ;; Self-destruct
  (do-game
    (new-game {:corp {:deck ["Self-destruct" "Dedicated Response Team" "Ice Wall"]}})
    (core/gain state :corp :credit 100 :click 4)
    (play-from-hand state :corp "Self-destruct" "New remote")
    (play-from-hand state :corp "Dedicated Response Team" "Server 1")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (let [self (get-content state :remote1 0)]
      (take-credits state :corp)
      (run-on state "Server 1")
      (card-ability state :corp self 0)
      (is (= 3 (-> (get-corp) :discard count)) "All 3 cards from Server 1 should be in discard")
      (is (= 2 (-> (get-corp) :prompt first :base)) "Self-destruct base trace should start at 2")
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in heap")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 3 (-> (get-runner) :discard count)) "Runner should take 3 net damage from losing Self-destruct trace"))))
