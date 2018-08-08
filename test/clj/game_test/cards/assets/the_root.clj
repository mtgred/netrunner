(ns game-test.cards.assets.the-root
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-root
  ;; The Root - recurring credits refill at Step 1.2
  (do-game
    (new-game {:corp {:id "Blue Sun: Powering the Future"
                      :deck ["The Root"]}})
    (play-from-hand state :corp "The Root" "New remote")
    (core/gain state :corp :credit 6)
    (let [root (get-content state :remote1 0)]
      (core/rez state :corp root)
      (card-ability state :corp (refresh root) 0)
      (is (= 2 (get-counters (refresh root) :recurring)) "Took 1 credit from The Root")
      (is (= 6 (:credit (get-corp))) "Corp took Root credit into credit pool")
      (take-credits state :corp)
      (take-credits state :runner)
      ;; we expect Step 1.2 to have triggered because of Blue Sun
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (is (= 3 (get-counters (refresh root) :recurring)) "Recurring credits were refilled before Step 1.2 window"))))
