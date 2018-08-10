(ns game-test.cards.resources.pad-tap
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest pad-tap
  ;; PAD Tap
  (do-game
    (new-game {:corp {:deck ["Melange Mining Corp."]}
               :runner {:deck ["PAD Tap"]}})
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "PAD Tap")
    (let [mel (get-content state :remote1 0)
          tap (get-resource state 0)]
      (take-credits state :runner)
      (let [credits (:credit (get-runner))]
        (core/click-credit state :corp nil)
        (is (zero? (-> (get-runner) :prompt count)) "Runner should have no prompts from PAD Tap")
        (is (= credits (:credit (get-runner))) "Runner shouldn't gain PAD Tap credits from clicking for a credit"))
      (let [credits (:credit (get-runner))]
        (core/rez state :corp mel)
        (core/gain state :corp :click 10)
        (card-ability state :corp mel 0)
        (is (= (+ credits 1) (:credit (get-runner))) "Runner should gain 1 credit from PAD Tap triggering from Melange Mining Corp. ability")
        (card-ability state :corp mel 0) ;; Triggering Melange a second time
        (is (zero? (-> (get-runner) :prompt count)) "Runner should have no prompts from PAD Tap"))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (zero? (-> (get-runner) :discard count)) "Runner should have 0 cards in Heap")
      (let [credits (:credit (get-corp))
            clicks (:click (get-corp))]
        (card-side-ability state :corp tap 0)
        (is (= (- credits 3) (:credit (get-corp))) "PAD Tap ability should cost Corp 3 credits")
        (is (= (- clicks 1) (:click (get-corp))) "PAD Tap ability should cost Corp 1 click")))))
