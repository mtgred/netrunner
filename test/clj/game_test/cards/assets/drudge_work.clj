(ns game-test.cards.assets.drudge-work
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest drudge-work
  ;; Drudge Work - Shuffle agenda from HQ or Archives into R&D, and gain credits = to agenda points
  ;; TODO: do some Noah magic on this test to test several agendas from several locations
  (do-game
    (new-game {:corp {:deck ["Drudge Work"
                             "Hostile Takeover" "Standoff" "Global Food Initiative" "Armed Intimidation"
                             "Hedge Fund"]}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Drudge Work" "New remote")
    (play-from-hand state :corp "Armed Intimidation" "New remote")
    (trash-from-hand state :corp "Hostile Takeover")
    (let [hand (:hand (get-corp))
          drudge-work (get-content state :remote1 0)
          ai (get-content state :remote2 0)
          ht (find-card "Hostile Takeover" (:discard (get-corp)))
          standoff (find-card "Standoff" hand)
          gfi (find-card "Global Food Initiative" hand)
          hf (find-card "Hedge Fund" hand)]
      (core/rez state :corp drudge-work)
      (testing "Rez cost and placing counters on Drudge Work on rez"
        (is (= (- 5 2) (:credit (get-corp))) "Cost 2 credits to rez Drudge Work")
        (is (= 3 (get-counters (refresh drudge-work) :power)) "Drudge Work gained 3 power counters on rez"))
      (testing "Selecting installed agenda, or Operation in hand"
        (card-ability state :corp (refresh drudge-work) 0)
        (click-card state :corp ai)
        (is (some? (get-content state :remote2 0)) "Armed Intimidation still installed in a remote server")
        (is (= 3 (get-counters (refresh drudge-work) :power)) "Did not use a counter when selecting incorrect target")
        (is (= (- 5 2) (:credit (get-corp))) "Did not gain credits when selecting installed Agenda")
        (is (empty? (:deck (get-corp))) "No cards in R&D")
        (click-card state :corp hf)
        (is (= 3 (count (:hand (get-corp)))) "Hedge Fund still in HQ")
        (is (= 3 (get-counters (refresh drudge-work) :power)) "Did not use a counter when selecting incorrect target")
        (is (= (- 5 2) (:credit (get-corp))) "Did not gain credits when selecting operation")
        (is (empty? (:deck (get-corp))) "No cards in R&D"))
      (testing "Gaining credits and shuffling in agenda from HQ"
        (click-card state :corp gfi)
        (is (= 1 (count (:deck (get-corp)))) "One card (the GFI) shuffled into R&D")
        (is (= 2 (count (:hand (get-corp)))) "Two cards left in HQ (Standoff and Hedge Fund)")
        (is (= 2 (:click (get-corp))) "Used a click to activate Drudge Work")
        (is (= 2 (get-counters (refresh drudge-work) :power)) "Used a counter when selecting GFI from HQ")
        (is (= (+ 5 -2 3) (:credit (get-corp))) "Gained 3 credits when revealing GFI"))
      (testing "Gaining credits and shuffling in agenda from Archives"
        (card-ability state :corp (refresh drudge-work) 0)
        (click-card state :corp ht)
        (is (= 2 (count (:deck (get-corp)))) "One more card (the Hostile Takeover) shuffled into R&D")
        (is (= 2 (count (:hand (get-corp)))) "Two cards left in HQ (Standoff and Hedge Fund)")
        (is (= 1 (:click (get-corp))) "Used a click to activate Drudge Work")
        (is (= 1 (get-counters (refresh drudge-work) :power)) "Used a counter when selecting Hostile Takeover from Archives")
        (is (= (+ 5 -2 3 1) (:credit (get-corp))) "Gained 1 credits when revealing Hostile Takeover"))
      (testing "Gain 0 credits when shuffling in Standoff, trashing Drudge Work after 3 uses"
        (card-ability state :corp (refresh drudge-work) 0)
        (click-card state :corp standoff)
        (is (= 3 (count (:deck (get-corp)))) "One more card (the Standoff) shuffled into R&D")
        (is (= 1 (count (:hand (get-corp)))) "Only the Hedge Fund left in HQ")
        (is (= 0 (:click (get-corp))) "Used a click to activate Drudge Work")
        (is (= "Drudge Work" (-> (get-corp) :discard first :title)) "Drudge Work trashed after no counters left")
        (is (= (+ 5 -2 3 1) (:credit (get-corp))) "Gained 0 credits when revealing Standoff")))))
