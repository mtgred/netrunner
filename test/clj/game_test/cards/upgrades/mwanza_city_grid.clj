(ns game-test.cards.upgrades.mwanza-city-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mwanza-city-grid
  ;; Mwanza City Grid - runner accesses 3 additional cards, gain 2C for each card accessed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Mwanza City Grid")
      (is (= #{"R&D" "HQ"} (-> (get-corp) :prompt first :choices set)) "Mwanza can only be installed in root of HQ or R&D")
      (click-prompt state :corp "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (core/rez state :corp mcg)
        (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
        (run-successful state)
        (click-prompt state :runner "Mwanza City Grid")
        (click-prompt state :runner "No action")
        (dotimes [c 4]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))
  (testing "effect persists through current run after trash"
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (core/rez state :corp mcg)
        (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
        (run-successful state)
        (click-prompt state :runner "Mwanza City Grid")
        (click-prompt state :runner "Pay 5 [Credits] to trash")
        (dotimes [c 4]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))
  (testing "works well with replacement effects"
    ;; Regression test for #3456
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" "Hedge Fund"]}
                 :runner {:deck ["Embezzle"]}})
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (take-credits state :corp)
      (core/rez state :corp (get-content state :hq 0))
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (play-run-event state (first (:hand (get-runner))) :hq)
      (click-prompt state :runner "ICE")
      (is (zero? (count (:discard (get-corp)))) "No cards trashed from HQ")
      (is (not (:run @state)) "Run ended after Embezzle completed - no accesses from Mwanza")
      (is (= 7 (:credit (get-corp))) "Corp did not gain any money from Mwanza")))
  (testing "interaction with Kitsune"
    ;; Regression test for #3469
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" "Breached Dome"
                               (qty "Kitsune" 2) (qty "Hedge Fund" 3)]}})
      (core/draw state :corp 1) ; Draw last card of deck
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (play-from-hand state :corp "Kitsune" "HQ")
      (play-from-hand state :corp "Kitsune" "R&D")
      (take-credits state :corp)
      (let [mwanza (get-content state :hq 0)
            k-hq (get-ice state :hq 0)
            k-rd (get-ice state :rd 0)]
        (core/rez state :corp mwanza)
        (core/rez state :corp k-hq)
        (core/rez state :corp k-rd)
        (run-on state "HQ")
        (card-subroutine state :corp k-hq 0)
        (click-card state :corp (find-card "Breached Dome" (:hand (get-corp))))
        (is (= 2 (-> (get-runner) :hand count)) "Runner took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :runner "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (run-jack-out state)
        (run-on state "R&D")
        (card-subroutine state :corp k-rd 0)
        (click-card state :corp (find-card "Breached Dome" (:hand (get-corp))))
        (is (= 1 (-> (get-runner) :hand count)) "Runner took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :runner "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (run-jack-out state)
        (is (= 2 (-> (get-corp) :discard count)) "Two Kitsunes trashed after resolving their subroutines")))))
