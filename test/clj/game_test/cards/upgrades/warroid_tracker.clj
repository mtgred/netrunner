(ns game-test.cards.upgrades.warroid-tracker
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest warroid-tracker
  ;; Warroid Tracker
  (testing "Trashing Warroid starts trace"
    (do-game
      (new-game {:corp {:deck ["Warroid Tracker"]}
                 :runner {:deck ["Corroder" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Dyson Mem Chip")
      (let [war (get-content state :remote1 0)
            cor (get-program state 0)
            mem (get-hardware state 0)]
        (core/rez state :corp war)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
        (click-card state :runner cor)
        (click-card state :runner mem)
        (is (= 2 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards"))))
  (testing "Trashing from central triggers Warroid in root"
    ;; Regression test for #3725
    (do-game
      (new-game {:corp {:deck ["Warroid Tracker" (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Clan Vengeance" "Corroder" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Warroid Tracker" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Dyson Mem Chip")
      (play-from-hand state :runner "Clan Vengeance")
      (let [war (get-content state :hq 0)
            clv (get-resource state 0)
            cor (get-program state 0)
            mem (get-hardware state 0)]
        (core/rez state :corp war)
        (core/add-counter state :runner clv :power 2)
        (card-ability state :runner (refresh clv) 0)
        ;; Prompt choice checks there is a trace prompt from Warroid
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (-> (get-runner) :discard count)) "Runner should start with 1 card in heap (Clan Vengeance)")
        (click-card state :runner cor)
        (click-card state :runner mem)
        (is (= 3 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards (and CV already in heap)")
        (is (= 2 (count (:discard (get-corp)))) "Two cards trashed from HQ by Clan Vengeance")))))
