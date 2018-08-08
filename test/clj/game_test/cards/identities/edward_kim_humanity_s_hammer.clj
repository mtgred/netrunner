(ns game-test.cards.identities.edward-kim-humanity-s-hammer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest edward-kim-humanity-s-hammer
  ;; Edward Kim
  (testing "Trash first operation accessed each turn, but not if first one was in Archives"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 2) "PAD Campaign"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Eater" (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "PAD Campaign")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "No operation trashed from HQ; accessed one in Archives first")
      (take-credits state :runner)
      (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :hand)
      (is (= 1 (count (:discard (get-corp)))))
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
      (take-credits state :runner)
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (play-from-hand state :runner "Eater")
      (let [eater (get-program state 0)]
        (run-on state "Archives")
        (card-ability state :runner eater 0) ; pretend to break a sub so no cards in Archives will be accessed
        (run-successful state)
        (is (= 3 (count (:discard (get-corp)))))
        (run-empty-server state "HQ")
        (is (= 4 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))
  (testing "Do not trigger maw on first Operation access (due to trash)"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 2)]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Maw" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Maw")
      (is (zero? (count (:discard (get-corp)))) "No cards in Archives")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Only one card trashed from HQ, by Ed Kim")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "One more card trashed from HQ, by Maw"))))
