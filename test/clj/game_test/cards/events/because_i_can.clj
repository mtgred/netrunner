(ns game-test.cards.events.because-i-can
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest because-i-can
  ;; make a successful run on a remote to shuffle its contents into R&D
  (do-game
    (new-game {:corp {:deck ["Sand Storm" "PAD Campaign" "Project Atlas" (qty "Shell Corporation" 2)]}
               :runner {:deck [(qty "Because I Can" 2)]}})
    (play-from-hand state :corp "Shell Corporation" "New remote")
    (play-from-hand state :corp "Shell Corporation" "Server 1")
    (play-from-hand state :corp "Project Atlas" "Server 1")
    (take-credits state :corp)
    (let [n (count (get-in @state [:corp :deck]))]
      (play-from-hand state :runner "Because I Can")
      (click-prompt state :runner "Server 1")
      (is (= 3 (count (get-in @state [:corp :servers :remote1 :content])))
          "3 cards in server 1 before successful run")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= (+ n 3) (count (get-in @state [:corp :deck]))) "3 cards were shuffled into R&D")
      (is (zero? (count (get-in @state [:corp :servers :remote1 :content]))) "No cards left in server 1"))
    (take-credits state :runner)
    (play-from-hand state :corp "Sand Storm" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (let [n (count (get-in @state [:corp :deck]))
          sand-storm (get-ice state :remote2 0)]
      (play-from-hand state :runner "Because I Can")
      (click-prompt state :runner "Server 2")
      (core/rez state :corp sand-storm)
      (is (= :remote2 (first (get-in @state [:run :server]))))
      (card-subroutine state :corp sand-storm 0)
      (click-prompt state :corp "Server 3")
      (is (= :remote3 (first (get-in @state [:run :server]))))
      (is (= 1 (count (get-in @state [:corp :servers :remote3 :content]))) "1 cards in server 3 before successful run")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= (+ n 1) (count (get-in @state [:corp :deck]))) "1 card was shuffled into R&D")
      (is (zero? (count (get-in @state [:corp :servers :remote3 :content]))) "No cards left in server 3"))))
