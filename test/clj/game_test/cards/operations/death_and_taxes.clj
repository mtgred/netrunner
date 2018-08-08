(ns game-test.cards.operations.death-and-taxes
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest death-and-taxes
  ;; Death and Taxes gain credit on runner install, runner trash installed card
  ;; Also regression test for #3160
  (do-game
    (new-game {:corp {:deck ["Death and Taxes" "PAD Campaign"]}
               :runner {:deck ["Aumakua" "DaVinci" "Fall Guy"]}})
    (play-from-hand state :corp "Death and Taxes")
    (is (= (- 5 2) (:credit (get-corp))) "Corp paid 2 to play Death and Taxes")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (let [corp-creds (:credit (get-corp))]
      (trash-from-hand state :runner "DaVinci")
      (is (= corp-creds (:credit (get-corp))) "Corp did not gain credit when runner trashes / discards from hand")
      (play-from-hand state :runner "Aumakua")
      (is (= (+ 1 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner installed Aumakua")
      (play-from-hand state :runner "Fall Guy")
      (is (= (+ 2 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner installed Fall Guy")
      (card-ability state :runner (get-resource state 0) 1)
      (is (= (+ 3 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner trashed Fall Guy")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (is (= (+ 4 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner trashed PAD Campaign"))))
