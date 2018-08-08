(ns game-test.cards.assets.sealed-vault
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sealed-vault
  ;; Sealed Vault - Store credits for 1c, retrieve credits by trashing or spending click
  (do-game
    (new-game {:corp {:deck ["Sealed Vault" "Hedge Fund"]}})
    (play-from-hand state :corp "Sealed Vault" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (let [sv (get-content state :remote1 0)]
      (core/rez state :corp sv)
      (card-ability state :corp sv 0)
      (click-prompt state :corp "8")
      (is (= 8 (get-counters (refresh sv) :credit)) "8 credits stored on Sealed Vault")
      (is (zero? (:credit (get-corp))))
      (card-ability state :corp sv 1)
      (click-prompt state :corp "8")
      (is (zero? (get-counters (refresh sv) :credit)) "Credits removed from Sealed Vault")
      (is (= 8 (:credit (get-corp))))
      (is (zero? (:click (get-corp))) "Spent a click")
      (card-ability state :corp sv 0)
      (click-prompt state :corp "7")
      (is (= 7 (get-counters (refresh sv) :credit)) "7 credits stored on Sealed Vault")
      (is (zero? (:credit (get-corp))))
      (card-ability state :corp sv 2)
      (click-prompt state :corp "7")
      (is (= 7 (:credit (get-corp))))
      (is (= 2 (count (:discard (get-corp)))) "Sealed Vault trashed"))))
