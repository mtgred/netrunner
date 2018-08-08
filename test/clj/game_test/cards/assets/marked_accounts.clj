(ns game-test.cards.assets.marked-accounts
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest marked-accounts
  ;; Marked Accounts
  (do-game
    (new-game {:corp {:deck ["Marked Accounts"]}})
    (play-from-hand state :corp "Marked Accounts" "New remote")
    (let [ma (get-content state :remote1 0)]
      (core/rez state :corp ma)
      (is (zero? (get-counters (refresh ma) :credit)) "Marked Accounts should start with 0 credits on it")
      (card-ability state :corp ma 1)
      (is (= 3 (get-counters (refresh ma) :credit)) "Marked Accounts should gain 3 credits when ability is used")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))]
        (take-credits state :runner)
        (is (= (+ credits 1) (:credit (get-corp))) "Should gain 1 credit at beginning of turn from Marked Accounts")))))
