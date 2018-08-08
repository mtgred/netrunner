(ns game-test.cards.assets.contract-killer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest contract-killer
  ;; Contract Killer
  (do-game
    (new-game {:corp {:deck ["Contract Killer"]}
               :runner {:deck [(qty "Sure Gamble" 2) "Data Dealer"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Contract Killer" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Data Dealer")
    (take-credits state :runner)
    (let [ck (get-content state :remote1 0)]
      (advance state ck 2)
      (card-ability state :corp ck 0)
      (click-card state :corp (get-resource state 0))
      (is (= 1 (-> (get-corp) :discard count)) "Contract Killer should be trashed as an ability cost")
      (is (= 1 (-> (get-runner) :discard count)) "Contract Killer should trash Data Dealer"))
    (take-credits state :corp)
    (take-credits state :runner)
    (core/gain state :corp :click 1)
    (core/move state :corp (find-card "Contract Killer" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Contract Killer" "New remote")
    (let [ck (get-content state :remote2 0)]
      (advance state ck 2)
      (card-ability state :corp ck 1)
      (is (= 1 (-> (get-corp) :discard count)) "Contract Killer should be trashed as an ability cost")
      (is (= 3 (-> (get-runner) :discard count)) "Contract Killer should do 2 meat damage"))))
