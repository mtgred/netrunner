(ns game-test.cards.assets.sundew
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sundew
  ;; Sundew
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Sundew"]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp sund)
        (take-credits state :corp 2)
        (is (= 5 (:credit (get-corp))) "Cost 2cr to rez")
        ;; spend a click not on a run
        (take-credits state :runner)
        (is (= 7 (:credit (get-corp))) "Corp gained 2cr from Sundew")
        (take-credits state :corp)
        (run-on state "Server 1")
        ; (is (= 10 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew")
        (is (= 3 (:click (get-runner))) "Runner spent 1 click to start run")))))
(deftest-pending sundew-run
  (testing "Sundew - Dirty Laundry"
    (do-game
      (new-game {:corp {:deck ["Sundew"]}
                 :runner {:deck ["Dirty Laundry"]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp (refresh sund))
        (is (= 3 (:credit (get-corp))) "Cost 2cr to rez")
        (take-credits state :corp)
        (play-from-hand state :runner "Dirty Laundry")
        (click-prompt state :runner "Server 1")
        ;; spend a click on a run through a card, not through click-run
        (is (= 5 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew")))))
