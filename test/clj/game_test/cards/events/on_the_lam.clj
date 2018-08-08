(ns game-test.cards.events.on-the-lam
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest on-the-lam
  ;; On the Lam
  (testing "vs tags"
    (do-game
      (new-game {:corp {:deck ["SEA Source"]}
                 :runner {:deck ["Daily Casts" "On the Lam"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "On the Lam")
      (click-card state :runner (get-resource state 0))
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (card-ability state :runner (-> (get-resource state 0) :hosted first) 0)
      (click-prompt state :runner "Done")
      (is (zero? (:tag (get-runner))) "Runner should avoid tag")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should have 1 card in Heap")))
  (testing "vs damage"
    (do-game
      (new-game {:corp {:deck ["Show of Force"]}
                 :runner {:deck ["Daily Casts" "On the Lam"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "On the Lam")
      (click-card state :runner (get-resource state 0))
      (take-credits state :runner)
      (play-and-score state "Show of Force")
      (card-ability state :runner (-> (get-resource state 0) :hosted first) 1)
      (click-prompt state :runner "Done")
      (is (zero? (:tag (get-runner))) "Runner should avoid all meat damage")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should have 1 card in Heap"))))
