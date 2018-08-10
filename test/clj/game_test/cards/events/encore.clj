(ns game-test.cards.events.encore
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest encore
  ;; Encore - Run all 3 central servers successfully to take another turn.  Remove Encore from game.
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}
                 :runner {:deck ["Encore"]}})
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Encore")
      (is (= 1 (count (:rfg (get-runner)))) "Encore removed from game")
      (take-credits state :runner)
      (take-credits state :runner)
      ; only get one extra turn
      (take-credits state :runner)
      (is (= 9 (:credit (get-runner))))))
  (testing "2 encores in a 5 click turn results in 2 extra turns"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}
                 :runner {:deck [(qty "Encore" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Encore")
      (play-from-hand state :runner "Encore")
      (is (= 2 (count (:rfg (get-runner)))) "2 Encores removed from game")
      (take-credits state :runner)
      (take-credits state :runner)
      ;; Two extra turns
      (take-credits state :runner)
      (is (= 13 (:credit (get-runner)))))))
