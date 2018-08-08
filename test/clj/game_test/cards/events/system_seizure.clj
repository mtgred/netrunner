(ns game-test.cards.events.system-seizure
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest system-seizure
  ;; System Seizure - First icebreaker boosted keeps strength for remainder of that run.
  (do-game
    (new-game {:corp {:deck ["Wraparound"]}
               :runner {:deck [(qty "Corroder" 2) "System Seizure"]}})
    (play-from-hand state :corp "Wraparound" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 3)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "System Seizure")
    (let [c1 (get-program state 0)
          c2  (get-program state 1)]
      (run-empty-server state "R&D") ;; Check that System Seizure triggers even if another run has been made
      (run-on state "HQ") ;; Check that System Seizure only keeps strength on one of the breakers
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (card-ability state :runner c1 1)
      (card-ability state :runner c2 1)
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-successful state)
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-on state "HQ") ;; Check that System Seizure does not keep strength on 2nd run
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (card-ability state :runner c1 1)
      (card-ability state :runner c2 1)
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-successful state)
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength"))))
