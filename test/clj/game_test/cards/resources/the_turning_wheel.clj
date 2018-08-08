(ns game-test.cards.resources.the-turning-wheel
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-turning-wheel
  ;; The Turning Wheel
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Ice Wall" "Ice Wall"]}
                 :runner {:deck ["The Turning Wheel"]}})
      (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (core/gain state :runner :click 10 :credit 10)
      (let [ttw (get-resource state 0)]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "R&D")
        (card-ability state :runner ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Runner should access 1 additional card"))))
  (testing "Access bonus shouldn't carry over to other runs if prematurely ended after spending TTW counters. #3598"
    (do-game
      (new-game {:corp {:deck ["Nisei MK II"]}
                 :runner {:deck ["The Turning Wheel"]}})
      (play-and-score state "Nisei MK II")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)))
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (core/gain state :runner :click 10 :credit 10)
      (let [nisei (get-scored state :corp 0)
            ttw (get-resource state 0)]
        (run-empty-server state "HQ")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "HQ")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "R&D")
        (card-ability state :runner ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Runner should access 1 additional card")
        (card-ability state :corp nisei 0)
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter from corp using Nisei counter")
        (run-on state "R&D")
        (is (zero? (-> @ state :run :access-bonus)) "Access bonus should be reset on new run"))))
  (testing "Spending counters shouldn't increase accesses when running a non-R&D/HQ server"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Ice Wall"]}
                 :runner {:deck ["The Turning Wheel"]}})
      (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
      (trash-from-hand state :corp "Hostile Takeover")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (core/gain state :runner :click 10 :credit 10)
      (let [ttw (get-resource state 0)]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "Archives")
        (card-ability state :runner ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Runner should access 1 additional card")
        (run-successful state)
        (is (zero? (-> (get-runner) :register :last-run :access-bonus)) "Access bonuses are zeroed out when attacked server isn't R&D or HQ")))))
