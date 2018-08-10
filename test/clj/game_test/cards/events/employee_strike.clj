(ns game-test.cards.events.employee-strike
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest employee-strike
  ;; Employee Strike
  (testing "vs Blue Sun, suppress Step 1.2"
    (do-game
      (new-game {:corp {:id "Blue Sun: Powering the Future"
                        :deck ["Ice Wall"]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Employee Strike suppressed Blue Sun step 1.2")))
  (testing "vs PU/Philotic - test for #2688"
    (do-game
      (new-game {:corp {:id "Jinteki: Potential Unleashed"
                        :deck ["Philotic Entanglement" (qty "Braintrust" 2)]}
                 :runner {:deck [(qty "Employee Strike" 10)]}})
      (play-from-hand state :corp "Braintrust" "New remote")
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Steal")
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (play-from-hand state :corp "Philotic Entanglement" "New remote")
      (score-agenda state :corp (get-content state :remote3 0))
      (is (= 3 (count (:discard (get-runner))))
          "Discard is 3 cards - 2 from Philotic, 1 EStrike.  Nothing from PU mill"))))
