(ns game-test.cards.resources.security-testing
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest security-testing
  ;; Security Testing
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Jackson Howard"]}
                 :runner {:deck ["Security Testing"]}})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Security Testing")
      (let [st (get-resource state 0)]
        (take-credits state :runner 3)
        (take-credits state :corp)
        (click-prompt state :runner "Server 1")
        (run-empty-server state "Server 1")
        (is (= 10 (:credit (get-runner))) "Gained 2 credits from Security Testing")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (= 10 (:credit (get-runner))) "Did not gain credits on second run")
        (take-credits state :runner 2)
        (take-credits state :corp)
        (click-prompt state :runner "Server 1")
        (run-empty-server state "Archives")
        (is (= 12 (:credit (get-runner))) "Did not gain credits when running other server"))))
  (testing "with multiple copies"
    (do-game
      (new-game {:runner {:deck [(qty "Security Testing" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Archives")
      (click-prompt state :runner "R&D")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-runner))) "Gained 2 credits")
      (run-empty-server state "R&D")
      (is (= 11 (:credit (get-runner)))))))
