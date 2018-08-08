(ns game-test.cards.resources.patron
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest patron
  ;; Patron
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Jackson Howard"]}
                 :runner {:deck [(qty "Patron" 4) (qty "Easy Mark" 4)]}})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Patron")
      (let [p (get-resource state 0)]
        (take-credits state :runner 3)
        (take-credits state :corp)
        (click-prompt state :runner "Server 1")
        (is (= 4 (count (:hand (get-runner)))) "Starts with 4 cards")
        (run-empty-server state "Server 1")
        (is (= 6 (count (:hand (get-runner)))) "Drew 2 cards")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (= 6 (count (:hand (get-runner)))) "Drew no cards")
        (play-from-hand state :runner "Easy Mark")
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Server 1")
        (run-empty-server state "Archives")
        (is (= 5 (count (:hand (get-runner)))) "Did not draw cards when running other server"))))
  (testing "Manually selecting during Step 1.2 does not show a second prompt at start of turn. Issue #1744."
    (do-game
      (new-game {:runner {:deck [(qty "Patron" 3) (qty "Jak Sinclair" 3)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (starting-hand state :runner ["Patron" "Jak Sinclair"])
      (play-from-hand state :runner "Patron")
      (play-from-hand state :runner "Jak Sinclair")
      (take-credits state :runner)
      (let [p (get-resource state 0)
            j (get-resource state 1)]
        (take-credits state :corp)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (card-ability state :runner p 0)
        (click-prompt state :runner "Archives")
        (card-ability state :runner j 0)
        (click-prompt state :runner "Archives")
        (run-successful state)
        (core/end-phase-12 state :runner nil)
        (is (empty? (:prompt (get-runner))) "No second prompt for Patron - used already")))))
