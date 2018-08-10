(ns game-test.cards.resources.bank-job
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bank-job
  ;; Bank Job
  (testing "Manhunt trace happens first"
    (do-game
      (new-game {:corp {:deck ["Manhunt" "PAD Campaign"]}
                 :runner {:deck ["Bank Job"]}})
      (play-from-hand state :corp "Manhunt")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "2") ; Manhunt trace active
      (click-prompt state :runner "0")
      (click-prompt state :runner "Replacement effect")
      (is (= "Bank Job" (:title (:card (first (get-in @state [:runner :prompt])))))
          "Bank Job prompt active")
      (click-prompt state :runner "8")
      (is (empty? (get-resource state)) "Bank Job trashed after all credits taken")
      (is (= 1 (count (:discard (get-runner)))))))
  (testing "Choose which to use when 2+ copies are installed"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck [(qty "Bank Job" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Replacement effect")
      (click-prompt state :runner "4")
      (play-from-hand state :runner "Bank Job")
      (let [bj1 (get-resource state 0)
            bj2 (get-resource state 1)]
        (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Replacement effect")
        (click-card state :runner bj2)
        (click-prompt state :runner "6")
        (is (= 13 (:credit (get-runner))))
        (is (= 2 (get-counters (refresh bj2) :credit)) "2 credits remaining on 2nd copy"))))
  (testing "Security Testing takes priority"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Bank Job" "Security Testing"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Bank Job")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Server 1")
      (is (= 6 (:credit (get-runner))))
      (run-empty-server state "Server 1")
      (is (empty? (:prompt (get-runner))) "No Bank Job replacement choice")
      (is (= 8 (:credit (get-runner))) "Security Testing paid 2c"))))
