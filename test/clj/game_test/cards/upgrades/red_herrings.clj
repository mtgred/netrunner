(ns game-test.cards.upgrades.red-herrings
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest red-herrings
  ;; Red Herrings
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Red Herrings" "House of Knives"]}})
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [rh (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        ;; prompt should be asking for the 5cr cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 5cr")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner rh)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay 5 [Credits] to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Cost increase even when trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Red Herrings" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [rh (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        ;; should now have prompt to pay 5cr for HoK
        (click-prompt state :runner "Pay 5 [Credits] to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Trashed from HQ"
    (do-game
      (new-game {:corp {:deck ["Red Herrings" "House of Knives"]}})
      (trash-from-hand state :corp "Red Herrings")
      (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      ;; prompt should be asking to steal HoK
      (is (= "Steal" (first (:choices (first (:prompt (get-runner))))))
          "Runner being asked to Steal")))
  (testing "Don't affect runs on other servers"
    (do-game
      (new-game {:corp {:deck ["Red Herrings" "House of Knives"]}})
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "New remote")
      (take-credits state :corp 1)
      (let [rh (get-content state :remote1 0)]
        (core/rez state :corp rh)
        (run-empty-server state "Server 2")
        ;; access is automatic
        (click-prompt state :runner "Steal")
        (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda")))))
