(ns game-test.cards.upgrades.prisec
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest prisec
  ;; Prisec
  (testing "Basic test - Pay 2 credits to give runner 1 tag and do 1 meat damage, only when installed"
    (do-game
      (new-game {:corp {:deck [(qty "Prisec" 2)]}})
      (play-from-hand state :corp "Prisec" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (let [pre-creds (:credit (get-corp))]
        (click-prompt state :corp "Yes")
        (is (= (- pre-creds 2) (:credit (get-corp))) "Pay 2 [Credits] to pay for Prisec"))
      (is (= 1 (:tag (get-runner))) "Give runner 1 tag")
      (is (= 1 (count (:discard (get-runner)))) "Prisec does 1 damage")
      ;; Runner trashes Prisec
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (run-empty-server state "HQ")
      (is (not (:prompt @state)) "Prisec does not trigger from HQ")))
  (testing "Multiple unrezzed upgrades in Archives interaction with DRT"
    (do-game
      (new-game {:corp {:deck [(qty "Prisec" 2) "Dedicated Response Team"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (play-from-hand state :corp "Prisec" "Archives")
      (play-from-hand state :corp "Prisec" "Archives")
      (core/gain state :corp :click 1 :credit 14)
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :archives)
      (is (:run @state) "Run still active")
      (click-prompt state :runner "Unrezzed upgrade in Archives")
      (click-card state :runner (get-content state :archives 0))
      (click-prompt state :corp "Yes") ; corp pay for PriSec
      (click-prompt state :runner "No action") ; runner don't pay to trash
      (is (:run @state) "Run still active")
      (click-prompt state :runner "Unrezzed upgrade in Archives")
      (click-prompt state :corp "Yes") ; corp pay for PriSec
      (click-prompt state :runner "No action") ; runner don't pay to trash
      (is (not (:run @state)) "Run ended")
      (is (= 4 (count (:discard (get-runner)))) "Runner took 4 meat damage"))))
