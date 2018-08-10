(ns game-test.cards.identities.chronos-protocol-selective-mind-mapping
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest chronos-protocol-selective-mind-mapping
  ;; Chronos Protocol - Choose Runner discard for first net damage of a turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck ["Pup" (qty "Neural EMP" 2)]}
                 :runner {:deck [(qty "Imp" 3)]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (run-on state :hq)
      (let [pup (get-ice state :hq 0)]
        (core/rez state :corp pup)
        (card-subroutine state :corp pup 0)
        (click-prompt state :corp "Yes")
        (let [imp (find-card "Imp" (:hand (get-runner)))]
          (click-prompt state :corp imp)
          (is (= 1 (count (:discard (get-runner)))))
          (card-subroutine state :corp pup 0)
          (is (empty? (:prompt (get-corp))) "No choice on second net damage")
          (is (= 2 (count (:discard (get-runner)))))
          (run-jack-out state)
          (take-credits state :runner)
          (core/move state :runner (find-card "Imp" (:discard (get-runner))) :hand)
          (play-from-hand state :corp "Neural EMP")
          (click-prompt state :corp "No")
          (is (= 2 (count (:discard (get-runner)))) "Damage dealt after declining ability")
          (play-from-hand state :corp "Neural EMP")
          (is (empty? (:prompt (get-corp))) "No choice after declining on first damage")
          (is (= 3 (count (:discard (get-runner)))))))))
  (testing "with Obokata: Pay 4 net damage to steal.  Only 3 damage left after Chronos.  No trigger of damage prevent."
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck [(qty "Obokata Protocol" 5)]}
                 :runner {:deck [(qty "Sure Gamble" 3) "Inti" "Feedback Filter"]}})
      (core/gain state :runner :credit 10)
      (play-from-hand state :corp "Obokata Protocol" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 net damage to steal")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp (find-card "Inti" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "Feedback Filter net damage prevention opportunity not given")
      (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 net damage")))
  (testing "vs Employee Strike. Issue #1958"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck ["Pup"]}
                 :runner {:deck ["Employee Strike" (qty "Scrubbed" 3) "Sure Gamble"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (run-on state :hq)
      (let [pup (get-ice state :hq 0)]
        (core/rez state :corp pup)
        (card-subroutine state :corp pup 0)
        (is (empty? (:prompt (get-corp))) "No choice because of Employee Strike")
        (card-subroutine state :corp pup 0)
        (is (= 2 (count (:discard (get-runner)))))
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (play-from-hand state :runner "Scrubbed")
        (run-on state :hq)
        (card-subroutine state :corp pup 0)
        (is (not (empty? (:prompt (get-corp)))) "Employee Strike out of play - Ability turned on correctly")))))
