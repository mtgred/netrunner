(ns game.core.access-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.test-framework :refer :all]))

(deftest rd-access
  (testing "Nothing in R&D, no upgrades"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]
                        :hand [(qty "Hedge Fund" 4)]}})
      (click-draw state :corp)
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")
      (is (no-prompt? state :corp))))
  (testing "Something in R&D, no upgrades"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")
      (is (no-prompt? state :corp))))
  (testing "Nothing in R&D, an unrezzed upgrade"
    (do-game
      (new-game {:corp {:deck []
                        :hand [(qty "Hedge Fund" 5) "Bryan Stinson"]}})
      (play-from-hand state :corp "Bryan Stinson" "R&D")
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")
      (is (no-prompt? state :corp))))
  (testing "Something in R&D, an upgrade"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Bryan Stinson"]}})
      (play-from-hand state :corp "Bryan Stinson" "R&D")
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (= ["Card from deck" "Unrezzed upgrade"] (prompt-buttons :runner)))
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "No action")
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")
      (is (no-prompt? state :corp))))
  (testing "Accessing multiple cards from R&D"
    (do-game
      (new-game {:corp {:deck []
                        :hand [(qty "Hedge Fund" 2) (qty "Hostile Takeover" 2)]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (run-on state "R&D")
      (core/access-bonus state :runner :rd 2)
      (run-continue state)
      ;; Hedge Fund #1
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      ;; Hostile Takeover #1
      (is (= ["Steal"] (prompt-buttons :runner)))
      (click-prompt state :runner "Steal")
      ;; Hedge Fund #2
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      ;; No more accesses
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")
      (is (no-prompt? state :corp))))
  (testing "Accessing multiple cards from R&D with multiple upgrades upgrades"
    (do-game
      (new-game {:corp {:deck ["Keegan Lane" "Midway Station Grid"
                               "Sweeps Week" "Manhunt"
                               "Hedge Fund" "Big Brother"]}
                 :runner {:deck ["Medium"]}})
      (play-from-hand state :corp "Keegan Lane" "R&D")
      (play-from-hand state :corp "Midway Station Grid" "R&D")
      (rez state :corp (get-content state :rd 1))
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Sweeps Week" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Manhunt" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Big Brother" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (run-on state "R&D")
      (core/access-bonus state :runner :rd 2)
      (run-continue state)
      (is (= ["Card from deck" "Midway Station Grid" "Unrezzed upgrade"] (prompt-buttons :runner)))
      (click-prompt state :runner "Card from deck")
      (is (accessing state "Hedge Fund"))
      (click-prompt state :runner "No action")
      (is (= ["Card from deck" "Midway Station Grid" "Unrezzed upgrade"] (prompt-buttons :runner)))
      (click-prompt state :runner "Unrezzed upgrade")
      (is (accessing state "Keegan Lane"))
      (click-prompt state :runner "No action")
      (is (= ["Card from deck" "Midway Station Grid"] (prompt-buttons :runner)))
      (click-prompt state :runner "Card from deck")
      (is (accessing state "Sweeps Week"))
      (click-prompt state :runner "No action")
      (is (= ["Card from deck" "Midway Station Grid"] (prompt-buttons :runner)))
      (click-prompt state :runner "Midway Station Grid")
      (is (accessing state "Midway Station Grid"))
      (click-prompt state :runner "No action")
      (is (accessing state "Manhunt"))
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner has no access prompt")
      (is (nil? (get-run)) "Run has ended normally")))
  (testing "Looping Ganked! and Ansel"
    (do-game
      (new-game {:corp {:hand ["Ganked!" "Ansel 1.0"]}})
      (play-from-hand state :corp "Ganked!" "R&D")
      (play-from-hand state :corp "Ansel 1.0" "R&D")
      (take-credits state :corp)
      (let [ansel (get-ice state :rd 0)]
        (rez state :corp ansel)
        (run-on state "R&D")
        (run-continue-until state :success)
        (dotimes [_ 3]
          (click-prompt state :corp "Yes") ;; use ganked!
          (click-card state :corp ansel)
          (card-subroutine state :corp (refresh ansel) 1)
          (click-card state :corp "Ganked!")
          (click-prompt state :corp "R&D")
          (encounter-continue state))
        (click-prompt state :corp "No")
        (click-prompt state :runner "No action")))))

(deftest rd-total-accesses-reduced-by-1-accessing-1-card
  ;; R&D - Accesses reduced by 1 - Accessing 1 card
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand [(qty "Hedge Fund" 2) "Bryan Stinson"]}})
   (take-credits state :corp)
   (run-on state :rd)
   (core/access-bonus state :corp :total -1)
   (run-continue state)
   (is (no-prompt? state :corp))
   (is (no-prompt? state :runner))
   (is (nil? (get-run)))))

(deftest rd-total-accesses-reduced-by-1-accessing-1-deck-1-upgrade
  ;; R&D - Accesses reduced by 1 - Accessing 1 card from deck and 1 upgrade
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand [(qty "Hedge Fund" 2) "Bryan Stinson"]}})
   (play-from-hand state :corp "Bryan Stinson" "R&D")
   (take-credits state :corp)
   (run-on state :rd)
   (core/access-bonus state :corp :total -1)
   (run-continue state)
   (click-prompt state :runner "Card from deck")
   (click-prompt state :runner "No action")
   (is (no-prompt? state :corp))
   (is (no-prompt? state :runner))
   (is (nil? (get-run)))))

(deftest rd-total-accesses-reduced-by-1-accessing-3-deck
  ;; R&D - Accesses reduced by 1 - Accessing 3 cards from deck
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand [(qty "Hedge Fund" 2) "Bryan Stinson"]}
              :runner {:hand ["The Maker's Eye"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "The Maker's Eye")
   (core/access-bonus state :corp :total -1)
   (run-continue state)
   (click-prompt state :runner "No action")
   (click-prompt state :runner "No action")
   (is (no-prompt? state :corp))
   (is (no-prompt? state :runner))
   (is (nil? (get-run)))))

(deftest hq-access
  (testing "Nothing in HQ, no upgrades"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand []}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")))
  (testing "Something in HQ, no upgrades"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")))
  (testing "Nothing in HQ, an unrezzed upgrade"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Bryan Stinson"]}})
      (play-from-hand state :corp "Bryan Stinson" "HQ")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")))
  (testing "Nothing in HQ, multiple unrezzed upgrades"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Bryan Stinson"  "Expo Grid"]}})
      (play-from-hand state :corp "Bryan Stinson" "HQ")
      (play-from-hand state :corp "Expo Grid" "HQ")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-card state :runner (get-content state :hq 0))
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (= ["Pay 3 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")))
  (testing "Something in HQ, an upgrade"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund" "Bryan Stinson"]}})
      (play-from-hand state :corp "Bryan Stinson" "HQ")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (= ["Card from hand" "Unrezzed upgrade"] (prompt-buttons :runner)))
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "No action")
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner) "Runner has no access prompt")))
  (testing "when access is limited to a single card, access only it"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 2) "Bryan Stinson"]}})
      (play-from-hand state :corp "Bryan Stinson" "HQ")
      (let [bryan (get-content state :hq 0)]
        (rez state :corp bryan)
        (take-credits state :corp)
        (run-on state "HQ")
        (core/set-only-card-to-access state :corp bryan))
      (run-continue state)
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))
      (is (nil? (get-run)))))
  (testing "Looping Ganked! and Ansel"
    (do-game
     (new-game {:corp {:hand ["Ganked!" "Ansel 1.0"]}})
     (play-from-hand state :corp "Ganked!" "HQ")
     (play-from-hand state :corp "Ansel 1.0" "HQ")
     (take-credits state :corp)
     (let [ansel (get-ice state :hq 0)]
       (rez state :corp ansel)
       (run-on state "HQ")
       (run-continue-until state :success)
       (dotimes [_ 3]
         (click-prompt state :corp "Yes") ;; use ganked!
         (click-card state :corp ansel)
         (card-subroutine state :corp (refresh ansel) 1)
         (click-card state :corp "Ganked!")
         (click-prompt state :corp "HQ")
         (encounter-continue state))
       (click-prompt state :corp "No")
       (click-prompt state :runner "No action")))))

(deftest hq-total-accesses-reduced-by-1-accessing-1-card
  ;; HQ - Accesses reduced by 1 - Accessing 1 card
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand [(qty "Hedge Fund" 2) "Bryan Stinson"]}})
   (take-credits state :corp)
   (run-on state :hq)
   (core/access-bonus state :corp :total -1)
   (run-continue state)
   (is (no-prompt? state :corp))
   (is (no-prompt? state :runner))
   (is (nil? (get-run)))))

(deftest hq-total-accesses-reduced-by-1-accessing-1-hand-1-upgrade
  ;; HQ - Accesses reduced by 1 - Accessing 1 card from hand and 1 upgrade
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand [(qty "Hedge Fund" 2) "Bryan Stinson"]}})
   (play-from-hand state :corp "Bryan Stinson" "HQ")
   (take-credits state :corp)
   (run-on state :hq)
   (core/access-bonus state :corp :total -1)
   (run-continue state)
   (click-prompt state :runner "Card from hand")
   (click-prompt state :runner "No action")
   (is (no-prompt? state :corp))
   (is (no-prompt? state :runner))
   (is (nil? (get-run)))))

(deftest hq-total-accesses-reduced-by-1-accessing-2-hand
  ;; HQ - Accesses reduced by 1 - Accessing 2 cards from hand
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand [(qty "Hedge Fund" 2) "Bryan Stinson"]}
              :runner {:hand ["Docklands Pass"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Docklands Pass")
   (run-on state :hq)
   (core/access-bonus state :corp :total -1)
   (run-continue state)
   (click-prompt state :runner "No action")
   (is (no-prompt? state :corp))
   (is (no-prompt? state :runner))
   (is (nil? (get-run)))))

(deftest archives-access
  (testing "Nothing in archives"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (no-prompt? state :runner) "Runner has no access prompt")))
  (testing "only non-interactive cards"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Hedge Fund" "Beanstalk Royalties"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (no-prompt? state :runner) "Runner has no access prompt")))
  (testing "contains one agenda"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Hostile Takeover"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= ["Steal"] (prompt-buttons :runner)))
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))))
  (testing "contains multiple agendas"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Hostile Takeover" "15 Minutes"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= ["Hostile Takeover" "15 Minutes"] (prompt-buttons :runner)))
      (click-prompt state :runner "Hostile Takeover")
      (click-prompt state :runner "Steal")
      (is (accessing state "15 Minutes"))
      (click-prompt state :runner "Steal")
      (is (nil? (get-run)))
      (is (no-prompt? state :runner))
      (is (= 2 (:agenda-point (get-runner))))))
  (testing "contains one access ability"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Cyberdex Virus Suite"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (prompt-is-type? state :runner :waiting))
      (click-prompt state :corp "Yes")))
  (testing "contains multiple access abilities"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Cyberdex Virus Suite" "Shock!"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= ["Cyberdex Virus Suite" "Shock!"] (prompt-buttons :runner)))
      (click-prompt state :runner "Shock!")
      (is (prompt-is-type? state :runner :waiting) "Accessing CVS")
      (click-prompt state :corp "Yes")))
  (testing "contains agendas and access abilities"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Hostile Takeover" "Cyberdex Virus Suite"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= ["Hostile Takeover" "Cyberdex Virus Suite"] (prompt-buttons :runner)))
      (click-prompt state :runner "Cyberdex Virus Suite")
      (is (prompt-is-type? state :runner :waiting))
      (click-prompt state :corp "Yes")
      (is (accessing state "Hostile Takeover"))
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))))
  (testing "contains non-interactive cards, agendas, and access abilities"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Hedge Fund" "Hostile Takeover" "Cyberdex Virus Suite"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= ["Hostile Takeover" "Cyberdex Virus Suite" "Everything else"] (prompt-buttons :runner)))
      (click-prompt state :runner "Cyberdex Virus Suite")
      (is (prompt-is-type? state :runner :waiting))
      (click-prompt state :corp "Yes")
      (is (= ["Hostile Takeover" "Everything else"] (prompt-buttons :runner)))
      (click-prompt state :runner "Hostile Takeover")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))
      (is (nil? (get-run)))))
  (testing "when access count is reduced"
    (testing "by 1"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Bryan Stinson"]
                          :discard ["Hedge Fund" "Hostile Takeover"]}
                   :runner {:credits 10}})
        (play-from-hand state :corp "Bryan Stinson" "Archives")
        (rez state :corp (get-content state :archives 0))
        (take-credits state :corp)
        (run-on state "Archives")
        (core/access-bonus state :corp :total -1)
        (run-continue state)
        (is (= ["Hostile Takeover" "Bryan Stinson" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Bryan Stinson")
        (click-prompt state :runner "No action")
        (is (= ["Hostile Takeover" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Everything else")
        (is (zero? (:agenda-point (get-runner))) "Runner doesn't access last card in Archives")
        (is (no-prompt? state :corp))
        (is (no-prompt? state :runner))
        (is (nil? (get-run)))))
    (testing "by more than 1"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Bryan Stinson"]
                          :discard ["Hedge Fund" "Hostile Takeover" "Shock!"]}
                   :runner {:credits 10}})
        (play-from-hand state :corp "Bryan Stinson" "Archives")
        (rez state :corp (get-content state :archives 0))
        (take-credits state :corp)
        (run-on state "Archives")
        (core/access-bonus state :corp :total -2)
        (run-continue state)
        (is (= ["Hostile Takeover" "Shock!" "Bryan Stinson" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Bryan Stinson")
        (click-prompt state :runner "No action")
        (is (= ["Hostile Takeover" "Shock!" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Everything else")
        (is (zero? (:agenda-point (get-runner))) "Runner didn't access Hostile Takeover")
        (is (zero? (count (:discard (get-runner)))) "Runner didn't access Shock!")
        (is (no-prompt? state :corp))
        (is (no-prompt? state :runner))
        (is (nil? (get-run))))))
  (testing "when access is limited to a single card, access only it #5015"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Bryan Stinson"]
                        :discard ["Hostile Takeover" "Cyberdex Virus Suite"]}})
      (play-from-hand state :corp "Bryan Stinson" "Archives")
      (let [bryan (get-content state :archives 0)]
        (rez state :corp bryan)
        (take-credits state :corp)
        (run-on state "Archives")
        (core/set-only-card-to-access state :corp bryan))
      (run-continue state)
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))
      (is (nil? (get-run)))))
  (testing "when a card is turned facedown mid-access"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Bryan Stinson"]
                        :discard ["Hostile Takeover" "Cyberdex Virus Suite" "Hedge Fund"]}})
      (play-from-hand state :corp "Bryan Stinson" "Archives")
      (rez state :corp (get-content state :archives 0))
      (take-credits state :corp)
      (run-on state "Archives")
      (run-continue state)
      (is (= ["Hostile Takeover" "Cyberdex Virus Suite" "Bryan Stinson" "Everything else"] (prompt-buttons :runner)))
      (click-prompt state :runner "Hostile Takeover")
      (core/update! state :corp (dissoc (find-card "Hedge Fund" (:discard (get-corp))) :seen))
      (click-prompt state :runner "Steal")
      (is (= ["Cyberdex Virus Suite" "Bryan Stinson" "Facedown card in Archives"] (prompt-buttons :runner)))
      (click-prompt state :runner "Cyberdex Virus Suite")
      (click-prompt state :corp "No")
      (is (= ["Bryan Stinson" "Facedown card in Archives"] (prompt-buttons :runner)))
      (click-prompt state :runner "Facedown card in Archives")
      (click-prompt state :runner "No action")
      (is (second-last-log-contains? state "Runner accesses Hedge Fund from Archives."))
      (is (accessing state "Bryan Stinson"))
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))
      (is (nil? (get-run)))))
  (testing "stealing multiple agendas from archives"
      (do-game
        (new-game {:corp {:discard [(qty "Breaking News" 3)]}})
        (take-credits state :corp)
        (run-empty-server state :archives)
        (click-prompt state :runner "Breaking News")
        (click-prompt state :runner "Steal")
        (click-prompt state :runner "Breaking News")
        (click-prompt state :runner "Steal")
        (is (accessing state "Breaking News"))
        (click-prompt state :runner "Steal")
        (is (= 3 (count (:scored (get-runner)))) "3 agendas stolen")
        (is (empty? (:discard (get-corp))) "0 agendas left in archives")))
  (testing "choosing Everything else first #5151"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :discard ["Global Food Initiative" (qty "Blue Level Clearance" 3)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= ["Global Food Initiative" "Everything else"] (prompt-buttons :runner)))
      (click-prompt state :runner "Everything else")
      (is (second-last-log-contains? state "Runner accesses everything else in Archives"))
      (is (accessing state "Global Food Initiative"))
      (is (= ["Steal"] (prompt-buttons :runner)))
      (click-prompt state :runner "Steal")))
  (testing "Looping Ganked! and Ansel"
    (do-game
      (new-game {:corp {:hand ["Ganked!" "Ansel 1.0"]}})
      (play-from-hand state :corp "Ganked!" "Archives")
      (play-from-hand state :corp "Ansel 1.0" "Archives")
      (take-credits state :corp)
      (let [ansel (get-ice state :archives 0)]
        (rez state :corp ansel)
        (run-on state "Archives")
        (run-continue-until state :success)
        (dotimes [_ 3]
          (click-prompt state :corp "Yes") ;; use ganked!
          (click-card state :corp ansel)
          (card-subroutine state :corp (refresh ansel) 1)
          (click-card state :corp "Ganked!")
          (click-prompt state :corp "Archives")
          (encounter-continue state))
        (click-prompt state :corp "No")
        (click-prompt state :runner "No action")))))

(deftest remote-access
  (testing "reduced by 1. #5014"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Bryan Stinson"]
                        :discard ["Hedge Fund" "Hostile Takeover"]}
                 :runner {:credits 10}})
      (play-from-hand state :corp "Bryan Stinson" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state "Server 1")
      (core/access-bonus state :runner :total -1)
      (run-continue state)
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner) "Runner has no access prompt")
      (is (nil? (get-run)))))
  (testing "Looping Ganked! and Ansel"
    (do-game
      (new-game {:corp {:hand ["Ganked!" "Ansel 1.0"]}})
      (play-from-hand state :corp "Ganked!" "New remote")
      (play-from-hand state :corp "Ansel 1.0" "Server 1")
      (take-credits state :corp)
      (let [ansel (get-ice state :remote1 0)]
        (rez state :corp ansel)
        (run-on state "Server 1")
        (run-continue-until state :success)
        (dotimes [_ 3]
          (click-prompt state :corp "Yes") ;; use ganked!
          (click-card state :corp ansel)
          (card-subroutine state :corp (refresh ansel) 1)
          (click-card state :corp "Ganked!")
          (click-prompt state :corp "Server 1")
          (encounter-continue state))
        (click-prompt state :corp "No")
        (click-prompt state :runner "No action")))))

(deftest access-count
  (testing "rd"
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                       :hand [(qty "Hedge Fund" 2)]}})
     (is (= {:random-access-limit 1 :total-mod 0 :chosen 0} (core/num-cards-to-access state :runner :rd nil)))
     (core/access-bonus state :runner :rd 2)
     (is (= {:random-access-limit 3 :total-mod 0 :chosen 0} (core/num-cards-to-access state :runner :rd nil)))
     (core/access-bonus state :runner :total -1)
     (is (= {:random-access-limit 3 :total-mod -1 :chosen 0} (core/num-cards-to-access state :runner :rd nil)))))
  (testing "hq"
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                       :hand [(qty "Hedge Fund" 2)]}})
     (is (= {:random-access-limit 1 :total-mod 0 :chosen 0} (core/num-cards-to-access state :runner :hq nil)))
     (core/access-bonus state :runner :hq 2)
     (is (= {:random-access-limit 3 :total-mod 0 :chosen 0} (core/num-cards-to-access state :runner :hq nil)))
     (core/access-bonus state :runner :total -1)
     (is (= {:random-access-limit 3 :total-mod -1 :chosen 0} (core/num-cards-to-access state :runner :hq nil)))))
  (testing "archives"
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                       :discard [(qty "Hedge Fund" 2)]}})
     (is (= {:total-mod 0 :chosen 0} (core/num-cards-to-access state :runner :archives nil)))
     (core/access-bonus state :runner :total -1)
     (is (= {:total-mod -1 :chosen 0} (core/num-cards-to-access state :runner :archives nil))))))
