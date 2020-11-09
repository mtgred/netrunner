(ns game.core.access-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest rd-access
  (testing "Nothing in R&D, no upgrades"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]
                        :hand [(qty "Hedge Fund" 4)]}})
      (click-draw state :corp)
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (nil? (get-run)))
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")
      (is (empty? (:prompt (get-corp))))))
  (testing "Something in R&D, no upgrades"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")
      (is (empty? (:prompt (get-corp))))))
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
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")
      (is (empty? (:prompt (get-corp))))))
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
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")
      (is (empty? (:prompt (get-corp))))))
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
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")
      (is (empty? (:prompt (get-corp))))))
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
      (is (= "You accessed Hedge Fund." (:msg (prompt-map :runner))))
      (click-prompt state :runner "No action")
      (is (= ["Card from deck" "Midway Station Grid" "Unrezzed upgrade"] (prompt-buttons :runner)))
      (click-prompt state :runner "Unrezzed upgrade")
      (is (= "You accessed Keegan Lane." (:msg (prompt-map :runner))))
      (click-prompt state :runner "No action")
      (is (= ["Card from deck" "Midway Station Grid"] (prompt-buttons :runner)))
      (click-prompt state :runner "Card from deck")
      (is (= "You accessed Sweeps Week." (:msg (prompt-map :runner))))
      (click-prompt state :runner "No action")
      (is (= ["Card from deck" "Midway Station Grid"] (prompt-buttons :runner)))
      (click-prompt state :runner "Midway Station Grid")
      (is (= "You accessed Midway Station Grid." (:msg (prompt-map :runner))))
      (click-prompt state :runner "No action")
      (is (= "You accessed Manhunt." (:msg (prompt-map :runner))))
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")
      (is (nil? (get-run)) "Run has ended normally")))
  )

(deftest hq-access
  (testing "Nothing in HQ, no upgrades"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand []}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (nil? (get-run)))
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")))
  (testing "Something in HQ, no upgrades"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")))
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
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")))
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
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")))
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
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")))
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
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))))
      (is (nil? (get-run))))))

(deftest archives-access
  (testing "Nothing in archives"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")))
  (testing "only non-interactive cards"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :discard ["Hedge Fund" "Beanstalk Royalties"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")))
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
      (click-prompt state :runner "15 Minutes")
      (click-prompt state :runner "Steal")
      (is (nil? (get-run)))
      (is (empty? (:prompt (get-runner))))
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
      (click-prompt state :runner "Cyberdex Virus Suite")
      (is (prompt-is-type? state :runner :waiting))
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
      (is (= ["Hostile Takeover"] (prompt-buttons :runner)))
      (click-prompt state :runner "Hostile Takeover")
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
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))))
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
        (core/access-bonus state :corp :archives -1)
        (run-continue state)
        (is (= ["Hostile Takeover" "Bryan Stinson" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Bryan Stinson")
        (click-prompt state :runner "No action")
        (is (= ["Hostile Takeover" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Everything else")
        (is (zero? (:agenda-point (get-runner))) "Runner doesn't access last card in Archives")
        (is (empty? (:prompt (get-corp))))
        (is (empty? (:prompt (get-runner))))
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
        (core/access-bonus state :corp :archives -2)
        (run-continue state)
        (is (= ["Hostile Takeover" "Shock!" "Bryan Stinson" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Bryan Stinson")
        (click-prompt state :runner "No action")
        (is (= ["Hostile Takeover" "Shock!" "Everything else"] (prompt-buttons :runner)))
        (click-prompt state :runner "Everything else")
        (is (zero? (:agenda-point (get-runner))) "Runner didn't access Hostile Takeover")
        (is (zero? (count (:discard (get-runner)))) "Runner didn't access Shock!")
        (is (empty? (:prompt (get-corp))))
        (is (empty? (:prompt (get-runner))))
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
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))))
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
      (is (last-log-contains? state "Runner accesses Hedge Fund from Archives."))
      (is (= ["Bryan Stinson"] (prompt-buttons :runner)))
      (click-prompt state :runner "Bryan Stinson")
      (is (= ["Pay 5 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))))
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
        (click-prompt state :runner "Breaking News")
        (click-prompt state :runner "Steal")
        (is (= 3 (count (:scored (get-runner)))) "3 agendas stolen")
        (is (empty (:discard (get-corp))) "0 agendas left in archives")))
  (testing "choosing Everything else first #5151"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :discard ["Global Food Initiative" (qty "Blue Level Clearance" 3)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= ["Global Food Initiative" "Everything else"] (prompt-buttons :runner)))
      (click-prompt state :runner "Everything else")
      (is (last-log-contains? state "Runner accesses everything else in Archives"))
      (is (= ["Global Food Initiative"] (prompt-buttons :runner)))
      (click-prompt state :runner "Global Food Initiative")
      (is (= ["Steal"] (prompt-buttons :runner)))
      (click-prompt state :runner "Steal"))))

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
      (core/access-bonus state :runner :remote1 -1)
      (run-continue state)
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))) "Runner has no access prompt")
      (is (nil? (get-run))))))

(deftest access-count
  (testing "rd"
    (testing "with no upgrades"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                          :hand [(qty "Hedge Fund" 2)]}})
        (is (= {:base 1 :total 1} (core/num-cards-to-access state :runner :rd nil)))
        (core/access-bonus state :runner :rd 2)
        (is (= {:base 2 :total 2} (core/num-cards-to-access state :runner :rd nil))
            "Limited by number of cards in R&D")
        (core/access-bonus state :runner :total -1)
        (is (= {:base 2 :total 1} (core/num-cards-to-access state :runner :rd nil)))))
    (testing "with some upgrades"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                          :hand [(qty "Midori" 2)]}})
        (play-from-hand state :corp "Midori" "R&D")
        (is (= {:base 1 :total 2} (core/num-cards-to-access state :runner :rd nil)))
        (core/access-bonus state :runner :rd 2)
        (is (= {:base 2 :total 3} (core/num-cards-to-access state :runner :rd nil))
            "Limited by number of cards in R&D")
        (play-from-hand state :corp "Midori" "R&D")
        (is (= {:base 2 :total 4} (core/num-cards-to-access state :runner :rd nil)))
        (core/access-bonus state :runner :total -1)
        (is (= {:base 2 :total 3} (core/num-cards-to-access state :runner :rd nil))))))
  (testing "hq"
    (testing "with no upgrades"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                          :hand [(qty "Hedge Fund" 2)]}})
        (is (= {:base 1 :total 1} (core/num-cards-to-access state :runner :hq nil)))
        (core/access-bonus state :runner :hq 2)
        (is (= {:base 2 :total 2} (core/num-cards-to-access state :runner :hq nil))
            "Limited by number of cards in R&D")
        (core/access-bonus state :runner :total -1)
        (is (= {:base 2 :total 1} (core/num-cards-to-access state :runner :hq nil)))))
    (testing "with some upgrades"
      (do-game
        (new-game {:corp {:deck [(qty "Midori" 2)]
                          :hand [(qty "Hedge Fund" 2) "Midori"]}})
        (play-from-hand state :corp "Midori" "HQ")
        (is (= {:base 1 :total 2} (core/num-cards-to-access state :runner :hq nil)))
        (core/access-bonus state :runner :hq 2)
        (is (= {:base 2 :total 3} (core/num-cards-to-access state :runner :hq nil))
            "Limited by number of cards in R&D")
        (core/move state :corp (find-card "Midori" (:deck (get-corp))) :hand)
        (play-from-hand state :corp "Midori" "HQ")
        (is (= {:base 2 :total 4} (core/num-cards-to-access state :runner :hq nil)))
        (core/access-bonus state :runner :total -1)
        (is (= {:base 2 :total 3} (core/num-cards-to-access state :runner :hq nil))))))
  (testing "archives"
    (testing "with no upgrades"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                          :discard [(qty "Hedge Fund" 2)]}})
        (is (= {:base 2 :total 2} (core/num-cards-to-access state :runner :archives nil))
            "Access all cards in Archives by default")
        (core/access-bonus state :runner :archives 2)
        (is (= {:base 2 :total 2} (core/num-cards-to-access state :runner :archives nil)))
        (core/access-bonus state :runner :total -1)
        (is (= {:base 2 :total 1} (core/num-cards-to-access state :runner :archives nil)))))
    (testing "with some upgrades"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                          :hand [(qty "Midori" 2)]
                          :discard [(qty "Hedge Fund" 2)]}})
        (play-from-hand state :corp "Midori" "Archives")
        (is (= {:base 2 :total 3} (core/num-cards-to-access state :runner :archives nil)))
        (core/access-bonus state :runner :archives 2)
        (is (= {:base 2 :total 3} (core/num-cards-to-access state :runner :archives nil)))
        (play-from-hand state :corp "Midori" "Archives")
        (is (= {:base 2 :total 4} (core/num-cards-to-access state :runner :archives nil)))
        (core/access-bonus state :runner :total -1)
        (is (= {:base 2 :total 3} (core/num-cards-to-access state :runner :archives nil)))))))
