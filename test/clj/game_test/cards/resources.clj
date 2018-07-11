(ns game-test.cards.resources
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "resources"))

(deftest activist-support
  ;; Activist Support - Take tag if you have none; Corp gains bad pub if they have none
  (do-game
    (new-game (default-corp)
              (default-runner ["Activist Support"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Activist Support")
    (is (zero? (:tag (get-runner))))
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag; had none")
    (is (zero? (:bad-publicity (get-corp))))
    (take-credits state :corp)
    (is (= 1 (:bad-publicity (get-corp))) "Corp took 1 bad pub; had none")
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Runner had 1 tag; didn't take another")
    (take-credits state :corp)
    (is (= 1 (:bad-publicity (get-corp))) "Corp had 1 bad pub; didn't take another")))

(deftest adjusted-chronotype
  ;; Ensure adjusted chronotype gains only 1 click when 2 clicks are lost
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner ["Adjusted Chronotype" (qty "Beach Party" 2)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:click (get-runner))) "Should have lost 2 clicks and gained 1 click")))
  (testing "Chronotype to cancel out MCA click loss"
    (do-game
      (new-game
        (default-corp ["MCA Austerity Policy"])
        (default-runner ["Adjusted Chronotype"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (take-credits state :runner)
      (play-from-hand state :corp "MCA Austerity Policy" "New remote")
      (let [mca (get-content state :remote1 0)]
        (core/rez state :corp mca)
        (card-ability state :corp mca 0)
        (is (= 1 (get-counters (refresh mca) :power)))
        (take-credits state :corp)
        ; runner does not lose a click
        (is (= 4 (:click (get-runner)))))))
  (testing "Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed"
    (do-game
      (new-game (default-corp)
                (default-runner ["Adjusted Chronotype"
                                 (qty "Beach Party" 3)
                                 "Gene Conditioning Shoppe"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :runner "Beach Party")
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 2 clicks and gained 2 clicks")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:click (get-runner))) "Should have lost 3 clicks and gained 2 clicks"))))

(deftest aesop's-pawnshop
  ;; Tests use cases for Aesop's Pawnshop
  (do-game
    (new-game (default-corp)
              (default-runner ["Aesop's Pawnshop" "Cache"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Aesop's Pawnshop")
    (play-from-hand state :runner "Cache")
    (let [orig-credits (:credit (get-runner))
          ap (get-resource state 0)
          cache (get-program state 0)]
      (card-ability state :runner ap 0)
      (prompt-select :runner cache)
      (card-ability state :runner ap 0)
      (prompt-select :runner ap)
      (let [ap (get-resource state 0)
            cache (get-in @state [:runner :discard 0])]
        (is (= (+ 3 orig-credits) (:credit (get-runner))) "Should have only gained 3 credits")
        (is (not= cache nil) "Cache should be in Heap")
        (is (not= ap nil) "Aesops should still be installed")))))

(deftest all-nighter
  ;; All-nighter - Click/trash to gain 2 clicks
  (do-game
    (new-game (default-corp)
              (default-runner ["All-nighter"]))
    (take-credits state :corp)
    (play-from-hand state :runner "All-nighter")
    (is (= 3 (:click (get-runner))))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 4 (:click (get-runner))) "Spent 1 click; gained 2 clicks")
    (is (= 1 (count (:discard (get-runner)))) "All-nighter is trashed")))

(deftest bank-job
  ;; Bank Job
  (testing "Manhunt trace happens first"
    (do-game
      (new-game (default-corp ["Manhunt" "PAD Campaign"])
                (default-runner ["Bank Job"]))
      (play-from-hand state :corp "Manhunt")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (prompt-choice :corp 2) ; Manhunt trace active
      (prompt-choice :runner 0)
      (prompt-choice :runner "Replacement effect")
      (is (= "Bank Job" (:title (:card (first (get-in @state [:runner :prompt])))))
          "Bank Job prompt active")
      (prompt-choice :runner 8)
      (is (empty? (get-resource state)) "Bank Job trashed after all credits taken")
      (is (= 1 (count (:discard (get-runner)))))))
  (testing "Choose which to use when 2+ copies are installed"
    (do-game
      (new-game (default-corp ["PAD Campaign"])
                (default-runner [(qty "Bank Job" 2)]))
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (prompt-choice :runner "Replacement effect")
      (prompt-choice :runner 4)
      (play-from-hand state :runner "Bank Job")
      (let [bj1 (get-resource state 0)
            bj2 (get-resource state 1)]
        (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Replacement effect")
        (prompt-select :runner bj2)
        (prompt-choice :runner 6)
        (is (= 13 (:credit (get-runner))))
        (is (= 2 (get-counters (refresh bj2) :credit)) "2 credits remaining on 2nd copy"))))
  (testing "Security Testing takes priority"
    (do-game
      (new-game (default-corp ["PAD Campaign"])
                (default-runner ["Bank Job" "Security Testing"]))
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Bank Job")
      (take-credits state :runner)
      (take-credits state :corp)
      (prompt-choice :runner "Server 1")
      (is (= 6 (:credit (get-runner))))
      (run-empty-server state "Server 1")
      (is (empty? (:prompt (get-runner))) "No Bank Job replacement choice")
      (is (= 8 (:credit (get-runner))) "Security Testing paid 2c"))))

(deftest bazaar
  ;; Bazaar - Only triggers when installing from Grip
  (do-game
    (new-game (default-corp)
              (default-runner ["Street Peddler"
                               "Bazaar"
                               (qty "Spy Camera" 6)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler" "Bazaar" "Spy Camera" "Spy Camera" "Spy Camera"])
    (play-from-hand state :runner "Bazaar")
    (play-from-hand state :runner "Street Peddler")
    (let [peddler (get-resource state 1)]
      (card-ability state :runner peddler 0)
      (prompt-card :runner (first (:hosted peddler)))
      (is (empty? (:prompt (get-runner))) "No Bazaar prompt from install off Peddler"))))

(deftest beach-party
  ;; Beach Party - Lose 1 click when turn begins; hand size increased by 5
  (do-game
    (new-game (default-corp)
              (default-runner ["Beach Party"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Beach Party")
    (is (= 10 (core/hand-size state :runner)) "Max hand size increased by 5")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))) "Lost 1 click at turn start")))

(deftest bhagat
  ;; Bhagat - only trigger on first run
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3) (qty "Architect" 3)])
              (default-runner ["Bhagat"]))
    (starting-hand state :corp [])
    (take-credits state :corp)
    (run-empty-server state :hq)
    (play-from-hand state :runner "Bhagat")
    (run-empty-server state :hq)
    (is (empty? (:discard (get-corp))) "Bhagat did not trigger on second successful run")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-corp)))) "Bhagat milled one card")))

(deftest chrome-parlor
  ;; Chrome Parlor - Prevent all meat/brain dmg when installing cybernetics
  (do-game
    (new-game (default-corp ["Traffic Accident"])
              (default-runner ["Chrome Parlor" "Titanium Ribs"
                               "Brain Cage" (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Chrome Parlor")
    (play-from-hand state :runner "Titanium Ribs")
    (is (empty? (:prompt (get-runner))) "Damage prevented, no Ribs prompt to choose cards")
    (is (= 3 (count (:hand (get-runner)))))
    (play-from-hand state :runner "Brain Cage")
    (is (= 2 (count (:hand (get-runner)))) "No cards lost")
    (is (zero? (:brain-damage (get-runner))))
    (is (= 8 (core/hand-size state :runner)) "Runner hand size boosted by Brain Cage")
    (take-credits state :runner)
    (core/gain state :runner :tag 2)
    (core/trash state :runner (get-hardware state 0))
    (play-from-hand state :corp "Traffic Accident")
    (is (= 3 (count (:discard (get-runner)))) "Conventional meat damage not prevented by Parlor")))

(deftest compromised-employee
  ;; Compromised Employee - Gain 1c every time Corp rezzes ICE
  (do-game
    (new-game (default-corp [(qty "Pup" 2) "Launch Campaign"])
              (default-runner ["Compromised Employee"]))
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Pup" "R&D")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Compromised Employee")
    (let [ce (get-resource state 0)]
      (is (= 1 (get-counters (refresh ce) :recurring)) "Has 1 recurring credit")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 4 (:credit (get-runner))) "Gained 1c from ICE rez")
      (core/rez state :corp (get-ice state :rd 0))
      (is (= 5 (:credit (get-runner))) "Gained 1c from ICE rez")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 5 (:credit (get-runner))) "Asset rezzed, no credit gained"))))

(deftest councilman
  ;; Councilman reverses the rezz and prevents re-rezz
  (do-game
    (new-game (default-corp ["Jackson Howard"])
              (default-runner ["Councilman"]))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Councilman")
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
      (core/rez state :corp jesus)
      ;; Runner triggers Councilman
      (card-ability state :runner judas 0)
      (prompt-select :runner jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/rez state :corp (refresh jesus))
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard cannot be rezzed")
      (take-credits state :runner)
      ;; Next turn
      (core/rez state :corp (refresh jesus))
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed next turn"))))

(deftest-pending councilman-zone-change
  ;; Rezz no longer prevented when card changes zone (issues #1571)
  (do-game
    (new-game (default-corp ["Jackson Howard"])
              (default-runner ["Councilman"]))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Councilman")
    (take-credits state :runner)
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
      (core/rez state :corp jesus)
      ;; Runner triggers Councilman
      (card-ability state :runner judas 0)
      (prompt-select :runner jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/move state :corp (refresh jesus) :hand))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (let [jesus (get-content state :remote2 0)]
      (core/rez state :corp jesus)
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed after changing zone"))))

(deftest counter-surveillance
  ;; Counter-Surveillance
  (testing "Trash to run, on successful run access cards equal to Tags and pay that amount in credits"
    (do-game
      (new-game (default-corp [(qty "Hedge Fund" 3)])
                (default-runner ["Counter Surveillance"]))
      (take-credits state :corp)
      (core/gain state :runner :tag 2)
      (play-from-hand state :runner "Counter Surveillance")
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (let [cs (get-resource state 0)]
        (card-ability state :runner cs 0)
        (prompt-choice :runner "HQ")
        (run-successful state)
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (prompt-choice :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (prompt-choice :runner "No action")
        (prompt-choice :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (prompt-choice :runner "No action")
        (is (= 1 (count (:discard (get-runner)))) "Counter Surveillance trashed")
        (is (= 2 (:credit (get-runner))) "Runner has 2 credits"))))
  (testing "Test Obelus does not trigger before Counter Surveillance accesses are done. Issues #2675"
    (do-game
      (new-game (default-corp [(qty "Hedge Fund" 3)])
                (default-runner ["Counter Surveillance" "Obelus" (qty "Sure Gamble" 3)]))
      (starting-hand state :runner ["Counter Surveillance" "Obelus"])
      (take-credits state :corp)
      (core/gain state :runner :tag 2)
      (core/gain state :runner :credit 2)
      (is (= 7 (:credit (get-runner))) "Runner has 7 credits")
      (play-from-hand state :runner "Counter Surveillance")
      (play-from-hand state :runner "Obelus")
      (is (= 2 (:credit (get-runner))) "Runner has 2 credits") ; Runner has enough credits to pay for CS
      (let [cs (get-resource state 0)]
        (card-ability state :runner cs 0)
        (prompt-choice :runner "HQ")
        (run-successful state)
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (zero? (count (:hand (get-runner)))) "Runner did not draw cards from Obelus yet")
        (prompt-choice :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (is (zero? (count (:hand (get-runner)))) "Runner did not draw cards from Obelus yet")
        (prompt-choice :runner "No action")
        (prompt-choice :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (prompt-choice :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Runner did draw cards from Obelus after all accesses are done")
        (is (= 1 (count (:discard (get-runner)))) "Counter Surveillance trashed")
        (is (zero? (:credit (get-runner))) "Runner has no credits")))))

(deftest daily-casts
  ;; Play and tick through all turns of daily casts
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Daily Casts" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (let [dc (get-resource state 0)]
      ;; Number of credits
      (is (= 8 (get-counters dc :credit)))
      (is (= 2 (get-in @state [:runner :credit])))
      ;; End turn
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 6 (get-counters (refresh dc) :credit)))
      (is (= 7 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (get-counters (refresh dc) :credit)))
      (is (= 13 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh dc) :credit)))
      (is (= 19 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (nil? (get-resource state 0))))))

(deftest data-folding
  ;; Data Folding - Gain 1c at start of turn if 2+ unused MU
  (do-game
    (new-game (default-corp)
              (default-runner ["Data Folding" "Hyperdriver"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Data Folding")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (core/available-mu state)) "At least 2 unused MU")
    (is (= 6 (:credit (get-runner))) "Gained 1c at turn start")
    (play-from-hand state :runner "Hyperdriver")
    (take-credits state :runner)
    (is (= 1 (core/available-mu state)) "Only 1 unused MU")
    (is (= 8 (:credit (get-runner))))
    (take-credits state :corp)
    (is (= 8 (:credit (get-runner))) "No credits gained at turn start")))

(deftest ddos
  ;; Prevent rezzing of outermost ice for the rest of the turn
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3)])
              (default-runner ["DDoS"]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "DDoS")
    (let [ddos (get-resource state 0)
          iwall (get-ice state :hq 1)]
      (card-ability state :runner ddos 0)
      (is (= (:title ddos) (get-in @state [:runner :discard 0 :title])))
      (run-on state "HQ")
      (core/rez state :corp iwall)
      (is (not (:rezzed (refresh iwall))))
      (run-jack-out state)
      (run-on state "HQ")
      (core/rez state :corp iwall)
      (is (not (:rezzed (refresh iwall))))
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state "HQ")
      (core/rez state :corp iwall)
      (is (:rezzed (refresh iwall))))))

(deftest decoy
  ;; Decoy - Trash to avoid 1 tag
  (do-game
    (new-game (default-corp ["SEA Source"])
              (default-runner ["Decoy"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Decoy")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (count (:prompt (get-runner)))) "Runner prompted to avoid tag")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (count (:discard (get-runner)))) "Decoy trashed")
    (is (zero? (:tag (get-runner))) "Tag avoided")))

(let [choose-runner
      (fn [name state prompt-map]
                      (let [the-choice (some #(when (= name (:title %)) %) (:choices (prompt-map :runner)))]
                        (core/resolve-prompt state :runner {:card the-choice})))
      ;; Start id
      sunny "Sunny Lebeau: Security Specialist"
      ;; List of all G-Mod identities
      geist "Armand \"Geist\" Walker: Tech Lord"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"
      maxx "MaxX: Maximum Punk Rock"]

  (deftest dj-fenris
    ;; DJ Fenris - host 1 g-mod id not in faction on DJ Fenris
    (testing "Hosting Chaos Theory"
      ;; Ensure +1 MU is handled correctly
      (do-game
        (new-game (default-corp)
                  ;; Runner id is Gabe, make sure Geist is not in list (would be first)
                  (make-deck sunny ["DJ Fenris"]) {:start-as :runner})
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (choose-runner chaos state prompt-map)
        (is (= chaos (get-in (get-resource state 0) [:hosted 0 :title])) "Chaos Theory hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Sunny, id not changed")
        (is (= 2 (:link (get-runner))) "2 link from Sunny")
        (is (= 5 (core/available-mu state)) "+1 MU from Chaos Theory")
        ;; Trash DJ Fenris
        (trash-resource state "DJ Fenris")
        (is (= chaos (get-in (get-runner) [:rfg 0 :title])) "Chaos Theory moved to RFG")
        (is (= 1 (count (:discard (get-runner)))) "1 card in heap: DJ Fenris")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")
        ;; Recover DJ Fenris
        (core/move state :runner (get-in (get-runner) [:discard 0]) :hand)
        (core/gain state :runner :credit 3)
        ;; Re-play DJ Fenris
        (play-from-hand state :runner "DJ Fenris")
        (choose-runner chaos state prompt-map)
        ;; Try moving CT to hand
        (game.core/move state :runner (get-in (get-resource state 0) [:hosted 0]) :hand)
        (is (= chaos (get-in (get-runner) [:rfg 0 :title])) "Chaos Theory moved to RFG")
        (is (zero? (count (:hand (get-runner)))) "Chaos Theory _not_ moved to hand")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")))
    (testing "Hosting Geist"
      ;; Ensure Geist effect triggers
      (do-game
        (new-game (default-corp)
                  ;; Runner id is Gabe, make sure Geist is not in list (would be first)
                  (make-deck sunny ["DJ Fenris" (qty "All-nighter" 3) (qty "Sure Gamble" 3)]) {:start-as :runner})
        (starting-hand state :runner ["DJ Fenris" "All-nighter" "All-nighter"])
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (choose-runner geist state prompt-map)
        (is (= geist (get-in (get-resource state 2) [:hosted 0 :title])) "Geist hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Sunny, id not changed")
        (is (= 2 (:link (get-runner))) "2 link from Sunny, no extra link from Geist")
        (let [hand-count (count (:hand (get-runner)))]
          (card-ability state :runner (get-resource state 0) 0) ; Use All-nighter
          (is (= (+ 1 hand-count) (count (:hand (get-runner))))
              "Drew one card with Geist when using All-nighter trash ability")
          (trash-resource state "DJ Fenris")
          (is (= geist (get-in (get-runner) [:rfg 0 :title])) "Geist moved to RFG")
          (is (= 2 (count (:discard (get-runner)))) "2 cards in heap: All-nighter and DJ Fenris")
          (card-ability state :runner (get-resource state 0) 0) ; Use All-nighter (again)
          (is (= (+ 1 hand-count) (count (:hand (get-runner))))
              "Did not draw another card - Geist ability removed when DJ Fenris was trashed"))))))

(deftest donut-taganes
  ;; Donut Taganes - add 1 to play cost of Operations & Events when this is in play
  (do-game
    (new-game (default-corp)
              (default-runner ["Donut Taganes" "Easy Mark"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Donut Taganes")
    (is (= 2 (:credit (get-runner))) "Donut played for 3c")
    (play-from-hand state :runner "Easy Mark")
    (is (= 4 (:credit (get-runner))) "Easy Mark only gained 2c")
    (take-credits state :runner)
    (is (= 8 (:credit (get-corp))) "Corp has 8c")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 11 (:credit (get-corp))) "Corp has 11c")))

(deftest dummy-box
  ;; Dummy Box - trash a card from hand to prevent corp trashing installed card
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Dummy Box")
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (core/trash state :runner (get-program state 0))
      (is (not-empty (:prompt (get-runner))) "Dummy Box prompting to prevent program trash")
      (card-ability state :runner (get-resource state 0) 2)
      (prompt-select :runner (find-card "Clot" (:hand (get-runner))))
      (prompt-choice :runner "Done")
      (is (= 1 (count (:discard (get-runner)))) "Clot trashed")
      (is (empty? (:hand (get-runner))) "Card trashed from hand")
      (is (= 1 (count (get-program state))) "Cache still installed")
      (is (= 1 (count (get-resource state))) "Dummy Box still installed")))
  (testing "doesn't prevent program deletion during purge"
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Dummy Box")
      (play-from-hand state :runner "Clot")
      (take-credits state :runner)
      (core/purge state :corp)
      (is (empty? (:prompt (get-runner))) "Dummy Box not prompting to prevent purge trash"))))

(deftest eden-shard
  ;; Eden Shard - Install from Grip in lieu of accessing R&D; trash to make Corp draw 2
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner ["Eden Shard"]))
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (run-on state :rd)
      (core/no-action state :corp nil)
      (play-from-hand state :runner "Eden Shard")
      (is (= 5 (:credit (get-runner))) "Eden Shard installed for 0c")
      (is (not (:run @state)) "Run is over")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 3 (count (:hand (get-corp)))) "Corp drew 2 cards")
      (is (= 1 (count (:discard (get-runner)))) "Eden Shard trashed")))
  (testing "Do not install when accessing cards"
    (do-game
      (new-game (default-corp)
                (default-runner ["Eden Shard"]))
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (run-empty-server state :rd)
      (play-from-hand state :runner "Eden Shard")
      (is (not (get-resource state 0)) "Eden Shard not installed")
      (is (= 1 (count (:hand (get-runner)))) "Eden Shard not installed"))))

(deftest fan-site
  ;; Fan Site - Add to score area as 0 points when Corp scores an agenda
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Hostile Takeover"])
                (default-runner ["Fan Site"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-runner))))
      (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")))
  (testing "Don't trigger after swap with Exchange of Information. Issue #1824"
    (do-game
      (new-game (default-corp [(qty "Hostile Takeover" 2) "Exchange of Information"])
                (default-runner ["Fan Site"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (core/gain-tags state :runner 1)
      (play-from-hand state :corp "Exchange of Information")
      (prompt-select :corp (find-card "Fan Site" (:scored (get-runner))))
      (prompt-select :corp (find-card "Hostile Takeover" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-runner))))
      (is (zero? (:agenda-point (get-corp))))
      (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site swapped into Corp score area")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site not removed from Corp score area")))
  (testing "Runner can forfeit Fan Site"
    (do-game
      (new-game (default-corp ["Hostile Takeover"])
                (default-runner ["Fan Site" "Data Dealer"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-runner))))
      (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")
      (take-credits state :corp)
      (play-from-hand state :runner "Data Dealer")
      (let [credits (:credit (get-runner))]
        (card-ability state :runner (get-resource state 0) 0)
        (prompt-select :runner (get-scored state :runner 0))
        (is (zero? (count (:scored (get-runner)))) "Fan Site successfully forfeit to Data Dealer")
        (is (= (+ credits 9) (:credit (get-runner))) "Gained 9 credits from Data Dealer")))))

(deftest fester
  ;; Fester - Corp loses 2c (if able) when purging viruses
  (do-game
    (new-game (default-corp)
              (default-runner ["Fester"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Fester")
    (take-credits state :runner)
    (core/lose state :corp :credit 5)
    (core/gain state :corp :click 3)
    (is (= 3 (:credit (get-corp))))
    (core/purge state :corp)
    (is (= 1 (:credit (get-corp))) "Lost 2c when purging")
    (core/purge state :corp)
    (is (= 1 (:credit (get-corp))) "Lost no credits when purging, only had 1c")))

(deftest film-critic
  ;; Film Critic
  (testing "Prevent Corp-trashed execs going to Runner scored. Issues #1181/#1042"
    (do-game
      (new-game (default-corp [(qty "Director Haas" 3) (qty "Project Vitruvius" 3) "Hedge Fund"])
                (default-runner ["Film Critic"]))
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (trash-from-hand state :corp "Director Haas")
        (is (= 1 (count (:discard (get-corp)))) "Director Haas stayed in Archives")
        (is (zero? (:agenda-point (get-runner))) "No points gained by Runner")
        (is (empty? (:scored (get-runner))) "Nothing in Runner scored"))))
  (testing "Fetal AI interaction"
    (do-game
      (new-game (default-corp [(qty "Fetal AI" 3)])
                (default-runner ["Film Critic" (qty "Sure Gamble" 3)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state "HQ")
        ;; should not have taken damage yet
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt yet")
        (prompt-choice :runner "Yes")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (card-ability state :runner fc 0)
        (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner scored")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))
  (testing "Do not take a net damage when a hosted agenda is trashed due to film critic trash #2382"
    (do-game
      (new-game (default-corp [(qty "Hostile Infrastructure" 3) "Project Vitruvius"])
                (default-runner ["Film Critic" (qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state :remote2)
        (prompt-choice :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (core/gain state :corp :credit 10)
        (core/trash-resource state :corp nil)
        (prompt-select :corp fc)
        (is (= 1 (count (:discard (get-runner)))) "FC trashed")
        (is (= 1 (count (:discard (get-corp)))) "Agenda trashed")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))
  (testing "required hosted cards to be an agenda before firing ability"
    (do-game
      (new-game (default-corp ["MCA Informant"])
                (default-runner ["Film Critic"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (take-credits state :runner)
        (play-from-hand state :corp "MCA Informant")
        (prompt-select :corp fc)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant hosted on FC")
        (take-credits state :corp)
        (card-ability state :runner fc 0)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant still hosted on FC")))))

(deftest find-the-truth
  ;; Find the Truth
  (testing "Basic test - On successful run see the top card from R&D before access"
    (do-game
      (new-game
        (default-corp [(qty "Restructure" 10)])
        (default-runner ["Find the Truth"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Find the Truth")
      (run-on state "HQ")
      (run-successful state)
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :runner :prompt first :msg)) "FTT prompt")
      (prompt-choice :runner "Yes")
      (is (= "The top card of R&D is Restructure" (-> @state :runner :prompt first :msg)) "FTT shows card on R&D")
      (prompt-choice :runner "Yes")))
  (testing "Equivocation & FTT - should get order of choice"
    (do-game
      (new-game
        (default-corp [(qty "Restructure" 10)])
        (default-runner ["Equivocation" "Find the Truth"]))
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Equivocation")
      (play-from-hand state :runner "Find the Truth")
      (run-empty-server state :rd)
      (prompt-choice :runner "Find the Truth")
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :runner :prompt first :msg)) "FTT prompt")
      (prompt-choice :runner "Yes")
      (is (= "The top card of R&D is Restructure" (-> @state :runner :prompt first :msg)) "FTT shows card")
      (prompt-choice :runner "Yes") ; Equivocation prompt
      (is (= "Reveal the top card of R&D?" (-> @state :runner :prompt first :msg)) "Equivocation Prompt")
      (prompt-choice :runner "Yes")))
  (testing "Find The Truth should completed before Marilyn trash is forced"
    (do-game
      (new-game
        (default-corp ["Marilyn Campaign" (qty "Vanilla" 10)])
        (default-runner ["Find the Truth" "Neutralize All Threats"]))
      (starting-hand state :corp ["Marilyn Campaign"])
      (play-from-hand state :corp "Marilyn Campaign" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 3 (:credit (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Find the Truth")
      (play-from-hand state :runner "Neutralize All Threats")
      (run-on state :remote1)
      (run-successful state)
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :runner :prompt first :msg)) "FTT prompt")
      (is (= "Waiting for Runner to resolve successful-run triggers" (-> @state :corp :prompt first :msg)) "No Marilyn Shuffle Prompt")
      (prompt-choice :runner "Yes")
      (is (= "The top card of R&D is Vanilla" (-> @state :runner :prompt first :msg)) "FTT shows card")
      (is (= "Waiting for Runner to resolve successful-run triggers" (-> @state :corp :prompt first :msg)) "No Marilyn Shuffle Prompt")
      (prompt-choice :runner "No action")
      (prompt-choice-partial :runner "Pay")
      (is (= "Waiting for Corp to use Marilyn Campaign" (-> @state :runner :prompt first :msg)) "Now Corp gets shuffle choice")
      (is (= "Shuffle Marilyn Campaign into R&D?" (-> @state :corp :prompt first :msg)) "Now Corp gets shuffle choice")
      (is (= 2 (:credit (get-runner)))) #_ trashed_marilyn)))

(deftest gang-sign
  ;; Gang Sign
  (testing "accessing from HQ, not including root. Issue #2113"
    (do-game
      (new-game (default-corp [(qty "Hostile Takeover" 3) (qty "Braintrust" 2) "Crisium Grid"])
                (default-runner [(qty "Gang Sign" 2) "HQ Interface"]))
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "HQ Interface")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (prompt-choice :runner "Gang Sign") ; simultaneous effect resolution
      (let [gs1 (-> (get-runner) :prompt first)]
        (is (= (:choices gs1) ["Card from hand"]) "Gang Sign does not let Runner access upgrade in HQ root")
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "Steal")
        (is (= (:card gs1) (-> (get-runner) :prompt first :card)) "Second access from first Gang Sign triggered")
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "Steal")
        (is (not= (:card gs1) (-> (get-runner) :prompt first :card)) "First access from second Gang Sign triggered")
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "Steal")
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "Steal"))))
  (testing "accessing from HQ, not including root. Issue #2113"
    (do-game
      (new-game (default-corp ["Hostile Takeover" "Snare!"])
                (default-runner ["Gang Sign"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (prompt-choice :runner "Card from hand")
      ;; Runner has "wait for Snare, wait for on-access" prompts.
      (is (= 2 (count (:prompt (get-runner)))) "Runner only has the Waiting prompt, not Snare!'s pay-prompt")
      ;; Core has "pay for Snare, wait for agenda-scored" prompts.
      (is (= 2 (count (:prompt (get-corp)))) "Corp has the prompt to use Snare!"))))

(deftest gene-conditioning-shoppe
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twice flag
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Hedge Fund" 3)])
                (default-runner ["Gene Conditioning Shoppe"
                                 "Adjusted Chronotype"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
      (core/trash state :runner (get-resource state 1))
      (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))))
  (testing "set :genetics-trigger-twice flag - ensure redundant copies work"
    (do-game
      (new-game (default-corp [(qty "Hedge Fund" 3)])
                (default-runner [(qty "Gene Conditioning Shoppe" 2)
                                 "Adjusted Chronotype"]))
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (let [adjusted-chronotype (get-resource state 0)]
        (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))
        (play-from-hand state :runner "Gene Conditioning Shoppe")
        (play-from-hand state :runner "Gene Conditioning Shoppe")
        (let [gcs1 (get-resource state 1)
              gcs2 (get-resource state 2)]
          (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
          (core/trash state :runner gcs1)
          (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
          (core/trash state :runner gcs2)
          (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice))))))))

(deftest globalsec-security-clearance
  ;; Globalsec Security Clearance - Ability, click lost on use
  (do-game
    (new-game (default-corp)
              (default-runner ["Globalsec Security Clearance"]))
    (take-credits state :corp)
    (core/gain state :runner :link 2)
    (play-from-hand state :runner "Globalsec Security Clearance")
    (take-credits state :runner)
    (starting-hand state :corp ["Hedge Fund"]) ; Hedge Fund on top
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [gsec (get-resource state 0)]
      (card-ability state :runner gsec 0)
      (is (pos? (.indexOf (-> (get-runner) :prompt first :msg) "Hedge Fund")) "GSec revealed Hedge Fund")
      (core/end-phase-12 state :runner nil)
      (is (= 3 (:click (get-runner))) "Runner lost 1 click from Globalsec Security Clearance"))))

(deftest grifter
  ;; Grifter - Gain 1c if you made a successful run this turn, otherwise trash it
  (do-game
    (new-game (default-corp)
              (default-runner ["Grifter"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Grifter")
    (run-empty-server state :hq)
    (take-credits state :runner)
    (is (= 6 (:credit (get-runner))) "Gained 1c for a successful run during the turn")
    (take-credits state :corp)
    (run-on state :hq)
    (run-jack-out state)
    (take-credits state :runner)
    (is (= 1 (count (:discard (get-runner)))) "No successful runs; Grifter is trashed")))

(deftest guru-davinder
  ;; Guru Davinder - no prompt/trash for 'preventing' 0 damage
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Punitive Counterstrike"])
                (default-runner ["Guru Davinder"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Guru Davinder")
      (take-credits state :runner)
      (play-from-hand state :corp "Punitive Counterstrike")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (empty? (get-in @state [:runner :prompt]))
          "There is no prompt for 0 damage")))
  (testing "cannot steal Obokata while installed"
    (do-game
      (new-game (make-deck "Jinteki: Personal Evolution" [(qty "Obokata Protocol" 10)])
                (default-runner ["Guru Davinder" (qty "Sure Gamble" 4)]))
      (play-from-hand state :corp "Obokata Protocol" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :agenda-point 6)
      (play-from-hand state :runner "Guru Davinder")
      (run-empty-server state "Server 1")
      (prompt-choice :runner "No action")
      (is (zero? (count (:discard (get-runner)))) "Runner did not pay damage")
      (is (not= :runner (:winner @state)) "Runner has not won"))))

(deftest hard-at-work
  ;; Hard at Work - Gain 2c and lose 1 click when turn begins
  (do-game
    (new-game (default-corp)
              (default-runner ["Hard at Work"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Hard at Work")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Gained 2c")
    (is (= 3 (:click (get-runner))) "Lost 1 click")))

(deftest ice-carver
  ;; Ice Carver - lower ice strength on encounter
  (do-game
    (new-game (default-corp ["Ice Wall"])
              (default-runner ["Ice Carver"]))
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp 2)
    (let [iwall (get-ice state :archives 0)]
      (core/rez state :corp iwall)
      (play-from-hand state :runner "Ice Carver")
      (run-on state "Archives")
      (is (zero? (:current-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest investigative-journalism
  ;; Investigative Journalism - 4 clicks and trash to give the Corp 1 bad pub
  (do-game
    (new-game (default-corp)
              (default-runner ["Investigative Journalism"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Investigative Journalism")
    (is (empty? (get-resource state)) "Corp has no bad pub, couldn't install")
    (core/gain state :corp :bad-publicity 1)
    (play-from-hand state :runner "Investigative Journalism")
    (take-credits state :runner)
    (take-credits state :corp)
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (:click (get-runner))) "Spent 4 clicks")
    (is (= 1 (count (:discard (get-runner)))) "IJ is trashed")
    (is (= 2 (:bad-publicity (get-corp))) "Corp took 1 bad publicity")))

(deftest jackpot!
  ;; Jackpot! - whenever a card enters your score area, trash Jackpot to pull off credits
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Braintrust"])
                (default-runner ["Jackpot!"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Jackpot!")
      (let [jak (get-resource state 0)]
        (is (zero? (get-counters (refresh jak) :credit)) "Jackpot! starts with 0 credits")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn (2nd turn)")
        (run-empty-server state "HQ")
        (prompt-choice :runner "Steal")
        (is (= 2 (:agenda-point (get-runner))) "Runner steals Braintrust")
        (prompt-choice :runner "Yes")
        (is (= 12 (:credit (get-runner))) "Runner starts with 12 credits")
        (prompt-choice :runner 2)
        (is (= 14 (:credit (get-runner))) "Runner gains 2 credits")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed"))))
  (testing "should fire when moving agendas from Film Critic to scored area"
    (do-game
      (new-game (default-corp ["Project Vitruvius"])
                (default-runner ["Jackpot!" "Film Critic"]))
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (play-from-hand state :runner "Jackpot!")
      (let [fc (get-resource state 0)
            jak (get-resource state 1)]
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (take-credits state :corp)
        (card-ability state :runner fc 0)
        (prompt-choice :runner "Yes")
        (prompt-choice :runner 1)
        (is (= 1 (count (:scored (get-runner)))) "Moved agenda to scored area")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed")
        (is (empty? (:hosted (refresh fc))) "Removed agenda hosted on FC"))))
  (testing "should fire when trashing Chairman Hiro"
    (do-game
      (new-game (default-corp ["Chairman Hiro"])
                (default-runner ["Jackpot!"]))
      (play-from-hand state :corp "Chairman Hiro" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Jackpot!")
      (let [jak (get-resource state 0)]
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (run-empty-server state "Server 1")
        (prompt-choice-partial :runner "Pay") ;trash CH
        (prompt-choice :runner "Yes") ;trash Jackpot!
        (prompt-choice :runner 1)
        (is (= 3 (:credit (get-runner))) "Runner gains 1 credit")
        (is (= 1 (count (:scored (get-runner)))) "Chairman Hiro in score area")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed")))))

(deftest jak-sinclair
  ;; Jak Sinclair
  (testing "Lost clicks carry through to when turn starts fully #1764"
    (do-game
      (new-game (default-corp [(qty "Enigma" 3)])
                (default-runner [(qty "Jak Sinclair" 3)]))
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Jak Sinclair")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [eni (get-ice state :hq 0)
            jak (get-resource state 0)]
        (core/rez state :corp eni)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (card-ability state :runner jak 0)
        (prompt-choice :runner "HQ")
        (card-subroutine state :corp (refresh eni) 0)
        (run-successful state)
        (core/end-phase-12 state :runner nil)
        (is (= 3 (:click (get-runner))) "Enigma took a click")))))

(deftest john-masanori
  ;; John Masanori - Draw 1 card on first successful run, take 1 tag on first unsuccessful run
  (do-game
    (new-game (default-corp ["Crisium Grid"])
              (default-runner [(qty "John Masanori" 3)
                               (qty "Sure Gamble" 3)
                               "Fall Guy"]))
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (core/rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (core/gain state :runner :click 2 :credit 2)
    (play-from-hand state :runner "John Masanori")
    (is (= 4 (count (:hand (get-runner)))))
    (run-empty-server state "HQ")
    (prompt-choice-partial :runner "Pay") ; trash crisium #2433
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-runner)))) "1 card drawn from first successful run")
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-runner)))) "No card drawn from second successful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-runner))) "1 tag taken from first unsuccessful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-runner))) "No tag taken from second unsuccessful run")))

(deftest joshua-b.
  ;; Joshua B. - Take 1 tag at turn end if you choose to gain the extra click
  (do-game
    (new-game (default-corp)
              (default-runner ["Joshua B."]))
    (take-credits state :corp)
    (play-from-hand state :runner "Joshua B.")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (zero? (:click (get-runner))) "Runner has 0 clicks")
    (is (:runner-phase-12 @state) "Runner is in Step 1.2")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (:click (get-runner))) "Gained extra click from Joshua")
    (core/end-phase-12 state :runner nil)
    (is (= 5 (:click (get-runner))) "Gained normal clicks as well")
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Took 1 tag")))

(deftest kati-jones
  ;; Kati Jones - Click to store and take
  (do-game
    (new-game (default-corp)
              (default-runner ["Kati Jones"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (is (= 3 (:credit (get-runner))))
    (let [kati (get-resource state 0)]
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))))
      (is (= 3 (get-counters (refresh kati) :credit)) "Store 3cr on Kati")
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))) "Second use of Kati should not be allowed")
      (is (= 3 (get-counters (refresh kati) :credit)) "Second use of Kati should not be allowed")
      (take-credits state :runner 2)
      (is (= 5 (:credit (get-runner))) "Pass turn, take 2cr")
      (take-credits state :corp)
      (card-ability state :runner kati 0)
      (is (= 6 (get-counters (refresh kati) :credit)) "Store 3cr more on Kati")
      (take-credits state :runner 3)
      (is (= 8 (:credit (get-runner))) "Pass turn, take 3cr")
      (take-credits state :corp)
      (card-ability state :runner (refresh kati) 1)
      (is (= 14 (:credit (get-runner))) "Take 6cr from Kati")
      (is (zero? (get-counters (refresh kati) :credit)) "No counters left on Kati"))))

(deftest lewi-guilherme
  ;; Lewi Guilherme - lower corp hand size by 1, pay 1 credit when turn begins or trash
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Lewi Guilherme" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Lewi Guilherme")
    (is (= -1 (get-in (get-corp) [:hand-size :mod])) "Corp hand size reduced by 1")
    (take-credits state :runner)
    (core/lose state :runner :credit 6)
    (is (= 2 (:credit (get-runner))) "Credits are 2")
    (take-credits state :corp)
    (prompt-choice :runner "Yes")
    (is (= 1 (:credit (get-runner))) "Lost a credit from Lewi")
    (take-credits state :runner)
    (take-credits state :corp)
    (prompt-choice :runner "No")
    (is (= 1 (count (:discard (get-runner)))) "First Lewi trashed")
    (is (zero? (get-in (get-corp) [:hand-size :mod])) "Corp hand size normal again")
    (play-from-hand state :runner "Lewi Guilherme")
    (take-credits state :runner)
    (core/lose state :runner :credit 8)
    (is (zero? (:credit (get-runner))) "Credits are 0")
    (take-credits state :corp)
    (prompt-choice :runner "Yes")
    (is (= 2 (count (:discard (get-runner)))) "Second Lewi trashed due to no credits")))

(deftest logic-bomb
  ;; Logic Bomb
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Ice Wall" 2)])
                (default-runner ["Logic Bomb"]))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Logic Bomb")
      (run-on state :hq)
      (is (= 2 (:click (get-runner))) "Should still have 2 clicks")
      (card-ability state :runner (get-resource state 0) 0)
      (is (zero? (:click (get-runner))) "Should now have 0 clicks")
      (is (= 1 (count (:discard (get-runner)))) "Logic Bomb should be discarded")
      (is (last-log-contains? state "uses Logic Bomb"))
      (is (last-log-contains? state "\\[Click\\]\\[Click\\]") "Log should mention 2 clicks")))
  (testing "if the runner has no clicks left"
    (do-game
      (new-game (default-corp [(qty "Ice Wall" 2)])
                (default-runner ["Logic Bomb"]))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Logic Bomb")
      (core/click-credit state :runner nil)
      (core/click-credit state :runner nil)
      (run-on state :hq)
      (is (zero? (:click (get-runner))) "Should have 0 clicks")
      (card-ability state :runner (get-resource state 0) 0)
      (is (zero? (:click (get-runner))) "Should still have 0 clicks")
      (is (= 1 (count (:discard (get-runner)))) "Logic Bomb should be discarded")
      (is (last-log-contains? state "uses Logic Bomb"))
      (is (not (last-log-contains? state "\\[Click\\]")) "Log shouldn't mention any clicks"))))

(deftest london-library
  ;; Install non-virus programs on London library. Includes #325/409
  (do-game
    (new-game (default-corp)
              (default-runner ["London Library" "Darwin" "Study Guide"
                               "Chameleon" "Femme Fatale"]))
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "London Library")
    (let [lib (get-resource state 0)]
      (is (zero? (count (:hosted (refresh lib)))) "0 programs hosted")
      (card-ability state :runner lib 0) ; Install a non-virus program on London Library
      (prompt-select :runner (find-card "Femme Fatale" (:hand (get-runner))))
      (prompt-choice :runner "Done") ; Cancel out of Femme's bypass
      (is (= 1 (count (:hosted (refresh lib)))) "1 program hosted")
      (card-ability state :runner lib 0)
      (prompt-select :runner (find-card "Study Guide" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (let [sg (second (:hosted (refresh lib)))]
        (is (zero? (:current-strength (refresh sg))) "Study Guide at 0 strength")
        (card-ability state :runner sg 1) ; Place 1 power counter
        (is (= 1 (:current-strength (refresh sg))) "Study Guide at 1 strength"))
      (card-ability state :runner lib 0)
      (prompt-select :runner (find-card "Chameleon" (:hand (get-runner))))
      (prompt-choice :runner "Sentry")
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (= 2 (:click (get-runner))) "At 2 clicks")
      (card-ability state :runner lib 0)
      (prompt-select :runner (find-card "Darwin" (:hand (get-runner)))) ; Darwin is a virus
      (is (= 3 (count (:hosted (refresh lib)))) "Still 3 programs hosted")
      (is (= 2 (:click (get-runner))) "Failed Darwin didn't use a click")
      (is (= 1 (count (:hand (get-runner)))))
      (card-ability state :runner lib 1) ; Add a program hosted on London Library to your Grip
      (prompt-card :runner nil)
      (prompt-select :runner (find-card "Study Guide" (:hosted (refresh lib))))
      (is (= 2 (count (:hand (get-runner)))) "Return Study Guide to hand")
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (card-ability state :runner lib 0)
      (prompt-select :runner (find-card "Study Guide" (:hand (get-runner))))
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (zero? (count (:discard (get-runner)))) "Nothing in archives yet")
      (take-credits state :runner)
      (is (zero? (count (:hosted (refresh lib)))) "All programs trashed when turn ends")
      (is (= 2 (count (:hand (get-runner)))) "Darwin never got played, Chameleon returned to hand")
      (is (= 2 (count (:discard (get-runner)))) "Femme Fatale and Study Guide trashed"))))

(deftest muertos-gang-member
  ;; Muertos Gang Member - Install and Trash
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Tollbooth" "Ice Wall"])
                (default-runner [(qty "Hedge Fund" 3) "Muertos Gang Member"]))
      (play-from-hand state :corp "Tollbooth" "HQ")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (let [toll (get-ice state :hq 0)
            iw (get-ice state :archives 0)]
        (core/rez state :corp iw)
        (core/move state :runner (find-card "Hedge Fund" (:hand (get-runner))) :deck)
        (play-from-hand state :runner "Muertos Gang Member")
        (prompt-select :corp (refresh iw))
        (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
        (is (= 2 (count (:hand (get-runner)))) "2 cards in Runner's hand")
        (let [muer (get-resource state 0)]
          (card-ability state :runner muer 0)
          (is (= 3 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
          (prompt-select :corp toll)
          (is (:rezzed (refresh toll)) "Tollbooth was rezzed")))))
  (testing "Account for Reina interaction, #1098"
    (do-game
      (new-game (default-corp ["Tollbooth" "Ice Wall"])
                (make-deck "Reina Roja: Freedom Fighter" [(qty "Hedge Fund" 3)
                                                          "Muertos Gang Member"]))
      (play-from-hand state :corp "Tollbooth" "HQ")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (let [toll (get-ice state :hq 0)
            iw (get-ice state :archives 0)]
        (core/rez state :corp iw)
        (take-credits state :corp)
        (core/lose state :corp :credit 100)
        (core/move state :runner (find-card "Hedge Fund" (:hand (get-runner))) :deck)
        (play-from-hand state :runner "Muertos Gang Member")
        (prompt-select :corp (refresh iw))
        (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
        (is (= 2 (count (:hand (get-runner)))) "2 cards in Runner's hand")
        (let [muer (get-resource state 0)]
          (card-ability state :runner muer 0)
          (is (= 3 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
          (prompt-select :corp toll)
          (is (:rezzed (refresh toll)) "Tollbooth was rezzed")
          (is (zero? (:credit (get-corp))) "Corp has 0 credits"))))))

(deftest net-mercur
  ;; Net Mercur - Gains 1 credit or draw 1 card when a stealth credit is used
  (do-game
    (new-game (default-corp)
              (default-runner ["Net Mercur" "Silencer" "Ghost Runner"]))
    (take-credits state :corp)
    (core/gain state :runner :click 4 :credit 10)
    (play-from-hand state :runner "Silencer")
    (play-from-hand state :runner "Net Mercur")
    (play-from-hand state :runner "Ghost Runner")
    (let [sil (get-hardware state 0)
          nm (get-resource state 0)
          gr (get-resource state 1)]
      (card-ability state :runner gr 0)
      (is (empty? (:prompt (get-runner))) "No Net Mercur prompt from stealth spent outside of run")
      (run-on state :hq)
      (card-ability state :runner sil 0)
      (prompt-choice :runner "Place 1 [Credits]")
      (is (= 1 (get-counters (refresh nm) :credit)) "1 credit placed on Net Mercur")
      (card-ability state :runner gr 0)
      (is (empty? (:prompt (get-runner))) "No Net Mercur prompt for 2nd stealth in run")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :runner nm 0)
      (is (= "Net Mercur" (:title (:card (first (get-in @state [:runner :prompt]))))) "Net Mercur triggers itself"))))

(deftest network-exchange
  ;; ICE install costs 1 more except for inner most
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Paper Wall" 3)])
                (default-runner ["Network Exchange"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Network Exchange")
      (take-credits state :runner)
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 8 (:credit (get-corp))) "Paid 0 to install Paper Wall")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 6 (:credit (get-corp))) "Paid 1 extra  to install Paper Wall")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 3 (:credit (get-corp))) "Paid 1 extra  to install Paper Wall")))
  (testing "Architect 1st sub should ignore additional install cost"
    (do-game
      (new-game (default-corp [(qty "Architect" 3)])
                (default-runner ["Network Exchange"]))
      (play-from-hand state :corp "Architect" "HQ")
      (take-credits state :corp) ; corp has 7 credits
      (play-from-hand state :runner "Network Exchange")
      (take-credits state :runner)
      (let [architect (get-ice state :hq 0)]
        (core/rez state :corp architect)
        (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rez")
        (core/move state :corp (find-card "Architect" (:hand (get-corp))) :deck)
        (card-subroutine state :corp architect 0)
        (prompt-card :corp (find-card "Architect" (:deck (get-corp))))
        (prompt-choice :corp "HQ")
        (is (= 3 (:credit (get-corp))) "Corp has 7 credits")))))

(deftest neutralize-all-threats
  ;; Neutralize All Threats - Access 2 cards from HQ, force trash first accessed card with a trash cost
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 2) "Breaker Bay Grid" "Elizabeth Mills"])
              (default-runner ["Neutralize All Threats"]))
    (play-from-hand state :corp "Breaker Bay Grid" "New remote")
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Neutralize All Threats")
    (run-empty-server state "HQ")
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "No action") ; access first Hedge Fund
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "No action") ; access second Hedge Fund
    (run-empty-server state "Server 1")
    (prompt-choice-partial :runner "Pay")
    (is (= 3 (:credit (get-runner))) "Forced to pay 2c to trash BBG")
    (is (= 1 (count (:discard (get-corp)))) "Breaker Bay Grid trashed")
    (run-empty-server state "Server 2")
    (is (not (empty? (:prompt (get-runner)))) "Runner prompt to trash Elizabeth Mills")))

(deftest new-angeles-city-hall
  ;; New Angeles City Hall - Avoid tags; trash when agenda is stolen
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["SEA Source" "Breaking News"])
                (default-runner ["New Angeles City Hall"]))
      (play-from-hand state :corp "Breaking News" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "New Angeles City Hall")
      (let [nach (get-resource state 0)]
        (run-empty-server state "Archives")
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))))
        (play-from-hand state :corp "SEA Source")
        (prompt-choice :corp 0) ; default trace
        (prompt-choice :runner 0) ; Runner won't match
        (card-ability state :runner nach 0)
        (prompt-choice :runner "Done")
        (is (zero? (:tag (get-runner))) "Avoided SEA Source tag")
        (is (= 4 (:credit (get-runner))) "Paid 2 credits")
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Steal")
        (is (= 1 (:agenda-point (get-runner))))
        (is (empty? (get-resource state)) "NACH trashed by agenda steal"))))
  (testing "don't gain Siphon credits until opportunity to avoid tags has passed"
    (do-game
      (new-game (default-corp)
                (default-runner ["Account Siphon" "New Angeles City Hall"]))
      (take-credits state :corp)
      (play-from-hand state :runner "New Angeles City Hall")
      (play-run-event state (first (:hand (get-runner))) :hq)
      (prompt-choice :runner "Replacement effect")
      (let [nach (get-resource state 0)]
        (is (= 4 (:credit (get-runner))) "Have not gained Account Siphon credits until tag avoidance window closes")
        (card-ability state :runner nach 0)
        (card-ability state :runner nach 0)
        (prompt-choice :runner "Done")
        (is (zero? (:tag (get-runner))) "Tags avoided")
        (is (= 10 (:credit (get-runner))) "10 credits siphoned")
        (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))))

(deftest no-one-home
  ;; Prevent first tag or net damage of the turn if you beat trace0, then trash
  (do-game
    (new-game (default-corp ["Data Mine" "SEA Source" "Scorched Earth"])
              (default-runner [(qty "No One Home" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :corp "Data Mine" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "No One Home")
    (let [dm (get-ice state :remote1 0)
          noh (get-resource state 0)]
      (run-on state "Server 1")
      (core/rez state :corp dm)
      (card-subroutine state :corp dm 0)
      (card-ability state :runner noh 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (prompt-choice :runner "Done")
      (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
      (run-successful state)
      (play-from-hand state :runner "No One Home")
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 1 (count (:prompt (get-runner)))) "Runner prompted to avoid tag")
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (prompt-choice :runner "Done")
      (is (= 3 (count (:discard (get-runner)))) "Two NOH trashed, 1 gamble played")
      (is (zero? (:tag (get-runner))) "Tags avoided")
      (take-credits state :corp)
      (play-from-hand state :runner "No One Home")
      (take-credits state :runner)
      (core/gain state :runner :tag 1)
      (core/gain state :corp :credit 4)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:prompt (get-runner)))) "Runner not prompted to avoid meat damage"))))

(deftest off-campus-apartment
  ;; Off-Campus Apartment
  (testing "ability shows a simultaneous resolution prompt when appropriate"
    (do-game
      (new-game (default-corp)
                (default-runner ["Street Peddler" "Off-Campus Apartment"
                                 "Underworld Contact" (qty "Spy Camera" 6)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Off-Campus Apartment" "Underworld Contact"])
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)]
        (card-ability state :runner oca 0)
        (prompt-select :runner (find-card "Underworld Contact" (:hand (get-runner))))
        (is (= 2 (count (:hand (get-runner)))) "Drew a card from OCA")
        (card-ability state :runner oca 0)
        (prompt-select :runner (find-card "Street Peddler" (:hand (get-runner))))
        ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
        (is (= 2 (-> (get-runner) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
        (prompt-choice :runner "Off-Campus Apartment")
        (is (= 2 (count (:hand (get-runner)))) "Drew a card from OCA"))))
  (testing "second ability does not break cards that are hosting others, e.g., Street Peddler"
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Street Peddler" 2) "Off-Campus Apartment" (qty "Spy Camera" 6)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Street Peddler" "Off-Campus Apartment"])
      (core/move state :runner (find-card "Street Peddler" (:hand (get-runner))) :deck {:front true})
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)]
        (card-ability state :runner oca 0)
        (prompt-select :runner (find-card "Street Peddler" (:hand (get-runner))))
        (prompt-choice :runner "Street Peddler")
        (let [ped1 (first (:hosted (refresh oca)))]
          (card-ability state :runner ped1 0)
          (prompt-card :runner (-> (get-runner) :prompt first :choices second)) ; choose Street Peddler
          (card-ability state :runner (refresh oca) 1)
          (prompt-select :runner (get-resource state 1))
          (let [ped2 (first (:hosted (refresh oca)))]
            (card-ability state :runner ped2 0)
            (prompt-card :runner (-> (get-runner) :prompt first :choices first)) ; choose Spy Camera
            ;; the fact that we got this far means the bug is fixed
            (is (= 1 (count (get-hardware state))) "Spy Camera installed")))))))

(deftest officer-frank
  ;; Officer Frank - meat damage to trash 2 from HQ
  (do-game
      (new-game (default-corp ["Swordsman" (qty "Hedge Fund" 2)])
                (default-runner ["Officer Frank" "Skulljack" (qty "Respirocytes" 4)]))
   (play-from-hand state :corp "Swordsman" "Archives")
   (take-credits state :corp)
   (starting-hand state :runner ["Officer Frank" "Skulljack" "Respirocytes" "Respirocytes" "Respirocytes" "Respirocytes"])
   (play-from-hand state :runner "Officer Frank")
   (card-ability state :runner (get-resource state 0) 0)
   (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
   (play-from-hand state :runner "Skulljack")
   (is (= 3 (count (:hand (get-runner)))) "Took 1 brain damage")
   (card-ability state :runner (get-resource state 0) 0)
   (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
   (let [sm (get-ice state :archives 0)]
     (run-on state :archives)
     (core/rez state :corp sm)
     (card-subroutine state :corp sm 0)
     (run-jack-out state))
   (is (= 2 (count (:hand (get-runner)))) "Took 1 net damage")
   (card-ability state :runner (get-resource state 0) 0)
   (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
   (play-from-hand state :runner "Respirocytes")
   (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")
   (card-ability state :runner (get-resource state 0) 0)
   (is (= 2 (count (:discard (get-corp)))) "Two cards trashed from HQ")))

(deftest pad-tap
  ;; PAD Tap
  (do-game
    (new-game (default-corp ["Melange Mining Corp."])
              (default-runner ["PAD Tap"]))
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "PAD Tap")
    (let [mel (get-content state :remote1 0)
          tap (get-resource state 0)]
      (take-credits state :runner)
      (let [credits (:credit (get-runner))]
        (core/click-credit state :corp nil)
        (is (zero? (-> (get-runner) :prompt count)) "Runner should have no prompts from PAD Tap")
        (is (= credits (:credit (get-runner))) "Runner shouldn't gain PAD Tap credits from clicking for a credit"))
      (let [credits (:credit (get-runner))]
        (core/rez state :corp mel)
        (core/gain state :corp :click 10)
        (card-ability state :corp mel 0)
        (is (= (+ credits 1) (:credit (get-runner))) "Runner should gain 1 credit from PAD Tap triggering from Melange Mining Corp. ability")
        (card-ability state :corp mel 0) ;; Triggering Melange a second time
        (is (zero? (-> (get-runner) :prompt count)) "Runner should have no prompts from PAD Tap"))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (zero? (-> (get-runner) :discard count)) "Runner should have 0 cards in Heap")
      (let [credits (:credit (get-corp))
            clicks (:click (get-corp))]
        (card-side-ability state :corp tap 0)
        (is (= (- credits 3) (:credit (get-corp))) "PAD Tap ability should cost Corp 3 credits")
        (is (= (- clicks 1) (:click (get-corp))) "PAD Tap ability should cost Corp 1 click")))))

(deftest paige-piper
  ;; Paige Piper
  (testing "interaction with Frantic Coding. Issue #2190"
    (do-game
      (new-game (default-corp)
                (default-runner ["Paige Piper" (qty "Frantic Coding" 2) (qty "Sure Gamble" 3)
                                 (qty "Gordian Blade" 2) "Ninja" (qty "Bank Job" 3) (qty "Indexing" 2)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Paige Piper" "Frantic Coding" "Frantic Coding"])
      (play-from-hand state :runner "Paige Piper")
      (prompt-choice :runner "No")
      (take-credits state :runner) ; now 8 credits
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (prompt-choice :runner "No action")
      (prompt-card :runner (find-card "Gordian Blade" (:deck (get-runner))))
      (is (= 1 (count (get-program state))) "Installed Gordian Blade")
      (prompt-choice :runner "Yes")
      (prompt-choice :runner "0")
      (is (= 1 (count (:discard (get-runner)))) "Paige Piper intervention stopped Frantic Coding from trashing 9 cards")
      (is (= 5 (:credit (get-runner))) "No charge to install Gordian")
      ;; a second Frantic Coding will not trigger Paige (once per turn)
      (play-from-hand state :runner "Frantic Coding")
      (prompt-choice :runner "No action")
      (prompt-card :runner (find-card "Ninja" (:deck (get-runner))))
      (is (= 2 (count (get-program state))) "Installed Ninja")
      (is (= 11 (count (:discard (get-runner)))) "11 cards in heap")
      (is (= 2 (:credit (get-runner))) "No charge to install Ninja"))))

(deftest patron
  ;; Patron
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Jackson Howard"])
                (default-runner [(qty "Patron" 4) (qty "Easy Mark" 4)]))
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Patron")
      (let [p (get-resource state 0)]
        (take-credits state :runner 3)
        (take-credits state :corp)
        (prompt-choice :runner "Server 1")
        (is (= 4 (count (:hand (get-runner)))) "Starts with 4 cards")
        (run-empty-server state "Server 1")
        (is (= 6 (count (:hand (get-runner)))) "Drew 2 cards")
        (run-empty-server state "Server 1")
        (prompt-choice :runner "No")
        (is (= 6 (count (:hand (get-runner)))) "Drew no cards")
        (play-from-hand state :runner "Easy Mark")
        (take-credits state :runner)
        (take-credits state :corp)
        (prompt-choice :runner "Server 1")
        (run-empty-server state "Archives")
        (is (= 5 (count (:hand (get-runner)))) "Did not draw cards when running other server"))))
  (testing "Manually selecting during Step 1.2 does not show a second prompt at start of turn. Issue #1744."
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Patron" 3) (qty "Jak Sinclair" 3)]))
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
        (prompt-choice :runner "Archives")
        (card-ability state :runner j 0)
        (prompt-choice :runner "Archives")
        (run-successful state)
        (core/end-phase-12 state :runner nil)
        (is (empty? (:prompt (get-runner))) "No second prompt for Patron - used already")))))

(deftest power-tap
  ;; Power Tap
  (do-game
    (new-game (default-corp ["Restructured Datapool"])
              (default-runner ["Power Tap"]))
    (play-and-score state "Restructured Datapool")
    (let [agenda (get-scored state :corp 0)
          tags (:tag (get-runner))
          credits (:credit (get-runner))]
      (card-ability state :corp agenda 0)
      (is (= credits (:credit (get-runner))) "Runner shouldn't gain any credits from trace")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= (+ tags 1) (:tag (get-runner))) "Runner should gain 1 tag from losing trace"))
    (take-credits state :corp)
    (play-from-hand state :runner "Power Tap")
    (take-credits state :runner)
    (let [agenda (get-scored state :corp 0)
          tags (:tag (get-runner))
          credits (:credit (get-runner))]
      (card-ability state :corp agenda 0)
      (is (= (+ credits 1) (:credit (get-runner))) "Runner should gain 1 credit from trace initiation")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= (+ tags 1) (:tag (get-runner))) "Runner should gain 1 tag from losing trace"))))

(deftest professional-contacts
  ;; Professional Contacts - Click to gain 1 credit and draw 1 card
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Professional Contacts" 3)
                               (qty "Sure Gamble" 2)
                               (qty "Shiv" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Professional Contacts")
    (let [proco (get-resource state 0)]
      (card-ability state :runner proco 0)
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (:credit (get-runner))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-runner)))) "Drew 1 card")
      (card-ability state :runner proco 0)
      (is (= 1 (:click (get-runner))) "Spent 1 click")
      (is (= 2 (:credit (get-runner))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-runner)))) "Drew 1 card"))))

(deftest reclaim
  ;; Reclaim - trash Reclaim, trash card from grip, install program, hardware, or virtual resource from heap
  (testing "Basic behavior"
    (do-game
      (new-game (default-corp)
                (default-runner ["Reclaim" "Mimic" "Clone Chip"]))
      (take-credits state :corp)
      (core/move state :runner (find-card "Mimic" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Reclaim")
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-card :runner (find-card "Clone Chip" (:hand (get-runner))))
      (prompt-card :runner (find-card "Mimic" (:discard (get-runner))))
      (is (= 1 (count (get-program state))) "1 Program installed")
      (is (= 2 (:credit (get-runner))) "Runner paid install cost")))
  (testing "No cards in hand"
    (do-game
      (new-game (default-corp)
                (default-runner ["Reclaim"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (card-ability state :runner (get-resource state 0) 0)
      (is (empty? (:prompt (get-runner))) "No Reclaim prompt")))
  (testing "Can install trashed card"
    (do-game
      (new-game (default-corp)
                (default-runner ["Reclaim" "Mimic"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-card :runner (find-card "Mimic" (:hand (get-runner))))
      (prompt-card :runner (find-card "Mimic" (:discard (get-runner))))
      (is (= 1 (count (get-program state))) "1 Program installed")
      (is (= 2 (:credit (get-runner))) "Runner paid install cost")))
  (testing "Can't afford to install card"
    (do-game
      (new-game (default-corp)
                (default-runner ["Reclaim" "Alpha"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (card-ability state :runner (get-resource state 0) 0)
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-card :runner (find-card "Alpha" (:hand (get-runner))))
      (prompt-card :runner (find-card "Alpha" (:discard (get-runner))))
      (is (empty? (get-program state)) "Did not install program")
      (is (= 5 (:credit (get-runner))) "Runner did not spend credits"))))

(deftest rolodex
  ;; Rolodex - Full test
  (do-game
    (new-game (default-corp)
              (default-runner ["Rolodex" "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron"]))
    (starting-hand state :runner ["Rolodex"])
    (is (= 1 (count (:hand (get-runner)))))
    (take-credits state :corp)
    (play-from-hand state :runner "Rolodex")
    (prompt-card :runner (find-card "Sure Gamble" (:deck (get-runner))))
    (prompt-card :runner (find-card "Desperado" (:deck (get-runner))))
    (prompt-card :runner (find-card "Diesel" (:deck (get-runner))))
    (prompt-card :runner (find-card "Corroder" (:deck (get-runner))))
    (prompt-card :runner (find-card "Patron" (:deck (get-runner))))
    ;; try starting over
    (prompt-choice :runner "Start over")
    (prompt-card :runner (find-card "Patron" (:deck (get-runner))))
    (prompt-card :runner (find-card "Corroder" (:deck (get-runner))))
    (prompt-card :runner (find-card "Diesel" (:deck (get-runner))))
    (prompt-card :runner (find-card "Desperado" (:deck (get-runner))))
    (prompt-card :runner (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
    (prompt-choice :runner "Done")
    (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
    (is (= "Desperado" (:title (second (:deck (get-runner))))))
    (is (= "Diesel" (:title (second (rest (:deck (get-runner)))))))
    (is (= "Corroder" (:title (second (rest (rest (:deck (get-runner))))))))
    (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-runner)))))))))
    (core/trash state :runner (get-resource state 0))
    (is (last-log-contains? state "Sure Gamble, Desperado, Diesel")
        "Rolodex did log trashed card names")
    (is (= 4 (count (:discard (get-runner)))) "Rolodex mills 3 cards when trashed")
    (is (= "Corroder" (:title (first (:deck (get-runner))))))))

(deftest rosetta-2.0
  ;; Rosetta 2.0 remove an installed program from the game and install one from the heap lower install cost
  (do-game
    (new-game (default-corp)
              (default-runner ["Rosetta 2.0" "Corroder" "Gordian Blade"]))
    (take-credits state :corp)
    (starting-hand state :runner ["Rosetta 2.0" "Corroder"])
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Rosetta 2.0")
    (play-from-hand state :runner "Corroder")
    (is (= 3 (core/available-mu state)) "Corrder cost 1 mu")
    (is (= 2 (:credit (get-runner))) "Starting with 2 credits")
    (card-ability state :runner (get-resource state 0) 0)
    (prompt-select :runner (get-program state 0))
    (prompt-choice :runner (find-card "Gordian Blade" (:deck (get-runner))))
    (is (= 3 (core/available-mu state)) "Gordian cost 1 mu, Corroder freed")
    (is (zero? (:credit (get-runner))) "Ending with 0 credits")
    (is (= 1 (count (:rfg (get-runner)))) "Corroder removed from game")
    (is (= 1 (count (get-program state))) "One program installed")
    (is (= "Gordian Blade" (:title (get-program state 0))) "Gordian installed")))

(deftest sacrificial-construct
  ;; Sacrificial Construct - Trash to prevent trash of installed program or hardware
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Sacrificial Construct" 2) "Cache"
                               "Motivation" "Astrolabe"]))
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "Sacrificial Construct")
    (play-from-hand state :runner "Sacrificial Construct")
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Motivation")
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner)
    (core/trash state :runner (get-resource state 2))
    (is (empty? (:prompt (get-runner))) "Sac Con not prompting to prevent resource trash")
    (core/trash state :runner (get-program state 0))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 2 (count (:discard (get-runner)))) "Sac Con trashed")
    (is (= 1 (count (get-program state))) "Cache still installed")
    (core/trash state :runner (get-hardware state 0))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 3 (count (:discard (get-runner)))) "Sac Con trashed")
    (is (= 1 (count (get-hardware state))) "Astrolabe still installed")))

(deftest safety-first
  ;; Safety First - Reduce hand size by 2, draw 1 at turn end if below maximum
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Safety First" 3) (qty "Cache" 3)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Safety First" "Safety First" "Cache"])
    (play-from-hand state :runner "Safety First")
    (is (= 3 (core/hand-size state :runner)) "Max hand size reduced by 2")
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Drew 1 card at end of turn")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Drew no cards, at maximum")))

(deftest scrubber
  ;; Scrubber
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["The Board"])
                (default-runner ["Scrubber"]))
      (play-from-hand state :corp "The Board" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner doesn't have enough credits to trash")
      (prompt-choice :runner "No action")
      (play-from-hand state :runner "Scrubber")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))) "Runner should only have 5 credits in pool")
      (run-empty-server state "Server 1")
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Runner can use Scrubber credits to trash")
      (let [scrubber (get-resource state 0)]
        (card-ability state :runner scrubber 0)
        (card-ability state :runner scrubber 0))
      (prompt-choice-partial :runner "Pay")
      (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board and gain 2 agenda points")))
  (testing "when under trash cost but can up with recurring credits"
    (do-game
      (new-game (default-corp ["The Board"])
                (default-runner ["Scrubber" "Skulljack" "Sure Gamble"]))
      (play-from-hand state :corp "The Board" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner doesn't have enough credits to trash")
      (prompt-choice :runner "No action")
      (play-from-hand state :runner "Scrubber")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Skulljack")
      (core/gain state :runner :credit 1)
      (is (= 4 (:credit (get-runner))) "Runner should only have 4 credits in pool")
      (run-empty-server state "Server 1")
      (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "The Board should cost 6 to trash")
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Runner can use Scrubber credits to trash")
      (prompt-choice-partial :runner "Pay") ;; Whoops, runner forgot to actually get the credits from Scrubber
      (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "Skulljack shouldn't trigger a second time")
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Runner can still use Scrubber credits the second time around")
      (let [scrubber (get-resource state 0)]
        (card-ability state :runner scrubber 0)
        (card-ability state :runner scrubber 0))
      (prompt-choice-partial :runner "Pay") ;; Now the runner has actually gained the Scrubber credits
      (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board and gain 2 agenda points"))))

(deftest salsette-slums
  ;; Salsette Slums - Once per turn, when the trash cost of a card is paid, optionally remove from the game
  (do-game
    (new-game (default-corp ["Hostile Infrastructure" "Tech Startup" "Thomas Haas"
                             (qty "Hedge Fund" 3)])
              (default-runner [(qty "Salsette Slums" 2) (qty "Sure Gamble" 3)]))
    ;; Use Hostile Infrastructure to ensure on-trash effects don't fire.
    (core/move state :corp (find-card "Hostile Infrastructure" (:deck (get-corp))) :hand)
    (core/move state :corp (find-card "Tech Startup" (:deck (get-corp))) :hand)
    (core/move state :corp (find-card "Thomas Haas" (:deck (get-corp))) :hand)
    (play-from-hand state :corp "Tech Startup" "New remote")
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (play-from-hand state :corp "Thomas Haas" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Salsette Slums")
    (play-from-hand state :runner "Salsette Slums")
    (core/gain state :runner :credit 2)
    (core/gain state :runner :click 4)
    (let [ts1 (get-content state :remote1 0)
          hostile2 (get-content state :remote2 0)
          th3 (get-content state :remote3 0)
          salsette1 (get-resource state 0)
          salsette2 (get-resource state 1)]
      (is (= 3 (count (:hand (get-runner)))) "Runner started this part with three cards in hand")
      (core/rez state :corp hostile2)
      (run-empty-server state "Server 1")
      (is (not (empty? (:prompt (get-runner)))) "Prompting to trash.")
      (card-ability state :runner salsette1 0)
      (is (empty? (:prompt (get-runner))) "All prompts done")
      (is (= 3 (count (:hand (get-runner)))) "On-trash ability of other Hostile didn't fire")
      (is (= (:cid ts1) (:cid (last (:rfg (get-corp))))) "Tech Startup was removed from game")
      (is (= 2 (:credit (get-runner))) "Runner paid the trash cost.")
      (is (not (:run @state)) "Run is over")
      (run-empty-server state :remote2)
      (is (not (empty? (:prompt (get-runner)))) "Prompting to trash")
      ;; Only able to use the ability once per turn
      (card-ability state :runner salsette1 0)
      (is (not (empty? (:prompt (get-runner)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      ;; Can't use the ability if you can't afford to trash
      (card-ability state :runner salsette2 0)
      (is (not (empty? (:prompt (get-runner)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      (prompt-choice :runner "No action")
      ;; Test the "oops I forgot" ability (runner feels bad that they forgot to use Slums when a Hostile is out)
      (run-empty-server state :remote3)
      (prompt-choice-partial :runner "Pay")
      ;; Can only use that first Slums once
      (card-ability state :runner salsette1 1)
      (is (empty? (:prompt (get-runner))) "Not prompting the runner")
      (is (not (= (:cid th3) (:cid (last (:rfg (get-corp)))))) "Card was not removed from the game")
      (card-ability state :runner salsette2 1)
      (is (not (empty? (:prompt (get-runner)))) "Prompting the runner to choose a card")
      (prompt-select :runner (find-card "Thomas Haas" (:discard (get-corp))))
      (is (= (:cid th3) (:cid (last (:rfg (get-corp))))) "Card was removed from the game"))
    ;; Set things up so we can trash the Hostile and then make sure we can't "oops I forgot on a later turn"
    (core/gain state :runner :credit 5)
    (run-empty-server state :remote2)
    (prompt-choice-partial :runner "Pay")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [salsette1 (get-resource state 0)
          hostile2 (get-content state :remote2 0)]
      (card-ability state :runner salsette1 1)
      (prompt-select :runner (find-card "Hostile Infrastructure" (:discard (get-corp))))
      (is (not (= (:cid hostile2) (:cid (last (:rfg (get-corp)))))) "Did not remove card from game"))))

(deftest security-testing
  ;; Security Testing
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Jackson Howard"])
                (default-runner ["Security Testing"]))
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Security Testing")
      (let [st (get-resource state 0)]
        (take-credits state :runner 3)
        (take-credits state :corp)
        (prompt-choice :runner "Server 1")
        (run-empty-server state "Server 1")
        (is (= 10 (:credit (get-runner))) "Gained 2 credits from Security Testing")
        (run-empty-server state "Server 1")
        (prompt-choice :runner "No")
        (is (= 10 (:credit (get-runner))) "Did not gain credits on second run")
        (take-credits state :runner 2)
        (take-credits state :corp)
        (prompt-choice :runner "Server 1")
        (run-empty-server state "Archives")
        (is (= 12 (:credit (get-runner))) "Did not gain credits when running other server"))))
  (testing "with multiple copies"
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Security Testing" 2)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (take-credits state :corp)
      (prompt-choice :runner "Archives")
      (prompt-choice :runner "R&D")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-runner))) "Gained 2 credits")
      (run-empty-server state "R&D")
      (is (= 11 (:credit (get-runner)))))))

(deftest spoilers
  ;; Spoilers - Mill the Corp when it scores an agenda
  (do-game
    (new-game (default-corp ["Hostile Takeover" "Hedge Fund"])
              (default-runner ["Spoilers"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Spoilers")
    (take-credits state :runner)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (is (= 1 (count (:deck (get-corp)))))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :corp ht)
      (is (= 1 (count (:discard (get-corp)))))
      (is (zero? (count (:deck (get-corp)))) "Last card from R&D milled"))))

(deftest stim-dealer
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game (default-corp)
              (default-runner ["Stim Dealer" "Sure Gamble" "Feedback Filter"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "Stim Dealer")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [sd (get-resource state 0)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (get-counters (refresh sd) :power)) "Lost all counters")
      (is (empty? (:prompt (get-runner))) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
      (is (= 4 (:click (get-runner))) "Didn't gain extra click"))))

(deftest street-peddler
  ;; Street Peddler
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner ["Street Peddler" "Gordian Blade"
                                 "Torch" (qty "Sure Gamble" 2)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Sure Gamble"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
        (card-ability state :runner sp 0)
        (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-program state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))
  (testing "Can't afford install"
    (do-game
      (new-game (default-corp)
                (default-runner ["Street Peddler" (qty "Gordian Blade" 3)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (card-ability state :runner sp 0)
        (core/lose state :runner :credit 3)
        (is (= 2 (count (:choices (first (:prompt (get-runner))))))
            "1 card and 1 cancel option on Street Peddler")
        (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (zero? (count (get-program state)))
            "Gordian Blade was not installed")
        (is (and (:installed (refresh sp)) (= 3 (count (:hosted (refresh sp))))
                 "Street Peddler still installed with 3 hosted cards")))))
  (testing "Interaction with Kate discount"
    (do-game
      (new-game (default-corp)
                (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" ["Street Peddler"
                                                                     "Gordian Blade"
                                                                     (qty "Sure Gamble" 2)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        ;; should still be able to afford Gordian w/ Kate discount
        (core/lose state :runner :credit 3)
        (card-ability state :runner sp 0)
        (is (= 2 (count (:choices (first (:prompt (get-runner))))))
            "Only 1 choice (plus Cancel) to install off Peddler")
        (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-program state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))
  (testing "Programs should cost memory. Issue #708"
    (do-game
      (new-game (default-corp)
                (default-runner ["Street Peddler" (qty "Corroder" 3)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (is (= 4 (core/available-mu state)) "No memory cost for hosting on Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
        (card-ability state :runner sp 0)
        (prompt-card :runner (first (:hosted sp))) ; choose to install Gordian
        (is (= "Corroder" (:title (get-program state 0)))
            "Corroder was installed")
        (is (= 3 (core/available-mu state)) "Corroder cost 1 mu"))))
  (testing "Muertos/Brain Chip uninstall effect not fired when removed off peddler/hosting Issue #2294, #2358"
    (do-game
      (new-game (default-corp ["Jackson Howard"])
                (default-runner [(qty "Street Peddler" 2) "Muertos Gang Member" "Brain Chip"]))
      (core/move state :runner (find-card "Muertos Gang Member" (:hand (get-runner))) :deck {:front true})
      (core/move state :runner (find-card "Brain Chip" (:hand (get-runner))) :deck {:front true})
      (core/move state :runner (find-card "Street Peddler" (:hand (get-runner))) :deck {:front true})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Street Peddler")
      (core/gain state :runner :agenda-point 1)
      (let [jh (get-content state :remote1 0)
            sp (get-resource state 0)]
        (core/rez state :corp jh)
        (card-ability state :runner sp 0)
        (prompt-card :runner (find-card "Street Peddler" (:hosted sp))) ; choose to another Peddler
        (is (empty? (:prompt (get-corp))) "Corp not prompted to rez Jackson")
        (is (= 4 (core/available-mu state)) "Runner has 4 MU"))))
  (testing "Trashing hardware should not reduce :in-play values"
    (do-game
      (new-game (default-corp)
                (default-runner ["Street Peddler" (qty "HQ Interface" 3)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (card-ability state :runner sp 0)
        (prompt-card :runner (first (:hosted sp))) ; choose to install HQ Interface
        (is (= 2 (:hq-access (get-runner)))
            "HQ Access increased by 1 from installed HQI and not reduced by the 2 trashed ones"))))
  (testing "Installing Parasite with only 1cr. Issue #491."
    (do-game
      (new-game (default-corp [(qty "Pop-up Window" 3)])
                (default-runner ["Street Peddler" (qty "Parasite" 3)]))
      (play-from-hand state :corp "Pop-up Window" "HQ")
      (take-credits state :corp 2)
      (starting-hand state :runner ["Street Peddler"])
      (core/lose state :runner :credit 4) ; go down to 1 credit
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)
            pu (get-ice state :hq 0)]
        (core/rez state :corp pu)
        (card-ability state :runner sp 0)
        (prompt-card :runner (first (:hosted sp))) ; choose to install Parasite
        (is (= "Parasite" (:title (:card (first (get-in @state [:runner :prompt])))))
            "Parasite target prompt")
        (prompt-select :runner pu)
        (is (= 4 (count (:discard (get-runner)))) "3 Parasite, 1 Street Peddler in heap")
        (is (= 1 (count (:discard (get-corp)))) "Pop-up Window in archives"))))
  (testing "Tech Trader install"
    (do-game
      (new-game (default-corp)
                (default-runner ["Street Peddler"
                                 "Tech Trader"]))
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= 1 (count (:hosted sp))) "Street Peddler is hosting 1 card")
        (card-ability state :runner sp 0)
        (prompt-card :runner (find-card "Tech Trader" (:hosted sp))) ; choose to install Tech Trader
        (is (= "Tech Trader" (:title (get-resource state 0)))
            "Tech Trader was installed")
        (is (= 5 (:credit (get-runner))) "Did not gain 1cr from Tech Trader ability")))))

(deftest-pending street-peddler-trash-while-choosing-card
  ;; Street Peddler - trashing Street Peddler while choosing which card to
  ;; discard should dismiss the choice prompt. Issue #587.
  (do-game
    (new-game (default-corp)
              (default-runner ["Street Peddler"
                               "Gordian Blade"
                               "Torch"
                               (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :runner "Street Peddler")
    (let [street-peddler (get-resource state 0)]
      (is (= 3 (count (:hosted street-peddler))) "Street Peddler is hosting 3 cards")
      (card-ability state :runner street-peddler 0)
      (trash-resource state "Street Peddler")
      (is (zero? (count (get-in @state [:runner :prompt])))))))

(deftest symmetrical-visage
  ;; Symmetrical Visage - Gain 1 credit the first time you click to draw each turn
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Symmetrical Visage" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Symmetrical Visage")
      (is (= 3 (:credit (get-runner))))
      (core/click-draw state :runner nil)
      (is (= 4 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
      (core/click-draw state :runner nil)
      (is (= 4 (:credit (get-runner))) "No credit gained from second click spent to draw")))
  (testing "Gain 1 credit the first and second time you click to draw each turn when GCS is installed"
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Symmetrical Visage" 3)
                                 (qty "Gene Conditioning Shoppe" 3)
                                 "Fall Guy"]))
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Symmetrical Visage")
      (is (= 3 (:credit (get-runner))))
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (is (= 1 (:credit (get-runner))))
      (core/click-draw state :runner nil)
      (is (= 2 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
      (core/click-draw state :runner nil)
      (is (= 3 (:credit (get-runner)))
          "Gained 1 credit from second click spent to draw with Gene Conditioning Shoppe")
      ;; Move Fall Guy back to deck
      (core/move state :runner (find-card "Fall Guy" (:hand (get-runner))) :deck)
      (core/click-draw state :runner nil)
      (is (= 3 (:credit (get-runner)))
          "No credit gained from third click spent to draw with Gene Conditioning Shoppe"))))

(deftest synthetic-blood
  ;; Synthetic Blood - The first time you take damage each turn, draw one card
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
                (default-runner [(qty "Synthetic Blood" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]))
      (play-from-hand state :corp "Data Mine" "HQ")
      (play-from-hand state :corp "Data Mine" "HQ")
      (take-credits state :corp)
      (let [first-dm (get-ice state :hq 1)
            second-dm (get-ice state :hq 0)]
        (play-from-hand state :runner "Synthetic Blood")
        (run-on state "HQ")
        (core/rez state :corp first-dm)
        (card-subroutine state :corp first-dm 0)
        (is (= 4 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
        (run-continue state)
        (core/rez state :corp second-dm)
        (card-subroutine state :corp second-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "no card drawn when receiving damage (2nd time)"))))
  (testing "The first and second time you take damage each turn (with GCS installed), draw one card"
    (do-game
      (new-game (default-corp [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
                (default-runner [(qty "Synthetic Blood" 3)
                                 "Sure Gamble"
                                 (qty "Gene Conditioning Shoppe" 3)]))
      (play-from-hand state :corp "Data Mine" "HQ")
      (play-from-hand state :corp "Data Mine" "HQ")
      (take-credits state :corp)
      (let [first-dm (get-ice state :hq 1)
            second-dm (get-ice state :hq 0)]
        (play-from-hand state :runner "Synthetic Blood")
        (play-from-hand state :runner "Gene Conditioning Shoppe")
        (run-on state "HQ")
        (core/rez state :corp first-dm)
        (card-subroutine state :corp first-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
        (run-continue state)
        (core/rez state :corp second-dm)
        (card-subroutine state :corp second-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (2nd time)")))))

(deftest technical-writer
  ;; Technical Writer - Gain 1c per program/hardware install; click/trash to take all credits
  (do-game
    (new-game (default-corp)
              (default-runner ["Technical Writer" (qty "Faerie" 2)
                               "Vigil" "Same Old Thing"]))
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "Technical Writer")
    (let [tw (get-resource state 0)]
      (play-from-hand state :runner "Faerie")
      (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :runner "Faerie")
      (is (= 2 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :runner "Vigil")
      (is (= 3 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :runner "Same Old Thing")
      (is (= 3 (get-counters (refresh tw) :credit)) "No credit gained for resource install")
      (card-ability state :runner tw 0)
      (is (= 6 (:credit (get-runner))) "Gained 3 credits")
      (is (zero? (:click (get-runner))) "Spent 1 click")
      (is (= 1 (count (:discard (get-runner)))) "Technical Writer trashed"))))

(deftest the-helpful-ai
  ;; The Helpful AI - +1 link; trash to give an icebreaker +2 str until end of turn
  (do-game
    (new-game (default-corp)
              (default-runner ["The Helpful AI" "Corroder"]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Helpful AI")
    (is (= 1 (:link (get-runner))) "Gained 1 link")
    (play-from-hand state :runner "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-select :runner corr)
      (is (= 4 (:current-strength (refresh corr))) "Corroder has +2 strength")
      (is (= 1 (count (:discard (get-runner)))) "Helpful AI trashed")
      (is (zero? (:link (get-runner))))
      (take-credits state :runner)
      (is (= 2 (:current-strength (refresh corr))) "Corroder back to default strength"))))

(deftest the-source
  ;; The Source - Increase advancement requirement of agendas by 1; 3c additional cost to steal
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 2)])
              (default-runner [(qty "The Source" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "The Source")
    (run-empty-server state :remote1)
    (prompt-choice-partial :runner "Pay") ; pay 3c extra to steal
    (is (= 4 (:credit (get-runner))) "Paid 3c to steal")
    (is (= 2 (count (:discard (get-runner)))) "The Source is trashed")
    (play-from-hand state :runner "The Source")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote2 0)]
      (core/advance state :corp {:card (refresh ht)})
      (core/advance state :corp {:card (refresh ht)})
      (core/score state :corp {:card (refresh ht)})
      (is (empty? (:scored (get-corp))) "Hostile Takeover can't be scored with 2 adv")
      (core/gain state :corp :click 1)
      (core/advance state :corp {:card (refresh ht)})
      (core/score state :corp {:card (refresh ht)})
      (is (= 1 (:agenda-point (get-corp))) "Hostile Takeover scored with 3 adv")
      (is (= 3 (count (:discard (get-runner)))) "The Source is trashed"))))

(deftest the-supplier
  ;; The Supplier
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner ["The Supplier"
                                 "Plascrete Carapace"
                                 "Utopia Shard"
                                 "Hedge Fund"]))
      (take-credits state :corp)
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (prompt-select :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
        (card-ability state :runner ts 0)
        (is (= 1 (count (-> @state :runner :prompt first :choices))))
        (prompt-select :runner (find-card "Utopia Shard" (:hand (get-runner))))
        (is (= 2 (count (:hosted (refresh ts)))) "The Supplier is hosting 2 cards")
        (take-credits state :runner)
        (take-credits state :corp)
        ;; Utopia Shard cannot be afforded and should not be in the prompt
        (prompt-select :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (= 2 (:credit (get-runner)))
            "Runner charged 1 credit to install Plascrete off The Supplier")
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))) "Runner ends turn with 5 credits")
        (is (= 1 (count (:hosted (refresh ts)))) "One card still on The Supplier"))))
  (testing "Interaction with Kate discount. Issue #578."
    (do-game
      (new-game (default-corp)
                (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                           ["The Supplier"
                            "Plascrete Carapace"
                            "Kati Jones"
                            "Hedge Fund"]))
      (take-credits state :corp)
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (prompt-select :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
        (core/lose state :runner :credit (:credit (get-runner)))
        (core/end-turn state :runner nil)
        (take-credits state :corp)
        (prompt-select :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (zero? (:credit (get-runner))) "Kate discount applied")
        (is (= 1 (count (get-resource state))) "Plascrete installed"))))
  (testing "Brain chip mem is deducted when it is hosted and Supplier is trashed. Issue #2358"
    (do-game
      (new-game (default-corp [(qty "Hostile Takeover" 2)])
                (default-runner ["The Supplier"
                                 "Brain Chip"]))
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (is (= 4 (core/available-mu state)) "Runner has 4 MU")
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (prompt-select :runner (find-card "Brain Chip" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "Runner has 4 MU")
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Steal")
        (take-credits state :runner)
        (core/gain state :runner :tag 1)
        (core/trash-resource state :corp nil)
        (prompt-select :corp (get-resource state 0))
        (is (= 2 (count (:discard (get-runner)))))
        (is (= 4 (core/available-mu state)) "Runner has 4 MU")))))

(deftest thunder-art-gallery
  (testing "Works when removing/avoiding tags"
    (do-game
      (new-game (default-corp)
                (default-runner ["Thunder Art Gallery" "New Angeles City Hall" "Corroder"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Thunder Art Gallery")
      (core/gain-credits state :runner 1)
      (core/gain-tags state :corp 1)
      (core/remove-tag state :runner nil)
      (prompt-card :runner (find-card "New Angeles City Hall" (:hand (get-runner))))
      (is (= 1 (:credit (get-runner))) "Runner paid one less to install (but 2 to remove tag)")
      (is (= "New Angeles City Hall" (:title (get-resource state 1))) "NACH is installed")
      (take-credits state :runner)
      (is (= 3 (:credit (get-runner))) "Runner is now at 3 credits")
      (core/gain-tags state :corp 1)
      (card-ability state :runner (get-resource state 1) 0)
      (prompt-choice :runner "Done")
      (prompt-card :runner (find-card "Corroder" (:hand (get-runner))))
      (is (= 0 (:credit (get-runner))) "Runner paid one less to install")
      (is (= "Corroder" (:title (get-program state 0))) "Corroder is installed"))))

(deftest tech-trader
  ;; Basic test
  (do-game
    (new-game (default-corp)
              (default-runner ["Tech Trader" "Fall Guy"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Tech Trader")
    (play-from-hand state :runner "Fall Guy")
    (is (= 4 (:credit (get-runner))))
    (let [fall (get-resource state 1)]
      (card-ability state :runner fall 1)
      (is (= 7 (:credit (get-runner)))))))

(deftest the-archivist
  ;; The Archivist
  (do-game
    (new-game (default-corp ["Global Food Initiative" "Private Security Force"])
              (default-runner ["The Archivist"]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Archivist")
    (is (zero? (:bad-publicity (get-corp))) "Corp should start with 0 bad publicity")
    (take-credits state :runner)
    (play-and-score state "Global Food Initiative")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:bad-publicity (get-corp))) "Corp should get 1 bad publicity from The Archivist")
    (play-and-score state "Private Security Force")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 2 (:bad-publicity (get-corp))) "Corp should get 1 bad publicity from The Archivist")))

(deftest the-black-file
  ;; The Black File - Prevent Corp from winning by agenda points
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Vanity Project" 3) (qty "Sure Gamble" 3)])
                (default-runner ["The Black File"]))
      (starting-hand state :corp ["Vanity Project"])
      (core/gain state :corp :agenda-point 3)
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (take-credits state :runner)
      (play-from-hand state :corp "Vanity Project" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 7 (:agenda-point (get-corp))))
      (is (not (:winner @state)) "No registered Corp win")
      (take-credits state :corp)
      (let [bf (get-resource state 0)]
        (is (= 1 (get-counters (refresh bf) :power)) "1 power counter on The Black File")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh bf) :power)) "2 power counters on The Black File")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:rfg (get-runner)))) "The Black File removed from the game")
        (is (= :corp (:winner @state)) "Corp wins")
        (is (= "Agenda" (:reason @state)) "Win condition reports agendas"))))
  (testing "Corp can still win by flatlining Runner"
    (do-game
      (new-game (default-corp [(qty "Vanity Project" 3) (qty "Scorched Earth" 3)])
                (default-runner ["The Black File"]))
      (starting-hand state :corp ["Vanity Project" "Scorched Earth"])
      (core/gain state :corp :agenda-point 3)
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (take-credits state :runner)
      (play-from-hand state :corp "Vanity Project" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 7 (:agenda-point (get-corp))))
      (is (not (:winner @state)) "No registered Corp win")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest temujin-contract
  ;; Temüjin Contract
  (testing "Multiple times in one turn. Issue #1952"
    (do-game
      (new-game (default-corp)
                (make-deck "Silhouette: Stealth Operative" ["Temüjin Contract"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Temüjin Contract")
      (prompt-choice :runner "Archives")
      (run-empty-server state "Archives")
      (is (= 5 (:credit (get-runner))) "Gained 4cr")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "Temjin has 12 credits remaining"))))

(deftest the-turning-wheel
  ;; The Turning Wheel
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Hostile Takeover" "Ice Wall" "Ice Wall"])
                (default-runner ["The Turning Wheel"]))
      (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (core/gain state :runner :click 10 :credit 10)
      (let [ttw (get-resource state 0)]
        (run-empty-server state "R&D")
        (prompt-choice :runner "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (prompt-choice :runner "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "R&D")
        (card-ability state :runner ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Runner should access 1 additional card"))))
  (testing "Access bonus shouldn't carry over to other runs if prematurely ended after spending TTW counters. #3598"
    (do-game
      (new-game (default-corp ["Nisei MK II"])
                (default-runner ["The Turning Wheel"]))
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
      (new-game (default-corp ["Hostile Takeover" "Ice Wall"])
                (default-runner ["The Turning Wheel"]))
      (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
      (trash-from-hand state :corp "Hostile Takeover")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (core/gain state :runner :click 10 :credit 10)
      (let [ttw (get-resource state 0)]
        (run-empty-server state "R&D")
        (prompt-choice :runner "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (prompt-choice :runner "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "Archives")
        (card-ability state :runner ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Runner should access 1 additional card")
        (run-successful state)
        (is (zero? (-> @state :run :access-bonus)) "Access bonuses are zeroed out when attacked server isn't R&D or HQ")))))

(deftest theophilius-bagbiter
  ;; Theophilius Bagbiter - hand size is equal to credit pool
  (do-game
    (new-game (default-corp)
              (default-runner ["Theophilius Bagbiter"]))
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Runner starts with 5c")
    (play-from-hand state :runner "Theophilius Bagbiter")
    (is (zero? (:credit (get-runner))) "Runner loses all credits on install")
    (is (= 1 (count (get-resource state))) "Theophilius Bagbiter installed")
    (is (zero? (core/hand-size state :runner)) "Max hand size is 0")
    (core/gain state :runner :credit 7)
    (is (= 7 (:credit (get-runner))) "Runner has 7c")
    (is (= 7 (core/hand-size state :runner)) "Max hand size is 7")
    (core/trash-resource state :runner nil)
    (prompt-select :runner (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))) "Theo is trashed")
    (is (empty? (get-resource state)) "No resources installed")
    (is (= 5 (core/hand-size state :runner)) "Max hand size is reset to default")))

(deftest tri-maf-contact
  ;; Tri-maf Contact - Click for 2c once per turn; take 3 meat dmg when trashed
  (do-game
    (new-game (default-corp)
              (default-runner ["Tri-maf Contact" (qty "Cache" 3) "Shiv"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Tri-maf Contact")
    (let [tmc (get-resource state 0)]
      (card-ability state :runner tmc 0)
      (is (= 5 (:credit (get-runner))) "Gained 2c")
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (card-ability state :runner tmc 0)
      (is (= 5 (:credit (get-runner))) "No credits gained; already used this turn")
      (core/move state :runner tmc :hand)
      (is (= 5 (count (:hand (get-runner)))) "No meat damage")
      (play-from-hand state :runner "Tri-maf Contact")
      (core/gain state :runner :tag 1)
      (take-credits state :runner)
      (core/trash-resource state :corp nil)
      (prompt-select :corp (get-resource state 0))
      (is (= 4 (count (:discard (get-runner)))) "Took 3 meat damage"))))

(deftest virus-breeding-ground
  ;; Virus Breeding Ground - Gain counters
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner ["Virus Breeding Ground"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (let [vbg (get-resource state 0)]
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn"))))
  (testing "Can move to programs pumped by Hivemind"
    (do-game
      (new-game (default-corp)
                (default-runner ["Virus Breeding Ground" "Hivemind" "Aumakua"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Hivemind")
      (play-from-hand state :runner "Aumakua")
      (let [aum (get-program state 1)
            vbg (get-resource state 0)]
        (is (zero? (get-counters aum :virus)) "Aumakua starts with 0 counters (excluding Hivemind)")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner vbg 0)
        (prompt-select :runner aum)
        (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))
  (testing "Move counters"
    (do-game
      (new-game (default-corp)
                (default-runner ["Virus Breeding Ground" "Hivemind"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Hivemind")
      (let [hive (get-program state 0)
            vbg (get-resource state 0)]
        (is (= 1 (get-counters hive :virus)) "Hivemind starts with 1 counter")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner vbg 0)
        (prompt-select :runner hive)
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))
  (testing "Move counters to a non-virus resource"
    (do-game
      (new-game (default-corp)
                (default-runner ["Virus Breeding Ground" "Crypt"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Crypt")
      (let [vbg (get-resource state 0)
            crypt (get-resource state 1)]
        (is (zero? (get-counters crypt :virus)) "Crypt starts with 0 counters")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner (refresh vbg) 0)
        (prompt-select :runner (refresh crypt))
        (prompt-choice :runner "Done")
        (is (zero? (get-counters (refresh crypt) :virus)) "Crypt doesn't gain a counter")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground doesn't lose a counter")
        (run-on state "Archives")
        (run-successful state)
        (prompt-choice :runner "Yes")
        (is (= 1 (get-counters (refresh crypt) :virus)) "Crypt gained a counter")
        (card-ability state :runner (refresh vbg) 0)
        (prompt-select :runner (refresh crypt))
        (is (= 2 (get-counters (refresh crypt) :virus)) "Crypt gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))

(deftest wasteland
  ;; Wasteland - Gain 1c the first time you trash an installed card of yours each turn
  (do-game
    (new-game (default-corp ["PAD Campaign"])
              (default-runner ["Wasteland" "Faust" (qty "Fall Guy" 4)]))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (core/gain state :runner :credit 4)
    (core/draw state :runner)
    (play-from-hand state :runner "Faust")
    (play-from-hand state :runner "Wasteland")
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (run-empty-server state "Server 1")
    (prompt-choice-partial :runner "Pay") ; Trash PAD campaign
    (is (zero? (:credit (get-runner))) "Gained nothing from Wasteland on corp trash")
    ; trash from hand first which should not trigger #2291
    (let [faust (get-program state 0)]
      (card-ability state :runner faust 1)
      (prompt-card :runner (first (:hand (get-runner)))))
    (is (zero? (:credit (get-runner))) "Gained nothing from Wasteland")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Fall Guy")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 1 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 3 (:credit (get-runner))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (take-credits state :runner)
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 2 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 6 (:credit (get-runner))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 3 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 8 (:credit (get-runner))) "Gained 2c from Fall Guy but no credits from Wasteland")))

(deftest xanadu
  ;; Xanadu - Increase all ICE rez cost by 1 credit
  (do-game
    (new-game (default-corp [(qty "Paper Wall" 2) "Launch Campaign"])
              (default-runner ["Xanadu"]))
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Xanadu")
    (let [pw1 (get-ice state :hq 0)
          pw2 (get-ice state :rd 0)
          lc (get-content state :remote1 0)]
      (core/rez state :corp pw1)
      (is (= 4 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :corp pw2)
      (is (= 3 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :corp lc)
      (is (= 2 (:credit (get-corp))) "Paid 1 to rez Launch Campaign; no effect on non-ICE"))))

(deftest zona-sul-shipping
  ;; Zona Sul Shipping - Gain 1c per turn, click to take all credits. Trash when tagged
  (do-game
    (new-game (default-corp)
              (default-runner ["Zona Sul Shipping"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Zona Sul Shipping")
    (let [zss (get-resource state 0)]
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (get-counters (refresh zss) :credit)) "Zona Sul holds 1c")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh zss) :credit)) "Zona Sul holds 2c")
      (card-ability state :runner zss 0)
      (is (= 12 (:credit (get-runner))) "Took 2c off Zona Sul")
      (is (= 3 (:click (get-runner))) "Spent 1 click")
      (core/gain state :runner :tag 1)
      (is (= 1 (count (:discard (get-runner)))) "Zona Sul trashed when tag taken"))))
