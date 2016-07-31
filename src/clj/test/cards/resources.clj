(ns test.cards.resources
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest activist-support
  "Activist Support - Take tag if you have none; Corp gains bad pub if they have none"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Activist Support" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Activist Support")
    (is (= 0 (:tag (get-runner))))
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag; had none")
    (is (= 0 (:bad-publicity (get-corp))))
    (take-credits state :corp)
    (is (= 1 (:bad-publicity (get-corp))) "Corp took 1 bad pub; had none")
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Runner had 1 tag; didn't take another")
    (take-credits state :corp)
    (is (= 1 (:bad-publicity (get-corp))) "Corp had 1 bad pub; didn't take another")))

(deftest adjusted-chronotype
  "Ensure adjusted chronotype gains only 1 click when 2 clicks are lost"
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Adjusted Chronotype" 1) (qty "Beach Party" 2)]))
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

(deftest adjusted-chronotype-gcs
  "Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed"
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Adjusted Chronotype" 1)
                              (qty "Beach Party" 3)
                              (qty "Gene Conditioning Shoppe" 1)]))
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
   (is (= 3 (:click (get-runner))) "Should have lost 3 clicks and gained 2 clicks")))

(deftest aesops-pawnshop
  "Tests use cases for Aesop's Pawnshop"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Aesop's Pawnshop" 1) (qty "Cache" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Aesop's Pawnshop")
    (play-from-hand state :runner "Cache")
    (let [orig-credits (:credit (get-runner))
          ap (get-in @state [:runner :rig :resource 0])
          cache (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner ap 0)
      (prompt-select :runner cache)
      (card-ability state :runner ap 0)
      (prompt-select :runner ap)
      (let [ap (get-in @state [:runner :rig :resource 0])
            cache (get-in @state [:runner :discard 0])]
        (is (= (+ 3 orig-credits) (:credit (get-runner))) "Should have only gained 3 credits")
        (is (not= cache nil) "Cache should be in Heap")
        (is (not= ap nil) "Aesops should still be installed")))))

(deftest all-nighter
  "All-nighter - Click/trash to gain 2 clicks"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "All-nighter" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "All-nighter")
    (is (= 3 (:click (get-runner))))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 4 (:click (get-runner))) "Spent 1 click; gained 2 clicks")
    (is (= 1 (count (:discard (get-runner)))) "All-nighter is trashed")))

(deftest bank-job-manhunt
  "Bank Job - Manhunt trace happens first"
  (do-game
    (new-game (default-corp [(qty "Manhunt" 1) (qty "PAD Campaign" 1)])
              (default-runner [(qty "Bank Job" 1)]))
    (play-from-hand state :corp "Manhunt")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Bank Job")
    (run-empty-server state "Server 1")
    (prompt-choice :corp 2) ; Manhunt trace active
    (prompt-choice :runner 0)
    (prompt-choice :runner "Run ability")
    (is (= "Bank Job" (:title (:card (first (get-in @state [:runner :prompt])))))
        "Bank Job prompt active")
    (prompt-choice :runner 8)
    (is (empty? (get-in @state [:runner :rig :resource])) "Bank Job trashed after all credits taken")
    (is (= 1 (count (:discard (get-runner)))))))

(deftest bank-job-multiple-copies
  "Bank Job - Choose which to use when 2+ copies are installed"
    (do-game
      (new-game (default-corp [(qty "PAD Campaign" 1)])
                (default-runner [(qty "Bank Job" 2)]))
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (prompt-choice :runner "Run ability")
      (prompt-choice :runner 4)
      (play-from-hand state :runner "Bank Job")
      (let [bj1 (get-resource state 0)
            bj2 (get-resource state 1)]
        (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Run ability")
        (prompt-select :runner bj2)
        (prompt-choice :runner 6)
        (is (= 13 (:credit (get-runner))))
        (is (= 2 (get-counters (refresh bj2) :credit)) "2 credits remaining on 2nd copy"))))

(deftest bank-job-sectesting
  "Bank Job - Security Testing takes priority"
  (do-game
    (new-game (default-corp [(qty "PAD Campaign" 1)])
              (default-runner [(qty "Bank Job" 1) (qty "Security Testing" 1)]))
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
    (is (= 8 (:credit (get-runner))) "Security Testing paid 2c")))

(deftest bazaar-grip-only
  "Bazaar - Only triggers when installing from Grip"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Street Peddler" 1)
                               (qty "Bazaar" 1)
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
  "Beach Party - Lose 1 click when turn begins; hand size increased by 5"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Beach Party" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Beach Party")
    (is (= 10 (core/hand-size state :runner)) "Max hand size increased by 5")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))) "Lost 1 click at turn start")))

(deftest chrome-parlor
  "Chrome Parlor - Prevent all meat/brain dmg when installing cybernetics"
  (do-game
    (new-game (default-corp [(qty "Traffic Accident" 1)])
              (default-runner [(qty "Chrome Parlor" 1) (qty "Titanium Ribs" 1)
                               (qty "Brain Cage" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Chrome Parlor")
    (play-from-hand state :runner "Titanium Ribs")
    (is (empty? (:prompt (get-runner))) "Damage prevented, no Ribs prompt to choose cards")
    (is (= 3 (count (:hand (get-runner)))))
    (play-from-hand state :runner "Brain Cage")
    (is (= 2 (count (:hand (get-runner)))) "No cards lost")
    (is (= 0 (:brain-damage (get-runner))))
    (is (= 8 (core/hand-size state :runner)) "Runner hand size boosted by Brain Cage")
    (take-credits state :runner)
    (core/gain state :runner :tag 2)
    (core/trash state :runner (get-hardware state 0))
    (play-from-hand state :corp "Traffic Accident")
    (is (= 3 (count (:discard (get-runner)))) "Conventional meat damage not prevented by Parlor")))

(deftest compromised-employee
  "Compromised Employee - Gain 1c every time Corp rezzes ICE"
  (do-game
    (new-game (default-corp [(qty "Pup" 2) (qty "Launch Campaign" 1)])
              (default-runner [(qty "Compromised Employee" 1)]))
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Pup" "R&D")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Compromised Employee")
    (let [ce (get-resource state 0)]
      (is (= 1 (:rec-counter (refresh ce))) "Has 1 recurring credit")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 4 (:credit (get-runner))) "Gained 1c from ICE rez")
      (core/rez state :corp (get-ice state :rd 0))
      (is (= 5 (:credit (get-runner))) "Gained 1c from ICE rez")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 5 (:credit (get-runner))) "Asset rezzed, no credit gained"))))

(deftest daily-casts
  "Play and tick through all turns of daily casts"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Daily Casts" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (let [dc (get-in @state [:runner :rig :resource 0])]
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
      (is (nil? (get-in @state [:runner :rig :resource 0]))))))

(deftest data-folding
  "Data Folding - Gain 1c at start of turn if 2+ unused MU"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Data Folding" 1) (qty "Hyperdriver" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Data Folding")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (:memory (get-runner))) "At least 2 unused MU")
    (is (= 6 (:credit (get-runner))) "Gained 1c at turn start")
    (play-from-hand state :runner "Hyperdriver")
    (take-credits state :runner)
    (is (= 1 (:memory (get-runner))) "Only 1 unused MU")
    (is (= 8 (:credit (get-runner))))
    (take-credits state :corp)
    (is (= 8 (:credit (get-runner))) "No credits gained at turn start")))

(deftest ddos
  "Prevent rezzing of outermost ice for the rest of the turn"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3)])
              (default-runner [(qty "DDoS" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "DDoS")
    (let [ddos (get-in @state [:runner :rig :resource 0])
          iwall (get-ice state :hq 1)]
      (card-ability state :runner ddos 0)
      (is (= (:title ddos) (get-in @state [:runner :discard 0 :title])))
      (run-on state "HQ")
      (core/rez state :corp iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (run-jack-out state)
      (run-on state "HQ")
      (core/rez state :corp iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state "HQ")
      (core/rez state :corp iwall)
      (is (get-in (refresh iwall) [:rezzed])))))

(deftest decoy
  "Decoy - Trash to avoid 1 tag"
  (do-game
    (new-game (default-corp [(qty "SEA Source" 1)])
              (default-runner [(qty "Decoy" 1)]))
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
    (is (= 0 (:tag (get-runner))) "Tag avoided")))

(deftest eden-shard
  "Eden Shard - Install from Grip in lieu of accessing R&D; trash to make Corp draw 2"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Eden Shard" 1)]))
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (is (= 1 (count (:hand (get-corp)))))
    (run-on state :rd)
    (core/no-action state :corp nil)
    (play-from-hand state :runner "Eden Shard")
    (is (= 5 (:credit (get-runner))) "Eden Shard installed for 0c")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 3 (count (:hand (get-corp)))) "Corp drew 2 cards")
    (is (= 1 (count (:discard (get-runner)))) "Eden Shard trashed")))

(deftest fan-site
  "Fan Site - Add to score area as 0 points when Corp scores an agenda"
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1)])
              (default-runner [(qty "Fan Site" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Fan Site")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (is (= 0 (:agenda-point (get-runner))))
    (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")))

(deftest fan-site-eoi
  "Fan Site - Don't trigger after swap with Exchange of Information. Issue #1824"
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 2) (qty "Exchange of Information" 1)])
              (default-runner [(qty "Fan Site" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Fan Site")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (core/tag-runner state :runner 1)

    (play-from-hand state :corp "Exchange of Information")

    (prompt-select :corp (find-card "Fan Site" (:scored (get-runner))))
    (prompt-select :corp (find-card "Hostile Takeover" (:scored (get-corp))))

    (is (= 1 (:agenda-point (get-runner))))
    (is (= 0 (:agenda-point (get-corp))))

    (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site swapped into Corp score area")

    (prn (find-card "Fan Site" (:scored (get-corp))))

    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (score-agenda state :corp (get-content state :remote2 0))
    (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site not removed from Corp score area")))

(deftest fester
  "Fester - Corp loses 2c (if able) when purging viruses"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Fester" 1)]))
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

(deftest film-critic-discarded-executives
  "Film Critic - Prevent Corp-trashed execs going to Runner scored. Issues #1181/#1042"
  (do-game
    (new-game (default-corp [(qty "Director Haas" 3) (qty "Project Vitruvius" 3) (qty "Hedge Fund" 1)])
              (default-runner [(qty "Film Critic" 1)]))
    (play-from-hand state :corp "Project Vitruvius" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Film Critic")
    (let [fc (first (get-in @state [:runner :rig :resource]))]
      (run-empty-server state "Server 1")
      (card-ability state :runner fc 0)
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (take-credits state :runner)
      (trash-from-hand state :corp "Director Haas")
      (is (= 1 (count (:discard (get-corp)))) "Director Haas stayed in Archives")
      (is (= 0 (:agenda-point (get-runner))) "No points gained by Runner")
      (is (empty? (:scored (get-runner))) "Nothing in Runner scored"))))

(deftest film-critic-fetal-ai
  "Film Critic - Fetal AI interaction"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Film Critic" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Film Critic")
    (let [fc (first (get-in @state [:runner :rig :resource]))]
      (run-empty-server state "HQ")
      ;; should not have taken damage yet
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt yet")
      (card-ability state :runner fc 0)
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt")
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (card-ability state :runner fc 1)
      (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner scored")
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))

(deftest gene-conditioning-shoppe
  "Gene Conditioning Shoppe - set :genetics-trigger-twice flag"
  (do-game
   (new-game (default-corp [(qty "Hedge Fund" 3)])
             (default-runner [(qty "Gene Conditioning Shoppe" 1)
                              (qty "Adjusted Chronotype" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Adjusted Chronotype")
   (let [adjusted-chronotype (get-in @state [:runner :rig :resource 0])]
     (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice)))
     (play-from-hand state :runner "Gene Conditioning Shoppe")
     (is (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))
     (core/trash state :runner (get-in @state [:runner :rig :resource 1]))
     (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))))))

(deftest gene-conditioning-shoppe-redundancy
  "Gene Conditioning Shoppe - set :genetics-trigger-twice flag - ensure redundant copies work"
  (do-game
   (new-game (default-corp [(qty "Hedge Fund" 3)])
             (default-runner [(qty "Gene Conditioning Shoppe" 2)
                              (qty "Adjusted Chronotype" 1)]))
   (take-credits state :corp)
   (take-credits state :runner)
   (take-credits state :corp)
   (play-from-hand state :runner "Adjusted Chronotype")
   (let [adjusted-chronotype (get-in @state [:runner :rig :resource 0])]
     (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice)))
     (play-from-hand state :runner "Gene Conditioning Shoppe")
     (play-from-hand state :runner "Gene Conditioning Shoppe")
     (let [gcs1 (get-in @state [:runner :rig :resource 1])
           gcs2 (get-in @state [:runner :rig :resource 2])]
       (is (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))
       (core/trash state :runner gcs1)
       (is (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))
       (core/trash state :runner gcs2)
       (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice)))))))

(deftest globalsec-security-clearance
  "Globalsec Security Clearance - Ability, click lost on use"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Globalsec Security Clearance" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :link 2)
    (play-from-hand state :runner "Globalsec Security Clearance")
    (take-credits state :runner)
    (starting-hand state :corp ["Hedge Fund"]) ; Hedge Fund on top
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [gsec (-> (get-runner) :rig :resource first)]
      (card-ability state :runner gsec 0)
      (is (pos? (.indexOf (-> (get-runner) :prompt first :msg) "Hedge Fund")) "GSec revealed Hedge Fund")
      (core/end-phase-12 state :runner nil)
      (is (= 3 (:click (get-runner))) "Runner lost 1 click from Globalsec Security Clearance"))))

(deftest grifter
  "Grifter - Gain 1c if you made a successful run this turn, otherwise trash it"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Grifter" 1)]))
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

(deftest hard-at-work
  "Hard at Work - Gain 2c and lose 1 click when turn begins"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Hard at Work" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Hard at Work")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Gained 2c")
    (is (= 3 (:click (get-runner))) "Lost 1 click")))

(deftest ice-carver
  "Ice Carver - lower ice strength on encounter"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1)])
              (default-runner [(qty "Ice Carver" 1)]))
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp 2)
    (let [iwall (get-ice state :archives 0)]
      (core/rez state :corp iwall)
      (play-from-hand state :runner "Ice Carver")
      (run-on state "Archives")
      (is (= 0 (:current-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest investigative-journalism
  "Investigative Journalism - 4 clicks and trash to give the Corp 1 bad pub"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Investigative Journalism" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Investigative Journalism")
    (is (empty? (get-in @state [:runner :rig :resource])) "Corp has no bad pub, couldn't install")
    (core/gain state :corp :bad-publicity 1)
    (play-from-hand state :runner "Investigative Journalism")
    (take-credits state :runner)
    (take-credits state :corp)
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 0 (:click (get-runner))) "Spent 4 clicks")
    (is (= 1 (count (:discard (get-runner)))) "IJ is trashed")
    (is (= 2 (:bad-publicity (get-corp))) "Corp took 1 bad publicity")))

(deftest john-masanori
  "John Masanori - Draw 1 card on first successful run, take 1 tag on first unsuccessful run"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "John Masanori" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Fall Guy" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "John Masanori")
    (is (= 4 (count (:hand (get-runner)))))
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

(deftest joshua-b
  "Joshua B. - Take 1 tag at turn end if you choose to gain the extra click"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Joshua B." 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Joshua B.")
    (take-credits state :runner)
    (take-credits state :corp)
    (prompt-choice :runner "Yes") ; gain the extra click
    (is (= 5 (:click (get-runner))) "Gained extra click")
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Took 1 tag")))

(deftest kati-jones
  "Kati Jones - Click to store and take"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Kati Jones" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (is (= 3 (:credit (get-runner))))
    (let [kati (get-in @state [:runner :rig :resource 0])]
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

(deftest london-library
  "Install non-virus programs on London library. Includes #325/409"
  (do-game
    (new-game (default-corp) (default-runner [(qty "London Library" 1) (qty "Darwin" 1) (qty "Study Guide" 1)
                                              (qty "Chameleon" 1) (qty "Femme Fatale" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "London Library")
    (let [lib (get-in @state [:runner :rig :resource 0])]
      (is (= 0 (count (:hosted (refresh lib)))) "0 programs hosted")
      (card-ability state :runner lib 0) ; Install a non-virus program on London Library
      (prompt-select :runner (find-card "Femme Fatale" (:hand (get-runner))))
      (prompt-choice :runner "Done") ; Cancel out of Femme's bypass
      (is (= 1 (count (:hosted (refresh lib)))) "1 program hosted")
      (card-ability state :runner lib 0)
      (prompt-select :runner (find-card "Study Guide" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (let [sg (second (:hosted (refresh lib)))]
        (is (= 0 (:current-strength (refresh sg))) "Study Guide at 0 strength")
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
      (is (= 0 (count (:discard (get-runner)))) "Nothing in archives yet")
      (take-credits state :runner)
      (is (= 0 (count (:hosted (refresh lib)))) "All programs trashed when turn ends")
      (is (= 2 (count (:hand (get-runner)))) "Darwin never got played, Chameleon returned to hand")
      (is (= 2 (count (:discard (get-runner)))) "Femme Fatale and Study Guide trashed"))))

(deftest muertos-trashed
  "Muertos Gang Member - Install and Trash"
  (do-game
    (new-game (default-corp [(qty "Tollbooth" 1) (qty "Ice Wall" 1)])
              (default-runner [(qty "Hedge Fund" 3) (qty "Muertos Gang Member" 1)]))
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
      (let [muer (get-in @state [:runner :rig :resource 0])]
        (card-ability state :runner muer 0)
        (is (= 3 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
        (prompt-select :corp toll)
        (is (:rezzed (refresh toll)) "Tollbooth was rezzed")))))

(deftest muertos-reina
  "Muertos Gang Member - Account for Reina interaction, #1098."
  (do-game
    (new-game (default-corp [(qty "Tollbooth" 1) (qty "Ice Wall" 1)])
              (make-deck "Reina Roja: Freedom Fighter" [(qty "Hedge Fund" 3)
                                                        (qty "Muertos Gang Member" 1)]))
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
      (let [muer (get-in @state [:runner :rig :resource 0])]
        (card-ability state :runner muer 0)
        (is (= 3 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
        (prompt-select :corp toll)
        (is (:rezzed (refresh toll)) "Tollbooth was rezzed")
        (is (= 0 (:credit (get-corp))) "Corp has 0 credits")))))

(deftest new-angeles-city-hall
  "New Angeles City Hall - Avoid tags; trash when agenda is stolen"
  (do-game
    (new-game (default-corp [(qty "SEA Source" 1) (qty "Breaking News" 1)])
              (default-runner [(qty "New Angeles City Hall" 1)]))
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "New Angeles City Hall")
    (let [nach (get-in @state [:runner :rig :resource 0])]
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (is (= 6 (:credit (get-runner))))
      (play-from-hand state :corp "SEA Source")
      (prompt-choice :corp 0) ; default trace
      (prompt-choice :runner 0) ; Runner won't match
      (card-ability state :runner nach 0)
      (prompt-choice :runner "Done")
      (is (= 0 (:tag (get-runner))) "Avoided SEA Source tag")
      (is (= 4 (:credit (get-runner))) "Paid 2 credits")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (is (empty? (get-in @state [:runner :rig :resource])) "NACH trashed by agenda steal"))))

(deftest patron
  "Patron - Ability"
  (do-game
    (new-game (default-corp [(qty "Jackson Howard" 1)])
              (default-runner [(qty "Patron" 4) (qty "Easy Mark" 4)]))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Patron")
    (let [p (get-in @state [:runner :rig :resource 0])]
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

(deftest patron-manual
  "Patron - Manually selecting during Step 1.2 does not show a second prompt at start of turn. Issue #1744."
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
      (core/end-phase-12 state :runner nil)
      (prompt-choice :runner "No")
      (is (empty? (:prompt (get-runner))) "No second prompt for Patron"))))

(deftest professional-contacts
  "Professional Contacts - Click to gain 1 credit and draw 1 card"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Professional Contacts" 3)
                               (qty "Sure Gamble" 2)
                               (qty "Shiv" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Professional Contacts")
    (let [proco (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner proco 0)
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (:credit (get-runner))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-runner)))) "Drew 1 card")
      (card-ability state :runner proco 0)
      (is (= 1 (:click (get-runner))) "Spent 1 click")
      (is (= 2 (:credit (get-runner))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-runner)))) "Drew 1 card"))))

(deftest rolodex
  "Rolodex - Full test"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Rolodex" 1) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :runner ["Rolodex"])
    (is (= 1 (count (:hand (get-runner)))))
    (take-credits state :corp)
    (play-from-hand state :runner "Rolodex")
    (prompt-choice :runner (find-card "Sure Gamble" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Desperado" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Diesel" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Corroder" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Patron" (:deck (get-runner))))
    ;try starting over
    (prompt-choice :runner "Start over")
    (prompt-choice :runner (find-card "Patron" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Corroder" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Diesel" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Desperado" (:deck (get-runner))))
    (prompt-choice :runner (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
    (prompt-choice :runner "Done")
    (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
    (is (= "Desperado" (:title (second (:deck (get-runner))))))
    (is (= "Diesel" (:title (second (rest (:deck (get-runner)))))))
    (is (= "Corroder" (:title (second (rest (rest (:deck (get-runner))))))))
    (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-runner)))))))))
    (core/trash state :runner (get-resource state 0))
    (is (= 4 (count (:discard (get-runner)))) "Rolodex mills 3 cards when trashed")
    (is (= "Corroder" (:title (first (:deck (get-runner))))))))

(deftest sacrificial-construct
  "Sacrificial Construct - Trash to prevent trash of installed program or hardware"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Sacrificial Construct" 2) (qty "Cache" 1)
                               (qty "Motivation" 1) (qty "Astrolabe" 1)]))
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
    (is (= 1 (count (get-in @state [:runner :rig :program]))) "Cache still installed")
    (core/trash state :runner (get-hardware state 0))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 3 (count (:discard (get-runner)))) "Sac Con trashed")
    (is (= 1 (count (get-in @state [:runner :rig :hardware]))) "Astrolabe still installed")))

(deftest safety-first
  "Safety First - Reduce hand size by 2, draw 1 at turn end if below maximum"
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

(deftest salsette-slums
  "Salsette Slums - Once per turn, when the trash cost of a card is paid, optionally remove from the game"
  (do-game
    (new-game (default-corp [(qty "Hostile Infrastructure" 1) (qty "Tech Startup" 1) (qty "Thomas Haas" 1)
                             (qty "Hedge Fund" 3)])
              (default-runner [(qty "Salsette Slums" 2) (qty "Sure Gamble" 3)]))
    ; Use Hostile Infrastructure to ensure on-trash effects don't fire.
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
      ; Only able to use the ability once per turn
      (card-ability state :runner salsette1 0)
      (is (not (empty? (:prompt (get-runner)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      ; Can't use the ability if you can't afford to trash
      (card-ability state :runner salsette2 0)
      (is (not (empty? (:prompt (get-runner)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      (prompt-choice :runner "No")
      ; Test the "oops I forgot" ability (runner feels bad that they forgot to use Slums when a Hostile is out)
      (run-empty-server state :remote3)
      (prompt-choice :runner "Yes")
      ; Can only use that first Slums once
      (card-ability state :runner salsette1 1)
      (is (empty? (:prompt (get-runner))) "Not prompting the runner")
      (is (not (= (:cid th3) (:cid (last (:rfg (get-corp)))))) "Card was not removed from the game")
      (card-ability state :runner salsette2 1)
      (is (not (empty? (:prompt (get-runner)))) "Prompting the runner to choose a card")
      (prompt-select :runner (find-card "Thomas Haas" (:discard (get-corp))))
      (is (= (:cid th3) (:cid (last (:rfg (get-corp))))) "Card was removed from the game"))
    ; Set things up so we can trash the Hostile and then make sure we can't "oops I forgot on a later turn"
    (core/gain state :runner :credit 5)
    (run-empty-server state :remote2)
    (prompt-choice :runner "Yes")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [salsette1 (get-resource state 0)
          hostile2 (get-content state :remote2 0)]
      (card-ability state :runner salsette1 1)
      (prompt-select :runner (find-card "Hostile Infrastructure" (:discard (get-corp))))
      (is (not (= (:cid hostile2) (:cid (last (:rfg (get-corp)))))) "Did not remove card from game"))
    ))

(deftest security-testing
  "Security Testing - Ability"
  (do-game
    (new-game (default-corp [(qty "Jackson Howard" 1)])
              (default-runner [(qty "Security Testing" 1)]))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Security Testing")
    (let [st (get-in @state [:runner :rig :resource 0])]
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

(deftest security-testing-multiple
  "Security Testing - multiple copies"
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
    (is (= 11 (:credit (get-runner))))))

(deftest spoilers
  "Spoilers - Mill the Corp when it scores an agenda"
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1) (qty "Hedge Fund" 1)])
              (default-runner [(qty "Spoilers" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Spoilers")
    (take-credits state :runner)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (is (= 1 (count (:deck (get-corp)))))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :corp ht)
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 0 (count (:deck (get-corp)))) "Last card from R&D milled"))))

(deftest stim-dealer
  "Stim Dealer - Take 1 brain damage when it accumulates 2 power counters"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Stim Dealer" 1) (qty "Sure Gamble" 1) (qty "Feedback Filter" 1)]))
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
      (is (= 0 (get-counters (refresh sd) :power)) "Lost all counters")
      (is (empty? (:prompt (get-runner))) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
      (is (= 4 (:click (get-runner))) "Didn't gain extra click"))))

(deftest street-peddler-ability
  "Street Peddler - Ability"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Street Peddler" 1)
                               (qty "Gordian Blade" 1)
                               (qty "Torch" 1)
                               (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
      (card-ability state :runner sp 0)
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:runner :rig :program 0])))
          "Gordian Blade was installed")
      (is (= 3 (:memory (get-runner))) "Gordian cost 1 mu"))))

(deftest street-peddler-cant-afford
  "Street Peddler - Can't afford install"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Street Peddler" 1) (qty "Gordian Blade" 3)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler"])
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner sp 0)
      (core/lose state :runner :credit 3)
      (is (= 2 (count (:choices (first (:prompt (get-runner))))))
          "1 card and 1 cancel option on Street Peddler")
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (zero? (count (get-in @state [:runner :rig :program])))
          "Gordian Blade was not installed")
      (is (and (:installed (refresh sp)) (= 3 (count (:hosted (refresh sp))))
               "Street Peddler still installed with 3 hosted cards")))))

(deftest street-peddler-kate-discount
  "Street Peddler - Interaction with Kate discount"
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Street Peddler" 1)
                                                                   (qty "Gordian Blade" 1)
                                                                   (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler"])
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      ;; should still be able to afford Gordian w/ Kate discount
      (core/lose state :runner :credit 3)
      (card-ability state :runner sp 0)
      (is (= 2 (count (:choices (first (:prompt (get-runner))))))
          "Only 1 choice (plus Cancel) to install off Peddler")
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:runner :rig :program 0])))
          "Gordian Blade was installed")
      (is (= 3 (:memory (get-runner))) "Gordian cost 1 mu"))))

(deftest street-peddler-memory-units
  "Street Peddler - Programs Should Cost Memory. Issue #708"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Street Peddler" 1) (qty "Corroder" 3)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler"])
    (play-from-hand state :runner "Street Peddler")
    (is (= 4 (:memory (get-runner))) "No memory cost for hosting on Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
      (card-ability state :runner sp 0)
      (prompt-card :runner (first (:hosted sp))) ; choose to install Gordian
      (is (= "Corroder" (:title (get-in @state [:runner :rig :program 0])))
          "Corroder was installed")
      (is (= 3 (:memory (get-runner))) "Corroder cost 1 mu"))))

(deftest street-peddler-in-play-effects
  "Street Peddler - Trashing hardware should not reduce :in-play values"
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Street Peddler" 1) (qty "HQ Interface" 3)]))
   (take-credits state :corp)
   (starting-hand state :runner ["Street Peddler"])
   (play-from-hand state :runner "Street Peddler")
   (let [sp (get-in @state [:runner :rig :resource 0])]
     (card-ability state :runner sp 0)
     (prompt-card :runner (first (:hosted sp))) ; choose to install HQ Interface
     (is (= 2 (:hq-access (get-runner)))
         "HQ Access increased by 1 from installed HQI and not reduced by the 2 trashed ones"))))

(deftest street-peddler-parasite-1cr
  "Street Peddler - Installing Parasite with only 1cr. Issue #491."
  (do-game
    (new-game (default-corp [(qty "Pop-up Window" 3)])
              (default-runner [(qty "Street Peddler" 1) (qty "Parasite" 3)]))
    (play-from-hand state :corp "Pop-up Window" "HQ")
    (take-credits state :corp 2)
    (starting-hand state :runner ["Street Peddler"])
    (core/lose state :runner :credit 4) ; go down to 1 credit
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])
          pu (get-ice state :hq 0)]
      (core/rez state :corp pu)
      (card-ability state :runner sp 0)
      (prompt-card :runner (first (:hosted sp))) ; choose to install Parasite
      (is (= "Parasite" (:title (:card (first (get-in @state [:runner :prompt])))))
          "Parasite target prompt")
      (prompt-select :runner pu)
      (is (= 4 (count (:discard (get-runner)))) "3 Parasite, 1 Street Peddler in heap")
      (is (= 1 (count (:discard (get-corp)))) "Pop-up Window in archives"))))

(deftest street-peddler-tech-trader
  "Street Peddler - Tech Trader install"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Street Peddler" 1)
                               (qty "Tech Trader" 1)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler"])
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= 1 (count (:hosted sp))) "Street Peddler is hosting 1 card")
      (card-ability state :runner sp 0)
      (prompt-card :runner (find-card "Tech Trader" (:hosted sp))) ; choose to install Tech Trader
      (is (= "Tech Trader" (:title (get-in @state [:runner :rig :resource 0])))
          "Tech Trader was installed")
      (is (= 5 (:credit (get-runner))) "Did not gain 1cr from Tech Trader ability"))))

(deftest symmetrical-visage
  "Symmetrical Visage - Gain 1 credit the first time you click to draw each turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Symmetrical Visage" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Fall Guy" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Symmetrical Visage")
    (is (= 3 (:credit (get-runner))))
    (core/click-draw state :runner nil)
    (is (= 4 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
    (core/click-draw state :runner nil)
    (is (= 4 (:credit (get-runner))) "No credit gained from second click spent to draw")))

(deftest symmetrical-visage-gcs
  "Symmetrical Visage - Gain 1 credit the first and second time you click to draw each turn when GCS is installed"
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Symmetrical Visage" 3)
                              (qty "Gene Conditioning Shoppe" 3)
                              (qty "Fall Guy" 1)]))
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
   ; Move Fall Guy back to deck
   (core/move state :runner (find-card "Fall Guy" (:hand (get-runner))) :deck)
   (core/click-draw state :runner nil)
   (is (= 3 (:credit (get-runner)))
       "No credit gained from third click spent to draw with Gene Conditioning Shoppe")))

(deftest synthetic-blood
  "Synthetic Blood - The first time you take damage each turn, draw one card"
  (do-game
   (new-game (default-corp [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-runner [(qty "Synthetic Blood" 3)
                              (qty "Sure Gamble" 3)
                              (qty "Fall Guy" 1)]))
   (play-from-hand state :corp "Data Mine" "HQ")
   (play-from-hand state :corp "Data Mine" "HQ")
   (take-credits state :corp)
   (let [first-dm (get-ice state :hq 1)
         second-dm (get-ice state :hq 0)]
     (play-from-hand state :runner "Synthetic Blood")
     (run-on state "HQ")
     (core/rez state :corp first-dm)
     (card-ability state :corp first-dm 0)
     (is (= 4 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
     (run-continue state)
     (core/rez state :corp second-dm)
     (card-ability state :corp second-dm 0)
     (is (= 3 (count (:hand (get-runner)))) "no card drawn when receiving damage (2nd time)"))))

(deftest synthetic-blood-gcs
  "Synthetic Blood - The first and second time you take damage each turn (with GCS installed), draw one card"
  (do-game
   (new-game (default-corp [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-runner [(qty "Synthetic Blood" 3)
                              (qty "Sure Gamble" 1)
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
     (card-ability state :corp first-dm 0)
     (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
     (run-continue state)
     (core/rez state :corp second-dm)
     (card-ability state :corp second-dm 0)
     (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (2nd time)"))))

(deftest technical-writer
  "Technical Writer - Gain 1c per program/hardware install; click/trash to take all credits"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Technical Writer" 1) (qty "Faerie" 2)
                               (qty "Vigil" 1) (qty "Same Old Thing" 1)]))
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
      (is (= 0 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (count (:discard (get-runner)))) "Technical Writer trashed"))))

(deftest the-helpful-ai
  "The Helpful AI - +1 link; trash to give an icebreaker +2 str until end of turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "The Helpful AI" 1) (qty "Corroder" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Helpful AI")
    (is (= 1 (:link (get-runner))) "Gained 1 link")
    (play-from-hand state :runner "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-select :runner corr)
      (is (= 4 (:current-strength (refresh corr))) "Corroder has +2 strength")
      (is (= 1 (count (:discard (get-runner)))) "Helpful AI trashed")
      (is (= 0 (:link (get-runner))))
      (take-credits state :runner)
      (is (= 2 (:current-strength (refresh corr))) "Corroder back to default strength"))))

(deftest the-source
  "The Source - Increase advancement requirement of agendas by 1; 3c additional cost to steal"
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 2)])
              (default-runner [(qty "The Source" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "The Source")
    (run-empty-server state :remote1)
    (prompt-choice :runner "Yes") ; pay 3c extra to steal
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

(deftest the-supplier-ability
  "The Supplier - Ability"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "The Supplier" 1)
                               (qty "Plascrete Carapace" 1)
                               (qty "Utopia Shard" 1)
                               (qty "Hedge Fund" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Supplier")
    (let [ts (get-in @state [:runner :rig :resource 0])]
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

(deftest the-supplier-kate-discount
  "The Supplier - Interaction with Kate discount. Issue #578."
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         [(qty "The Supplier" 1)
                          (qty "Plascrete Carapace" 1)
                          (qty "Kati Jones" 1)
                          (qty "Hedge Fund" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Supplier")
    (let [ts (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner ts 0)
      (prompt-select :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
      (core/lose state :runner :credit (:credit (get-runner)))
      (core/end-turn state :runner nil)
      (take-credits state :corp)
      (prompt-select :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 0 (:credit (get-runner))) "Kate discount applied")
      (is (= 1 (count (get-in @state [:runner :rig :resource]))) "Plascrete installed"))))

(deftest tech-trader
  "Basic test"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Tech Trader" 1) (qty "Fall Guy" 1)]))

    (take-credits state :corp)
    (play-from-hand state :runner "Tech Trader")
    (play-from-hand state :runner "Fall Guy")
    (is (= 4 (:credit (get-runner))))

    (let [fall (get-in @state [:runner :rig :resource 1])]
      (card-ability state :runner fall 1)
      (is (= 7 (:credit (get-runner)))))))

(deftest the-black-file
  "The Black File - Prevent Corp from winning by agenda points"
  (do-game
    (new-game (default-corp [(qty "Vanity Project" 3) (qty "Sure Gamble" 3)])
              (default-runner [(qty "The Black File" 1)]))
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
      (is (= 1 (get-in (refresh bf) [:counter :power])) "1 power counter on The Black File")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-in (refresh bf) [:counter :power])) "2 power counters on The Black File")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (count (:rfg (get-runner)))) "The Black File removed from the game")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Agenda" (:reason @state)) "Win condition reports agendas"))))

(deftest the-black-file-flatline
  "The Black File - Corp can still win by flatlining Runner"
  (do-game
    (new-game (default-corp [(qty "Vanity Project" 3) (qty "Scorched Earth" 3)])
              (default-runner [(qty "The Black File" 1)]))
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
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest tri-maf-contact
  "Tri-maf Contact - Click for 2c once per turn; take 3 meat dmg when trashed"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Tri-maf Contact" 2) (qty "Cache" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Tri-maf Contact")
    (let [tmc (get-resource state 0)]
      (card-ability state :runner tmc 0)
      (is (= 5 (:credit (get-runner))) "Gained 2c")
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (card-ability state :runner tmc 0)
      (is (= 5 (:credit (get-runner))) "No credits gained; already used this turn")
      (take-credits state :runner)
      (core/trash state :runner tmc)
      (is (= 4 (count (:discard (get-runner)))) "Took 3 meat damage"))))

(deftest virus-breeding-ground-gain
  "Virus Breeding Ground - Gain counters"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Virus Breeding Ground" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (let [vbg (get-in @state [:runner :rig :resource 0])]
      (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh vbg) :virus))
          "Virus Breeding Ground gains 1 counter per turn"))))

(deftest virus-breeding-ground-gain
  "Virus Breeding Ground - Move counters"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Hivemind")
    (let [hive (get-in @state [:runner :rig :program 0])
          vbg (get-in @state [:runner :rig :resource 0])]
      (is (= 1 (get-counters hive :virus)) "Hivemind starts with 1 counter")
      (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
      (card-ability state :runner vbg 0)
      (prompt-select :runner hive)
      (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
      (is (= 0 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))

(deftest wasteland
  "Wasteland - Gain 1c the first time you trash an installed card each turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Wasteland" 1) (qty "Fall Guy" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Wasteland")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Fall Guy")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 1 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 6 (:credit (get-runner))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (take-credits state :runner)
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 2 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 9 (:credit (get-runner))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 3 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 11 (:credit (get-runner))) "Gained 2c from Fall Guy but no credits from Wasteland")))

(deftest xanadu
  "Xanadu - Increase all ICE rez cost by 1 credit"
  (do-game
    (new-game (default-corp [(qty "Paper Wall" 2) (qty "Launch Campaign" 1)])
              (default-runner [(qty "Xanadu" 1)]))
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
  "Zona Sul Shipping - Gain 1c per turn, click to take all credits. Trash when tagged"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Zona Sul Shipping" 1)]))
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
