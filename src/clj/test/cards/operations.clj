(ns test.cards.operations
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest accelerated-diagnostics
  "Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan"
  (do-game
    (new-game (default-corp [(qty "Accelerated Diagnostics" 1) (qty "Cerebral Overwriter" 1) (qty "Shipment from SanSan" 1)
                             (qty "Hedge Fund" 1) (qty "Back Channels" 1)])
              (default-runner))
    (starting-hand state :corp ["Accelerated Diagnostics" "Cerebral Overwriter"])
    (play-from-hand state :corp "Cerebral Overwriter" "New remote")
    (core/gain state :corp :credit 1)
    (play-from-hand state :corp "Accelerated Diagnostics")

    (let [playarea (get-in @state [:corp :play-area])
          hf (find-card "Hedge Fund" playarea)
          ss (find-card "Shipment from SanSan" playarea)
          bc (find-card "Back Channels" playarea)
          co (get-content state :remote1 0)]
      (is (= 3 (count playarea)) "3 cards in play area")
      (prompt-select :corp ss)
      (prompt-choice :corp "2")
      (prompt-select :corp co)
      (is (= 2 (:advance-counter (refresh co))) "Cerebral Overwriter gained 2 advancements")
      (prompt-select :corp hf)
      (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")
      (prompt-select :corp bc)
      (prompt-select :corp (refresh co))
      (is (= 15 (:credit (get-corp))) "Corp gained 6 credits for Back Channels"))))


(deftest big-brother
  "Big Brother - Give the Runner 2 tags if already tagged"
  (do-game
    (new-game (default-corp [(qty "Big Brother" 1)])
              (default-runner))
    (play-from-hand state :corp "Big Brother")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Big Brother")
    (is (= 3 (:tag (get-runner))) "Runner gained 2 tags")))

(deftest biotic-labor
  "Biotic Labor - Gain 2 clicks"
  (do-game
    (new-game (default-corp [(qty "Biotic Labor" 1)])
              (default-runner))
    (play-from-hand state :corp "Biotic Labor")
    (is (= 1 (:credit (get-corp))))
    (is (= 4 (:click (get-corp))) "Spent 1 click to gain 2 additional clicks")))

(deftest blue-level-clearance
  "Blue Level Clearance - Gain 5 credits and draw 2 cards"
  (do-game
    (new-game (default-corp [(qty "Blue Level Clearance" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Sweeps Week" 2)])
              (default-runner))
    (play-from-hand state :corp "Blue Level Clearance")
    (is (= 8 (:credit (get-corp))) "Gained 5 credits")
    (is (= 1 (:click (get-corp))))
    (is (= 7 (count (:hand (get-corp)))) "Drew 2 cards")))

(deftest casting-call
  "Casting Call - Only do card-init on the Public agendas.  Issue #1128"
  (do-game
    (new-game (default-corp [(qty "Casting Call" 2) (qty "Oaktown Renovation" 1)
                             (qty "Improved Tracers" 1) (qty "Hunter" 1)])
              (default-runner))
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Hunter" "HQ")
    (let [hunter (get-ice state :hq 0)]
      (core/rez state :corp hunter)
      (is (= 4 (:current-strength (refresh hunter))))
      (play-from-hand state :corp "Casting Call")
      (prompt-select :corp (find-card "Improved Tracers" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (let [imptrac (get-content state :remote1 0)]
        (is (get-in (refresh imptrac) [:rezzed]) "Improved Tracers is faceup")
        (is (= 4 (:current-strength (refresh hunter))) "Hunter hasn't gained strength")
        (play-from-hand state :corp "Casting Call")
        (prompt-select :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (prompt-choice :corp "New remote")
        (let [oak (get-content state :remote2 0)]
          (core/advance state :corp {:card (refresh oak)})
          (is (= 5 (:credit (get-corp))) "Events on Public agenda work; gained 2 credits from advancing")
          (take-credits state :corp)
          (run-empty-server state "Server 2")
          (prompt-select :runner oak)
          (prompt-choice :runner "Steal")
          (is (= 2 (:tag (get-runner))) "Runner took 2 tags from accessing agenda with Casting Call hosted on it"))))))

(deftest cerebral-static-chaos-theory
  "Cerebral Static - vs Chaos Theory"
  (do-game
    (new-game (default-corp [(qty "Cerebral Static" 1) (qty "Lag Time" 1)])
              (make-deck "Chaos Theory: Wünderkind" [(qty "Sure Gamble" 3)]))
    (is (= 5 (:memory (get-runner))) "CT starts with 5 memory")
    (play-from-hand state :corp "Cerebral Static")
    (is (= 4 (:memory (get-runner))) "Cerebral Static causes CT to have 4 memory")
    (play-from-hand state :corp "Lag Time")
    (is (= 5 (:memory (get-runner))) "CT 5 memory restored")))

(deftest closed-accounts
  "Closed Accounts - Play if Runner is tagged to make Runner lose all credits"
  (do-game
    (new-game (default-corp [(qty "Closed Accounts" 1)])
              (default-runner))
    (play-from-hand state :corp "Closed Accounts")
    (is (and (= 3 (:click (get-corp)))
             (= 5 (:credit (get-runner))))
        "Closed Accounts precondition not met; card not played")
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Closed Accounts")
    (is (= 0 (:credit (get-runner))) "Runner lost all credits")))

(deftest consulting-visit
  "Consulting Visit - Only show single copies of operations corp can afford as choices. Play chosen operation"
  (do-game
    (new-game (default-corp [(qty "Consulting Visit" 1) 
                             (qty "Beanstalk Royalties" 2)
                             (qty "Green Level Clearance" 1)
                             (qty "Breaking News" 1)
                             (qty "Hedge Fund" 1)])
              (default-runner))
    (is (= 5 (:credit (get-corp))))
    (starting-hand state :corp ["Consulting Visit"])
    (play-from-hand state :corp "Consulting Visit")

    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]

      (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
      (prompt-choice :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
      (is (= 6 (:credit (get-corp)))))))

(deftest consulting-visit-mumbad
  "Consulting Visit - Works properly when played with Mumbad City Hall"
  (do-game
    (new-game (default-corp [(qty "Mumbad City Hall" 1)
                             (qty "Beanstalk Royalties" 1)
                             (qty "Green Level Clearance" 1)
                             (qty "Breaking News" 1)
                             (qty "Hedge Fund" 1)
                             (qty "Consulting Visit" 1)
                             (qty "Mumba Temple" 1)])
              (default-runner))
    (is (= 5 (:credit (get-corp))))
    (starting-hand state :corp ["Mumbad City Hall"])
    (play-from-hand state :corp "Mumbad City Hall" "New remote")

    (let [hall (get-content state :remote1 0)
          get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]

      (card-ability state :corp hall 0)
      (is (= (list "Consulting Visit" "Mumba Temple" nil) (prompt-names)))

      (prompt-choice :corp (find-card "Consulting Visit" (:deck (get-corp))))
      (is (= 3 (:credit (get-corp))))
      (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))

      (prompt-choice :corp (find-card "Green Level Clearance" (:deck (get-corp))))
      (is (= 5 (:credit (get-corp)))))))

(deftest defective-brainchips
  "Defective Brainchips - Do 1 add'l brain damage the first time Runner takes some each turn"
  (do-game
    (new-game (default-corp [(qty "Defective Brainchips" 1) (qty "Viktor 1.0" 1)])
              (default-runner [(qty "Sure Gamble" 2) (qty "Shiv" 2)]))
    (play-from-hand state :corp "Defective Brainchips")
    (play-from-hand state :corp "Viktor 1.0" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [vik (get-ice state :hq 0)]
      (core/rez state :corp vik)
      (card-ability state :corp vik 0)
      (is (= 2 (count (:discard (get-runner)))) "2 cards lost to brain damage")
      (is (= 2 (:brain-damage (get-runner))) "Brainchips dealt 1 additional brain dmg")
      (card-ability state :corp vik 0)
      (is (= 3 (count (:discard (get-runner)))) "2 cards lost to brain damage")
      (is (= 3 (:brain-damage (get-runner))) "Brainchips didn't do additional brain dmg"))))

(deftest diversified-portfolio
  (do-game
    (new-game (default-corp [(qty "Diversified Portfolio" 1)
                             (qty "Paper Wall" 1)
                             (qty "PAD Campaign" 3)])
              (default-runner))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Paper Wall" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Diversified Portfolio")
    (is (= 7 (:credit (get-corp))) "Ignored remote with ICE but no server contents")))

(deftest exchange-of-information
  "Exchange of Information - Swapping agendas works correctly"
  (do-game
    (new-game (default-corp [(qty "Exchange of Information" 1) 
                             (qty "Market Research" 1) 
                             (qty "Breaking News" 1) 
                             (qty "Project Beale" 1) 
                             (qty "Explode-a-palooza" 1)])
              (default-runner))

      (score-agenda state :corp (find-card "Market Research" (:hand (get-corp))))
      (score-agenda state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner gained 2 tags")
      (take-credits state :corp)
      (is (= 0 (:tag (get-runner))) "Runner lost 2 tags")

      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (find-card "Explode-a-palooza" (:hand (get-corp))))
      (take-credits state :runner)

      (is (= 4 (:agenda-point (get-runner))))
      (is (= 3 (:agenda-point (get-corp))))

      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Exchange of Information")

      (prompt-select :corp (find-card "Project Beale" (:scored (get-runner))))
      (prompt-select :corp (find-card "Breaking News" (:scored (get-corp))))

      (is (= 3 (:agenda-point (get-runner))))
      (is (= 4 (:agenda-point (get-corp))))))

(deftest exchange-of-information-breaking-news
  "Exchange of Information - Swapping a just scored Breaking News keeps the tags"
  (do-game
    (new-game (default-corp [(qty "Exchange of Information" 1) 
                             (qty "Market Research" 1) 
                             (qty "Breaking News" 1) 
                             (qty "Project Beale" 1) 
                             (qty "Explode-a-palooza" 1)])
              (default-runner))

      (take-credits state :corp)

      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (find-card "Explode-a-palooza" (:hand (get-corp))))
      (take-credits state :runner)

      (score-agenda state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner gained 2 tags")
      (play-from-hand state :corp "Exchange of Information")

      (prompt-select :corp (find-card "Project Beale" (:scored (get-runner))))
      (prompt-select :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 2 (:tag (get-runner))) "Still has tags after swap and before end of turn")

      (take-credits state :corp)
      (is (= 3 (:agenda-point (get-runner))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner does not lose tags at end of turn")))

(deftest election-day
  (do-game
    (new-game (default-corp [(qty "Election Day" 7)])
                (default-runner))
      (is (= 6 (count (:hand (get-corp)))) "Corp starts with 5 + 1 cards")
      (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Election Day" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Election Day")
      (is (= 1 (count (:hand (get-corp)))) "Could not play Election Day")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (count (:hand (get-corp)))) "Corp has now 1 + 1 cards before Election Day")
      (play-from-hand state :corp "Election Day")
      (is (= 5 (count (:hand (get-corp)))) "Corp has now 5 cards due to Election Day")))

(deftest hedge-fund
  (do-game
    (new-game (default-corp) (default-runner))
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Hedge Fund")
    (is (= 9 (:credit (get-corp))))))

(deftest housekeeping
  "Housekeeping - Runner must trash a card from Grip on first install of a turn"
  (do-game
    (new-game (default-corp [(qty "Housekeeping" 1)])
              (default-runner [(qty "Cache" 2) (qty "Fall Guy" 1) (qty "Mr. Li" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Fall Guy")
    (take-credits state :runner)
    (play-from-hand state :corp "Housekeeping")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (prompt-select :runner (find-card "Mr. Li" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "Fall Guy prevention didn't trigger")
    (is (= 1 (count (:discard (get-runner)))) "Card trashed")
    (play-from-hand state :runner "Cache")
    (is (empty? (:prompt (get-runner))) "Housekeeping didn't trigger on 2nd install")))

(deftest lateral-growth
  (do-game
    (new-game (default-corp [(qty "Lateral Growth" 1) (qty "Breaking News" 1)])
              (default-runner))
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Lateral Growth")
    (prompt-select :corp (find-card "Breaking News" (:hand (get-corp))))
    (prompt-choice :corp "New remote")
    (is (= "Breaking News" (:title (get-content state :remote1 0)))
      "Breaking News installed by Lateral Growth")
    (is (= 7 (:credit (get-corp))))))

(deftest midseason-replacements
  "Midseason Replacements - Trace to give Runner tags after they steal an agenda"
  (do-game
    (new-game (default-corp [(qty "Midseason Replacements" 1) (qty "Breaking News" 1)])
              (default-runner))
    (play-from-hand state :corp "Midseason Replacements")
    (is (= 3 (:click (get-corp))) "Midseason precondition not met; Corp not charged a click")
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (let [bn (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (prompt-choice :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Stole Breaking News")
      (take-credits state :runner)
      (play-from-hand state :corp "Midseason Replacements")
      (prompt-choice :corp 0) ; default trace
      (prompt-choice :runner 0) ; Runner won't match
      (is (= 6 (:tag (get-runner))) "Runner took 6 tags"))))

(deftest mushin-no-shin
  "Mushin No Shin - Add 3 advancements to a card; prevent rez/score of that card the rest of the turn"
  (do-game
    (new-game (default-corp [(qty "Mushin No Shin" 2) (qty "Ronin" 1) (qty "Profiteering" 1)])
              (default-runner))
    (play-from-hand state :corp "Mushin No Shin")
    (prompt-select :corp (find-card "Ronin" (:hand (get-corp))))
    (let [ronin (get-content state :remote1 0)]
      (is (= 3 (:advance-counter (refresh ronin))) "3 advancements placed on Ronin")
      (core/rez state :corp (refresh ronin))
      (is (not (get-in (refresh ronin) [:rezzed])) "Ronin did not rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh ronin))
      (is (get-in (refresh ronin) [:rezzed]) "Ronin now rezzed")
      (play-from-hand state :corp "Mushin No Shin")
      (prompt-select :corp (find-card "Profiteering" (:hand (get-corp))))
      (let [prof (get-content state :remote2 0)]
        (core/score state :corp (refresh prof))
        (is (empty? (:scored (get-corp))) "Profiteering not scored")
        (is (= 0 (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/score state :corp (refresh prof))
        (prompt-choice :corp "0")
        (is (= 1 (:agenda-point (get-corp))) "Profiteering was able to be scored")))))

(deftest neural-emp
  "Neural EMP - Play if Runner made a run the previous turn to do 1 net damage"
  (do-game
    (new-game (default-corp [(qty "Neural EMP" 1)])
              (default-runner))
    (play-from-hand state :corp "Neural EMP")
    (is (= 3 (:click (get-corp))) "Neural precondition not met; card not played")
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (take-credits state :runner)
    (play-from-hand state :corp "Neural EMP")
    (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")))

(deftest news-cycle
  (do-game
    (new-game (default-corp [(qty "Breaking News" 2) (qty "24/7 News Cycle" 3)])
              (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-content state :remote1 0)
          ag2 (get-content state :remote2 0)]
      (score-agenda state :corp ag1)
      (score-agenda state :corp ag2)
      (take-credits state :corp)
      (is (= 0 (:tag (get-runner)))) ; tags cleared
      (take-credits state :runner)
      (play-from-hand state :corp "24/7 News Cycle")
      (prompt-card :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))) "Forfeited Breaking News")
      (prompt-select :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner given 2 tags")
      (take-credits state :corp 2)
      (is (= 2 (:tag (get-runner))) "Tags remained after Corp ended turn"))))

(deftest news-cycle-posted-bounty
  "24/7 News Cycle and Posted Bounty interaction -- Issue #1043"
  (do-game
    (new-game (default-corp [(qty "Posted Bounty" 2) (qty "24/7 News Cycle" 3)])
              (default-runner))
    (play-from-hand state :corp "Posted Bounty" "New remote")
    (play-from-hand state :corp "Posted Bounty" "New remote")
    (let [ag1 (get-content state :remote1 0)
          ag2 (get-content state :remote2 0)]
      (score-agenda state :corp ag1)
      (prompt-choice :corp "No")
      (score-agenda state :corp ag2)
      (prompt-choice :corp "No")
      (play-from-hand state :corp "24/7 News Cycle")
      (prompt-card :corp (find-card "Posted Bounty" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))) "Forfeited Posted Bounty")
      (prompt-select :corp (find-card "Posted Bounty" (:scored (get-corp))))
      (prompt-choice :corp "Yes") ; "Forfeit Posted Bounty to give 1 tag?"
      (is (= 1 (:tag (get-runner))) "Runner given 1 tag")
      (is (= 1 (:bad-publicity (get-corp))) "Corp has 1 bad publicity")
      (is (= 0 (:agenda-point (get-corp))) "Forfeited Posted Bounty to 24/7 News Cycle"))))

(deftest oversight-ai
  "Oversight AI - Rez a piece of ICE ignoring all costs"
  (do-game
    (new-game (default-corp [(qty "Oversight AI" 1) (qty "Archer" 1)])
              (default-runner))
    (play-from-hand state :corp "Archer" "R&D")
    (let [archer (get-ice state :rd 0)]
      (play-from-hand state :corp "Oversight AI")
      (prompt-select :corp archer)
      (is (get-in (refresh archer) [:rezzed]))
      (is (= 4 (:credit (get-corp))) "Archer rezzed at no credit cost")
      (is (= "Oversight AI" (:title (first (:hosted (refresh archer)))))
          "Archer hosting OAI as a condition"))))

(deftest paywall-implementation
  "Paywall Implementation - Gain 1 credit for every successful run"
  (do-game
    (new-game (default-corp [(qty "Paywall Implementation" 1)])
              (default-runner))
    (play-from-hand state :corp "Paywall Implementation")
    (is (= "Paywall Implementation" (:title (first (get-in @state [:corp :current]))))
        "Paywall active in Current area")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-empty-server state "Archives")
    (is (= 8 (:credit (get-corp))) "Gained 1 credit from successful run")
    (run-empty-server state "Archives")
    (is (= 9 (:credit (get-corp))) "Gained 1 credit from successful run")))

(deftest peak-efficiency
  "Peak Efficiency - Gain 1 credit for each rezzed ICE"
  (do-game
    (new-game (default-corp [(qty "Peak Efficiency" 1) (qty "Paper Wall" 3) (qty "Wraparound" 1)])
              (default-runner))
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Paper Wall" "New remote")
    (play-from-hand state :corp "Wraparound" "New remote")
    (core/rez state :corp (get-ice state :hq 0))
    (core/rez state :corp (get-ice state :rd 0))
    (core/rez state :corp (get-ice state :remote1 0))
    (play-from-hand state :corp "Peak Efficiency")
    (is (= 7 (:credit (get-corp))) "Gained 3 credits for 3 rezzed ICE; unrezzed ICE ignored")))

(deftest power-shutdown
  "Power Shutdown - Trash cards from R&D to force Runner to trash a program or hardware"
  (do-game
    (new-game (default-corp [(qty "Power Shutdown" 3) (qty "Hive" 3)])
              (default-runner [(qty "Grimoire" 1) (qty "Cache" 1)]))
    (play-from-hand state :corp "Power Shutdown")
    (is (empty? (:discard (get-corp))) "Not played, no run last turn")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Grimoire")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (core/move state :corp (find-card "Hive" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Hive" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Hive" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Power Shutdown")
    (prompt-choice :corp 2)
    (is (= 3 (count (:discard (get-corp)))) "2 cards trashed from R&D")
    (is (= 1 (count (:deck (get-corp)))) "1 card remaining in R&D")
    (prompt-select :runner (get-in @state [:runner :rig :hardware 0])) ; try targeting Grimoire
    (is (empty? (:discard (get-runner))) "Grimoire too expensive to be targeted")
    (prompt-select :runner (get-in @state [:runner :rig :program 0]))
    (is (= 1 (count (:discard (get-runner)))) "Cache trashed")))

(deftest psychographics
  "Psychographics - Place advancements up to the number of Runner tags on a card"
  (do-game
    (new-game (default-corp [(qty "Psychographics" 1) (qty "Project Junebug" 1)])
              (default-runner))
    (core/gain state :runner :tag 4)
    (play-from-hand state :corp "Project Junebug" "New remote")
    (let [pj (get-content state :remote1 0)]
      (play-from-hand state :corp "Psychographics")
      (prompt-choice :corp 4)
      (prompt-select :corp pj)
      (is (= 1 (:credit (get-corp))) "Spent 4 credits")
      (is (= 4 (:advance-counter (refresh pj))) "Junebug has 4 advancements"))))

(deftest reuse
  "Reuse - Gain 2 credits for each card trashed from HQ"
  (do-game
    (new-game (default-corp [(qty "Reuse" 2) (qty "Hive" 1) (qty "IQ" 1)
                             (qty "Ice Wall" 1)])
              (default-runner))
    (play-from-hand state :corp "Reuse")
    (prompt-select :corp (find-card "Ice Wall" (:hand (get-corp))))
    (prompt-select :corp (find-card "Hive" (:hand (get-corp))))
    (prompt-select :corp (find-card "IQ" (:hand (get-corp))))
    (prompt-choice :corp "Done")
    (is (= 4 (count (:discard (get-corp)))) "3 cards trashed plus operation played")
    (is (= 11 (:credit (get-corp))) "Gained 6 credits")
    (is (= 1 (:click (get-corp))) "Spent 2 clicks")))

(deftest scorched-earth
  "Scorched Earth - burn 'em"
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Scorched Earth")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))

(deftest scorched-earth-no-tag
  "Scorched Earth - not tagged"
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (play-from-hand state :corp "Scorched Earth")
    (is (= 3 (:click (get-corp))) "Corp not charged a click")
    (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))

(deftest scorched-earth-flatline
  "Scorched Earth - murderize 'em"
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner))
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Scorched Earth")
    (is (= 0 (count (:hand (get-runner)))) "Runner has 0 cards in hand")
    (is (= :corp (:winner @state)) "Corp wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest subcontract-scorched
  "Subcontract - Don't allow second operation until damage prevention completes"
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 2) (qty "Subcontract" 1)])
              (default-runner [(qty "Plascrete Carapace" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :tag 1)
    (play-from-hand state :runner "Plascrete Carapace")
    (take-credits state :runner)
    (play-from-hand state :corp "Subcontract")
    (prompt-select :corp (find-card "Scorched Earth" (:hand (get-corp))))
    (is (empty? (:prompt (get-corp))) "Corp does not have prompt until damage prevention completes")
    (prompt-choice :runner "Done")
    (is (not-empty (:prompt (get-corp))) "Corp can now play second Subcontract operation")))

(deftest shipment-from-sansan
  "Shipment from SanSan - placing advancements"
  (do-game
    (new-game (default-corp [(qty "Shipment from SanSan" 3) (qty "Ice Wall" 3)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iwall (get-ice state :hq 0)]
      (play-from-hand state :corp "Shipment from SanSan")
      (prompt-choice :corp "2")
      (prompt-select :corp iwall)
      (is (= 5 (:credit (get-corp))))
      (is (= 2 (:advance-counter (refresh iwall)))))))

(deftest successful-demonstration
  "Successful Demonstration - Play if only Runner made unsuccessful run last turn; gain 7 credits"
  (do-game
    (new-game (default-corp [(qty "Successful Demonstration" 1)])
              (default-runner))
    (play-from-hand state :corp "Successful Demonstration")
    (is (and (= 3 (:click (get-corp)))
             (= 5 (:credit (get-runner))))
        "Successful Demonstration precondition not met; card not played")
    (take-credits state :corp)
    (run-on state "R&D")
    (run-jack-out state)
    (take-credits state :runner)
    (play-from-hand state :corp "Successful Demonstration")
    (is (= 13 (:credit (get-corp))) "Paid 2 to play event; gained 7 credits")))
