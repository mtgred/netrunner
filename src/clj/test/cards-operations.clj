(in-ns 'test.core)

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

(deftest hedge-fund
  (do-game
    (new-game (default-corp) (default-runner))
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Hedge Fund")
    (is (= 9 (:credit (get-corp))))))

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

(deftest subliminal-messaging
  "Subliminal Messaging - Playing/trashing/milling all will prompt returning to hand"
  (do-game
    (new-game (default-corp [(qty "Subliminal Messaging" 3)])
              (make-deck "Noise: Hacker Extraordinaire" [(qty "Cache" 3) (qty "Utopia Shard" 1)]))
    (play-from-hand state :corp "Subliminal Messaging")
    (is (= 6 (:credit (get-corp))))
    (is (= 3 (:click (get-corp))) "First Subliminal Messaging gains 1 click")
    (play-from-hand state :corp "Subliminal Messaging")
    (is (= 7 (:credit (get-corp))))
    (is (= 2 (:click (get-corp))) "Second Subliminal Messaging does not gain 1 click")
    (trash-from-hand state :corp "Subliminal Messaging")
    (is (= 0 (count (:hand (get-corp)))) "Corp no more cards in HQ")
    (take-credits state :corp)
    (take-credits state :runner)
    (prompt-choice :corp "Yes")
    (prompt-choice :corp "Yes")
    (prompt-choice :corp "Yes")
    (is (= 3 (count (:hand (get-corp)))) "All 3 Subliminals returned to HQ")
    (core/move state :corp (find-card "Subliminal Messaging" (:hand (get-corp))) :deck)
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Utopia Shard")
    (let [utopia (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner utopia 0))
    (take-credits state :runner)
    (prompt-choice :corp "Yes")
    (prompt-choice :corp "Yes")
    (prompt-choice :corp "Yes")
    (is (= 3 (count (:hand (get-corp)))) "All 3 Subliminals returned to HQ")
    (play-from-hand state :corp "Subliminal Messaging")
    (take-credits state :corp)
    (run-on state "R&D")
    (run-jack-out state)
    (take-credits state :runner)
    (take-credits state :corp) ; There should not be a prompt here because runner made a run last turn
    (is (= 2 (count (:hand (get-corp)))) "2 Subliminals in HQ")
    (is (= 1 (count (:discard (get-corp)))) "1 Subliminal not returned because runner made a run last turn")))

(deftest subliminal-messaging-no
  "Subliminal Messaging - User declines to return to hand, ensure game asks again next turn"
  (do-game
    (new-game (default-corp [(qty "Subliminal Messaging" 2)])
              (default-runner))
    (play-from-hand state :corp "Subliminal Messaging")
    (trash-from-hand state :corp "Subliminal Messaging")
    (take-credits state :corp)
    (take-credits state :runner)
    (prompt-choice :corp "No")
    (prompt-choice :corp "No")
    (is (= 0 (count (:hand (get-corp)))) "Neither Subliminal returned to HQ")
    (is (= 2 (count (:discard (get-corp)))) "Both Subliminals in Archives")
    (take-credits state :corp)
    (take-credits state :runner)
    (prompt-choice :corp "Yes")
    (prompt-choice :corp "Yes")
    (is (= 2 (count (:hand (get-corp)))) "Both Subliminals returned to HQ")
    (is (= 0 (count (:discard (get-corp)))) "No Subliminals in Archives")))

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
