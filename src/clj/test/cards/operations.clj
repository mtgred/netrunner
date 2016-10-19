(ns test.cards.operations
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest twenty-four-seven-news-cycle-breaking-news
  ;; 24/7 News Cycle - Breaking News interaction
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

(deftest twenty-four-seven-news-cycle-posted-bounty
  ;; 24/7 News Cycle and Posted Bounty interaction -- Issue #1043
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

(deftest twenty-four-seven-news-cycle-swaps
  ;; 24/7 News Cycle - Swapped agendas are able to be used. #1555
  (do-game
    (new-game (default-corp [(qty "24/7 News Cycle" 1) (qty "Chronos Project" 1)
                             (qty "Philotic Entanglement" 1) (qty "Profiteering" 1)])
              (default-runner [(qty "Turntable" 3)]))
    (score-agenda state :corp (find-card "Chronos Project" (:hand (get-corp))))
    (score-agenda state :corp (find-card "Philotic Entanglement" (:hand (get-corp))))
    (take-credits state :corp)
    (play-from-hand state :runner "Turntable")
    (core/steal state :runner (find-card "Profiteering" (:hand (get-corp))))
    (prompt-choice :runner "Yes")
    (prompt-select :runner (find-card "Philotic Entanglement" (:scored (get-corp))))
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 2 (:agenda-point (get-runner))))
    (take-credits state :runner)
    (play-from-hand state :corp "24/7 News Cycle")
    (prompt-card :corp (find-card "Chronos Project" (:scored (get-corp))))
    (is (= "Chronos Project" (:title (first (:rfg (get-corp))))))
    ;; shouldn't work on an agenda in the Runner's scored area
    (is (= 2 (count (:hand (get-runner)))))
    (prompt-select :corp (find-card "Philotic Entanglement" (:scored (get-runner))))
    (is (= 2 (count (:hand (get-runner)))))
    ;; resolve 'when scored' ability on swapped Profiteering
    (is (= 8 (:credit (get-corp))))
    (prompt-select :corp (find-card "Profiteering" (:scored (get-corp))))
    (prompt-choice :corp "3")
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 3 (:bad-publicity (get-corp))))
    (is (= 23 (:credit (get-corp))) "Gained 15 credits")))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
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
  ;; Big Brother - Give the Runner 2 tags if already tagged
  (do-game
    (new-game (default-corp [(qty "Big Brother" 1)])
              (default-runner))
    (play-from-hand state :corp "Big Brother")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Big Brother")
    (is (= 3 (:tag (get-runner))) "Runner gained 2 tags")))

(deftest biotic-labor
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game (default-corp [(qty "Biotic Labor" 1)])
              (default-runner))
    (play-from-hand state :corp "Biotic Labor")
    (is (= 1 (:credit (get-corp))))
    (is (= 4 (:click (get-corp))) "Spent 1 click to gain 2 additional clicks")))

(deftest blue-level-clearance
  ;; Blue Level Clearance - Gain 5 credits and draw 2 cards
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
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
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
  ;; Cerebral Static - vs Chaos Theory
  (do-game
    (new-game (default-corp [(qty "Cerebral Static" 1) (qty "Lag Time" 1)])
              (make-deck "Chaos Theory: Wünderkind" [(qty "Sure Gamble" 3)]))
    (is (= 5 (:memory (get-runner))) "CT starts with 5 memory")
    (play-from-hand state :corp "Cerebral Static")
    (is (= 4 (:memory (get-runner))) "Cerebral Static causes CT to have 4 memory")
    (play-from-hand state :corp "Lag Time")
    (is (= 5 (:memory (get-runner))) "CT 5 memory restored")))

(deftest closed-accounts
  ;; Closed Accounts - Play if Runner is tagged to make Runner lose all credits
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
  ;; Consulting Visit - Only show single copies of operations corp can afford as choices. Play chosen operation
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
  ;; Consulting Visit - Works properly when played with Mumbad City Hall
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
  ;; Defective Brainchips - Do 1 add'l brain damage the first time Runner takes some each turn
  (do-game
    (new-game (default-corp [(qty "Defective Brainchips" 1) (qty "Viktor 1.0" 1)])
              (default-runner [(qty "Sure Gamble" 2) (qty "Shiv" 2)]))
    (play-from-hand state :corp "Defective Brainchips")
    (play-from-hand state :corp "Viktor 1.0" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [vik (get-ice state :hq 0)]
      (core/rez state :corp vik)
      (card-subroutine state :corp vik 0)
      (is (= 2 (count (:discard (get-runner)))) "2 cards lost to brain damage")
      (is (= 2 (:brain-damage (get-runner))) "Brainchips dealt 1 additional brain dmg")
      (card-subroutine state :corp vik 0)
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

(deftest enhanced-login-protocol
  ;; Enhanced Login Protocol - tests interaction with run events, interaction with runs, and inability to run last click
  (do-game
    (new-game (default-corp [(qty "Enhanced Login Protocol" 1)])
              (default-runner [(qty "Dirty Laundry" 1)]))
    (play-from-hand state :corp "Enhanced Login Protocol")
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner starts turn with 4 clicks")
    (play-from-hand state :runner "Dirty Laundry")
    (prompt-choice :runner "Archives")
    (run-successful state)
    (is (= 3 (:click (get-runner))) "Run event only takes 1 click")
    (run-on state :hq)
    (run-jack-out state)
    (is (= 1 (:click (get-runner))) "first non-event run takes 2 clicks")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner starts turn with 4 clicks")
    (run-on state :hq)
    (run-jack-out state)
    (is (= 2 (:click (get-runner))) "first run takes 2 clicks")
    (run-on state :hq)
    (is (= 1 (:click (get-runner))) "second run takes 1 click")
    (run-jack-out state)
    (take-credits state :runner)
    (take-credits state :corp)
    (take-credits state :runner 2)
    (is (= 2 (:click (get-runner))))
    (is (core/can-run-server? state "HQ") "Runner can run - two clicks")
    (take-credits state :runner 1)
    (is (= 1 (:click (get-runner))))
    (run-on state :hq)
    (is (not (:run @state)) "No run initiated - not enough clicks")
    (is (= 1 (:click (get-runner))) "Runner still has last click")))

(deftest enhanced-login-protocol-keyhole
  ;; Enhanced Login Protocol - tests interaction with runs initiated from programs
  (do-game
    (new-game (default-corp [(qty "Enhanced Login Protocol" 1)])
              (default-runner [(qty "Keyhole" 1)]))
    (play-from-hand state :corp "Enhanced Login Protocol")
    (take-credits state :corp)
    (play-from-hand state :runner "Keyhole")
    (is (= 3 (:click (get-runner))) "Runner has 3 clicks before run")
    (let [keyhole (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner keyhole 0)
      (is (= 2 (:click (get-runner))) "Runner has 2 clicks after using keyhole"))))

(deftest enhanced-login-protocol-sol
  ;; Enhanced Login Protocol - tests interaction with ELP played on runners turn
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Enhanced Login Protocol" 1) (qty "Breaking News" 1)])
      (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-choice :runner "Steal")
    (prompt-choice :corp "Yes")
    (prompt-select :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "ELP in play")
    (is (= 3 (:click (get-runner))) "runner has 3 clicks when ELP is in play")
    (run-on state :hq)
    (run-jack-out state)
    (is (= 1 (:click (get-runner))) "first non-event run takes 2 clicks")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "runner has 4 clicks on start of turn")
    (run-on state :hq)
    (run-jack-out state)
    (is (= 2 (:click (get-runner))) "ELP still works correctly")))


(deftest exchange-of-information
  ;; Exchange of Information - Swapping agendas works correctly
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
  ;; Exchange of Information - Swapping a just scored Breaking News keeps the tags
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

(deftest exchange-of-information-fifteen-minutes
  ;; Exchange of Information - Swapping a 15 Minutes still keeps the ability. #1783
  (do-game
    (new-game (default-corp [(qty "Exchange of Information" 2) (qty "15 Minutes" 1)
                             (qty "Project Beale" 1)])
              (default-runner))
    (score-agenda state :corp (find-card "15 Minutes" (:hand (get-corp))))
    (take-credits state :corp)
    (core/gain state :runner :tag 1)
    (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
    (take-credits state :runner)
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 2 (:agenda-point (get-runner))))
    (play-from-hand state :corp "Exchange of Information")
    (prompt-select :corp (find-card "Project Beale" (:scored (get-runner))))
    (prompt-select :corp (find-card "15 Minutes" (:scored (get-corp))))
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 0 (count (:deck (get-corp)))))
    ;; shuffle back into R&D from runner's scored area
    (let [fifm (get-in @state [:runner :scored 0])]
      (card-ability state :corp fifm 0))
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 0 (:agenda-point (get-runner))))
    (is (= "15 Minutes" (:title (first (:deck (get-corp))))))
    (take-credits state :corp)
    (core/steal state :runner (find-card "15 Minutes" (:deck (get-corp))))
    (take-credits state :runner)
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 1 (:agenda-point (get-runner))))
    (play-from-hand state :corp "Exchange of Information")
    (prompt-select :corp (find-card "15 Minutes" (:scored (get-runner))))
    (prompt-select :corp (find-card "Project Beale" (:scored (get-corp))))
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 2 (:agenda-point (get-runner))))
    ;; shuffle back into R&D from corp's scored area
    (let [fifm (get-in @state [:corp :scored 0])]
      (card-ability state :corp fifm 0))
    (is (= "15 Minutes" (:title (first (:deck (get-corp))))))))

(deftest exchange-of-information-mandatory-upgrades
  ;; Exchange of Information - Swapping a Mandatory Upgrades gives the Corp an additional click per turn. #1687
  (do-game
    (new-game (default-corp [(qty "Exchange of Information" 2) (qty "Mandatory Upgrades" 1)
                             (qty "Global Food Initiative" 1)])
              (default-runner))
    (score-agenda state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
    (take-credits state :corp)
    (core/gain state :runner :tag 1)
    (core/steal state :runner (find-card "Mandatory Upgrades" (:hand (get-corp))))
    (take-credits state :runner)
    (is (= 3 (:agenda-point (get-corp))))
    (is (= 2 (:agenda-point (get-runner))))
    (is (= 3 (:click (get-corp))))
    (is (= 3 (:click-per-turn (get-corp))))
    (play-from-hand state :corp "Exchange of Information")
    (prompt-select :corp (find-card "Mandatory Upgrades" (:scored (get-runner))))
    (prompt-select :corp (find-card "Global Food Initiative" (:scored (get-corp))))
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 2 (:agenda-point (get-runner))))
    (is (= 3 (:click (get-corp))))
    (is (= 4 (:click-per-turn (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 4 (:click (get-corp))))
    (is (= 4 (:click-per-turn (get-corp))))
    (play-from-hand state :corp "Exchange of Information")
    (prompt-select :corp (find-card "Global Food Initiative" (:scored (get-runner))))
    (prompt-select :corp (find-card "Mandatory Upgrades" (:scored (get-corp))))
    (is (= 3 (:agenda-point (get-corp))))
    (is (= 2 (:agenda-point (get-runner))))
    (is (= 2 (:click (get-corp))))
    (is (= 3 (:click-per-turn (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))))
    (is (= 3 (:click-per-turn (get-corp))))))

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
  ;; Housekeeping - Runner must trash a card from Grip on first install of a turn
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

(deftest invasion-of-privacy
  ;; Invasion of Privacy - Full test
  (do-game
    (new-game (default-corp [(qty "Invasion of Privacy" 3)])
              (default-runner [(qty "Sure Gamble" 2) (qty "Fall Guy" 1) (qty "Cache" 2)]))
    (core/gain state :corp :click 3 :credit 6)
    ;; trash 2 cards
    (play-from-hand state :corp "Invasion of Privacy")
    (prompt-choice :corp 0) ; default trace
    (prompt-choice :runner 0) ; Runner won't match
    (is (= 5 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (prompt-choice :corp (find-card "Sure Gamble" (:hand (get-runner))))
      (prompt-choice :corp (find-card "Sure Gamble" (:hand (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))))
    ;; able to trash 2 cards but only 1 available target in Runner's hand
    (play-from-hand state :corp "Invasion of Privacy")
    (prompt-choice :corp 0) ; default trace
    (prompt-choice :runner 0) ; Runner won't match
    (is (= 3 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (prompt-choice :corp (find-card "Fall Guy" (:hand (get-runner))))
      (is (empty? (get-in @state [:corp :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-runner)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :corp "Invasion of Privacy")
    (prompt-choice :corp 0) ; default trace
    (prompt-choice :runner 2) ; Runner matches
    (is (= 1 (:bad-publicity (get-corp))))))

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

(deftest manhunt-every-run
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game (default-corp [(qty "Manhunt" 1) (qty "Hedge Fund" 3)])
              (default-runner))
    (play-from-hand state :corp "Manhunt")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (is (:prompt (get-corp)) "Manhunt trace initiated")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
    (prompt-choice :runner "OK")
    (is (not (:run @state)) "Run ended")
    (run-empty-server state "HQ")
    (is (empty? (:prompt (get-corp))) "No Manhunt trace on second run")
    (prompt-choice :runner "OK")
    (is (not (:run @state)) "Run ended")))

(deftest midseason-replacements
  ;; Midseason Replacements - Trace to give Runner tags after they steal an agenda
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
  ;; Mushin No Shin - Add 3 advancements to a card; prevent rez/score of that card the rest of the turn
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
  ;; Neural EMP - Play if Runner made a run the previous turn to do 1 net damage
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

(deftest oversight-ai
  ;; Oversight AI - Rez a piece of ICE ignoring all costs
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

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game (default-corp [(qty "Patch" 1) (qty "Vanilla" 1)])
              (default-runner))
    (play-from-hand state :corp "Vanilla" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Patch")
    (prompt-select :corp (get-ice state :hq 0))
    (is (= 2 (:current-strength (get-ice state :hq 0))) "Vanilla at 2 strength")))

(deftest paywall-implementation
  ;; Paywall Implementation - Gain 1 credit for every successful run
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
  ;; Peak Efficiency - Gain 1 credit for each rezzed ICE
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
  ;; Power Shutdown - Trash cards from R&D to force Runner to trash a program or hardware
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

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game (default-corp [(qty "Precognition" 1) (qty "Caprice Nisei" 1) (qty "Adonis Campaign" 1)
                             (qty "Quandary" 1) (qty "Jackson Howard" 1) (qty "Global Food Initiative" 1)])
              (default-runner))
    (starting-hand state :corp ["Precognition"])
    (play-from-hand state :corp "Precognition")
    (prompt-choice :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Quandary" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (prompt-choice :corp "Start over")
    (prompt-choice :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Quandary" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (prompt-choice :corp (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (prompt-choice :corp "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (do-game
    (new-game (default-corp [(qty "Subliminal Messaging" 3)
                             (qty "Preemptive Action" 1)])
              (default-runner))
    (play-from-hand state :corp "Subliminal Messaging")
    (play-from-hand state :corp "Subliminal Messaging")
    (play-from-hand state :corp "Subliminal Messaging")
    (play-from-hand state :corp "Preemptive Action")
    (prompt-select :corp (first (:discard (get-corp))))
    (prompt-select :corp (second (:discard (get-corp))))
    (prompt-select :corp (last (:discard (get-corp))))
    (is (= 0 (count (:discard (get-corp)))))
    (is (= 1 (count (:rfg (get-corp)))))))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Runner tags on a card
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

(deftest punitive-counterstrike
  ;; Punitive Counterstrike - deal meat damage equal to printed agenda points
  (do-game
    (new-game (default-corp [(qty "Global Food Initiative" 1) (qty "Punitive Counterstrike" 1)])
              (default-runner))
    (play-from-hand state :corp "Global Food Initiative" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-choice :runner "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Runner scored 2 points")
    (take-credits state :runner)
    (play-from-hand state :corp "Punitive Counterstrike")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (empty? (:hand (get-runner))) "Runner took 3 meat damage")))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card trashed from HQ
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

(deftest salems-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game (default-corp [(qty "Salem's Hospitality" 3)])
              (default-runner [(qty "I've Had Worse" 3) (qty "Faust" 1)
                               (qty "Levy AR Lab Access" 1)]))
    (play-from-hand state :corp "Salem's Hospitality")
    (is (= 5 (count (:hand (get-runner)))))
    (prompt-choice :corp "I've Had Worse")
    (is (= 2 (count (:hand (get-runner)))))
    (play-from-hand state :corp "Salem's Hospitality")
    (prompt-choice :corp "Plascrete Carapace")
    (is (= 2 (count (:hand (get-runner)))))))

(deftest scorched-earth
  ;; Scorched Earth - burn 'em
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Scorched Earth")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))

(deftest scorched-earth-no-tag
  ;; Scorched Earth - not tagged
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (play-from-hand state :corp "Scorched Earth")
    (is (= 3 (:click (get-corp))) "Corp not charged a click")
    (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))

(deftest scorched-earth-flatline
  ;; Scorched Earth - murderize 'em
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner))
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Scorched Earth")
    (is (= 0 (count (:hand (get-runner)))) "Runner has 0 cards in hand")
    (is (= :corp (:winner @state)) "Corp wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest subcontract-scorched
  ;; Subcontract - Don't allow second operation until damage prevention completes
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 2) (qty "Subcontract" 1)])
              (default-runner [(qty "Plascrete Carapace" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :tag 1)
    (play-from-hand state :runner "Plascrete Carapace")
    (take-credits state :runner)
    (play-from-hand state :corp "Subcontract")
    (prompt-select :corp (find-card "Scorched Earth" (:hand (get-corp))))
    (is (and (= 1 (count (:prompt (get-corp)))) (= :waiting (-> (get-corp) :prompt first :prompt-type)))
        "Corp does not have Subcontract prompt until damage prevention completes")
    (prompt-choice :runner "Done")
    (is (not-empty (:prompt (get-corp))) "Corp can now play second Subcontract operation")))

(deftest subcontract-terminal
  ;; Subcontract - interaction with Terminal operations
  (do-game
    (new-game
      (default-corp [(qty "Hard-Hitting News" 2) (qty "Subcontract" 1)])
      (default-runner))
    (core/gain state :runner :tag 1)
    (take-credits state :corp)
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "Subcontract")
    (prompt-select :corp (find-card "Hard-Hitting News" (:hand (get-corp))))
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 5 (:tag (get-runner))) "Runner has 5 tags")
    (is (empty? (:prompt (get-corp))) "Corp does not have a second Subcontract selection prompt")))

(deftest service-outage
  ;; Service Outage - First click run each turn costs a credit
  (do-game
    (new-game (default-corp [(qty "Service Outage" 1)])
              (default-runner [(qty "Employee Strike" 1)]))
    (play-from-hand state :corp "Service Outage")
    (take-credits state :corp)

    (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
    (run-on state :archives)
    (is (= 4 (:credit (get-runner)))
        "Runner spends 1 credit to make the first run")
    (run-successful state)

    (run-on state :archives)
    (is (= 4 (:credit (get-runner)))
        "Runner doesn't spend 1 credit to make the second run")
    (run-successful state)

    (take-credits state :runner)
    (take-credits state :corp)

    (core/lose state :runner :credit 6)
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (is (= 0 (:credit (get-runner))) "Runner has 0 credits")
    (run-on state :archives)
    (is (not (:run @state)) "No run was initiated")
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (is (= 0 (:credit (get-runner))) "Runner has 0 credits")

    (take-credits state :runner)
    (take-credits state :corp)

    (core/lose state :runner :credit 2)
    (play-from-hand state :runner "Employee Strike")
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")

    (run-on state :archives)
    (is (= 1 (:credit (get-runner)))
        "Runner doesn't spend 1 credit to make a run")))

(deftest service-outage-card-ability
  ;; Service Outage - First card ability run each turn costs an additional credit
  (do-game
    (new-game (default-corp [(qty "Service Outage" 1)])
              (default-runner [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :corp "Service Outage")
    (take-credits state :corp)
    (play-from-hand state :runner "Sneakdoor Beta")
    (take-credits state :runner 1)

    (is (= 2 (:credit (get-runner))) "Runner has 2 credits")
    (let [sneakdoor (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner sneakdoor 0)
      (is (= 1 (:credit (get-runner)))
          "Runner spends 1 additional credit to run with a card ability")
      (run-successful state)

      (run-on state :archives)
      (is (= 1 (:credit (get-runner)))
          "Runner doesn't spend 1 credit to make a run")
      (run-successful state)

      (take-credits state :runner)
      (take-credits state :corp)

      (core/lose state :runner :credit 1)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (= 0 (:credit (get-runner))) "Runner has 0 credits")
      (card-ability state :runner sneakdoor 0)
      (is (not (:run @state)) "No run was initiated")
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (= 0 (:credit (get-runner))) "Runner has 0 credits"))))

(deftest service-outage-run-events
  ;; Service Outage - First run event each turn costs an additional credit
  (do-game
    (new-game (default-corp [(qty "Service Outage" 1)])
              (default-runner [(qty "Out of the Ashes" 2)]))
    (play-from-hand state :corp "Service Outage")
    (take-credits state :corp)

    (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
    (play-from-hand state :runner "Out of the Ashes")
    (is (= 3 (:credit (get-runner)))
        "Runner spends 1 additional credit to run with a run event")
    (prompt-choice :runner "Archives")
    (run-successful state)

    (run-on state :archives)
    (is (= 3 (:credit (get-runner)))
        "Runner doesn't spend 1 credit to make a run")
    (run-successful state)

    (take-credits state :runner)
    (take-credits state :corp)
    (prompt-choice :runner "No") ; Out of the Ashes prompt

    (core/lose state :runner :credit 4)
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :runner "Out of the Ashes")
    (is (empty? (get-in @state [:runner :prompt]))
        "Out of the Ashes was not played")
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")))

(deftest shipment-from-sansan
  ;; Shipment from SanSan - placing advancements
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

(deftest stock-buy-back
  ;; Stock Buy-Back - Gain 3c for every agenda in Runner's area
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 2) (qty "Stock Buy-Back" 3)])
              (default-runner))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :runner "Steal")
    (take-credits state :runner)
    (is (= 2 (count (:scored (get-runner)))))
    (play-from-hand state :corp "Stock Buy-Back")
    (is (= 11 (:credit (get-corp))))))

(deftest sub-boost
  ;; Sub Boost - Give ice barrier
  (do-game
    (new-game (default-corp [(qty "Sub Boost" 1) (qty "Quandary" 1)])
              (default-runner))
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Sub Boost")
    (let [qu (get-ice state :hq 0)]
      (core/rez state :corp qu)
      (prompt-select :corp qu)
      (is (core/has-subtype? (refresh qu) "Code Gate") "Quandary has code gate")
      (is (core/has-subtype? (refresh qu) "Barrier") "Quandary has barrier"))))

(deftest subliminal-messaging
  ;; Subliminal Messaging - Playing/trashing/milling will all prompt returning to hand
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
    (is (= 0 (count (:hand (get-corp)))))
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
    (is (empty? (get-in @state [:corp :prompt])) "No prompt here because runner made a run last turn")
    (take-credits state :corp)
    (is (= 2 (count (:hand (get-corp)))))
    (is (= 1 (count (:discard (get-corp)))) "1 Subliminal not returned because runner made a run last turn")))

(deftest subliminal-messaging-archived
  ;; Subliminal Messaging - Scenario involving Subliminal being added to HQ with Archived Memories
  (do-game
    (new-game (default-corp [(qty "Subliminal Messaging" 2) (qty "Archived Memories" 1)])
              (default-runner))
    (play-from-hand state :corp "Subliminal Messaging")
    (play-from-hand state :corp "Subliminal Messaging")
    (play-from-hand state :corp "Archived Memories")
    (prompt-select :corp (find-card "Subliminal Messaging" (:discard (get-corp))))
    (is (= 2 (count (:discard (get-corp)))))
    (is (= 1 (count (:hand (get-corp)))))
    (take-credits state :corp)
    (take-credits state :runner)
    (prompt-choice :corp "No")
    (is (empty? (get-in @state [:corp :prompt])) "Only 1 Subliminal prompt")
    (play-from-hand state :corp "Subliminal Messaging")
    (take-credits state :corp)
    (take-credits state :runner)
    (prompt-choice :corp "Yes")
    (prompt-choice :corp "Yes")
    (is (empty? (get-in @state [:corp :prompt]))
        "Only 2 Subliminal prompts - there will be a third if flag not cleared")))

(deftest subliminal-messaging-jackson
  ;; Subliminal Messaging - Scenario involving Subliminal being reshuffled into R&D with Jackson
  (do-game
    (new-game (default-corp [(qty "Subliminal Messaging" 1) (qty "Jackson Howard" 1)])
              (default-runner))
    (play-from-hand state :corp "Subliminal Messaging")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp)
    (let [jhow (get-content state :remote1 0)]
      (core/rez state :corp jhow)
      (card-ability state :corp jhow 1)
      (prompt-select :corp (find-card "Subliminal Messaging" (:discard (get-corp))))
      (prompt-choice :corp "Done")
      (is (= 0 (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp))))))
    (take-credits state :runner)
    (play-from-hand state :corp "Subliminal Messaging")
    (take-credits state :corp)
    (take-credits state :runner)
    (prompt-choice :corp "Yes")
    (is (= 1 (count (:hand (get-corp)))) "Subliminal returned to HQ")
    (is (empty? (get-in @state [:corp :prompt]))
        "Subliminal prompt cleared - there will be a second prompt if flag not cleared")))

(deftest subliminal-messaging-made-run
  ;; Subliminal Messaging - Runner made run, ensure game asks again next turn
  (do-game
    (new-game (default-corp [(qty "Subliminal Messaging" 2)])
              (default-runner))
  (play-from-hand state :corp "Subliminal Messaging")
  (trash-from-hand state :corp "Subliminal Messaging")
  (take-credits state :corp)
  (run-on state "R&D")
  (run-jack-out state)
  (take-credits state :runner)
  (is (empty? (get-in @state [:corp :prompt])) "No prompt here because runner made a run last turn")
  (take-credits state :corp)
  (take-credits state :runner)
  (prompt-choice :corp "Yes")
  (prompt-choice :corp "Yes")
  (is (= 2 (count (:hand (get-corp)))) "Both Subliminals returned to HQ")
  (is (= 0 (count (:discard (get-corp)))) "No Subliminals in Archives")))

(deftest subliminal-messaging-no
  ;; Subliminal Messaging - User declines to return to hand, ensure game asks again next turn
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
  ;; Successful Demonstration - Play if only Runner made unsuccessful run last turn; gain 7 credits
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

(deftest wetwork-refit
  ;; Wetwork Refit - Only works on bioroid ice and adds a subroutine
  (do-game
    (new-game (default-corp [(qty "Eli 1.0" 1)
                             (qty "Vanilla" 1)
                             (qty "Wetwork Refit" 3)])
              (default-runner))
    (play-from-hand state :corp "Eli 1.0" "R&D")
    (play-from-hand state :corp "Vanilla" "HQ")
    (let [eli (get-ice state :rd 0)
          vanilla (get-ice state :hq 0)]
      (play-from-hand state :corp "Wetwork Refit")
      (is (not-any? #{"Eli 1.0"} (get-in @state [:runner :prompt :choices]))
          "Unrezzed Eli 1.0 is not a choice to host Wetwork Refit")
      (prompt-choice :corp "Done")

      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh eli))
      (core/rez state :corp (refresh vanilla))

      (play-from-hand state :corp "Wetwork Refit")
      (prompt-select :corp (refresh eli))
      (is (= "Wetwork Refit" (:title (first (:hosted (refresh eli)))))
          "Wetwork Refit is hosted on Eli 1.0")
      (is (= 2 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 2 different subroutines")
      (is (= "Do 1 brain damage" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has a brain damage subroutine as his first subroutine")

      (core/move state :corp (first (:hosted (refresh eli))) :hand)
      (is (empty? (:hosted (refresh eli))) "No cards are hosted on Eli 1.0")
      (is (= 1 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 1 different subroutine")
      (is (= "End the run" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has an end the run subroutine as his first subroutine")

      (play-from-hand state :corp "Wetwork Refit")
      (prompt-select :corp (refresh vanilla))
      (is (not= "Wetwork Refit" (:title (first (:hosted (refresh vanilla)))))
          "Wetwork Refit is not hosted on Vanilla"))))
