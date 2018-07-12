(ns game-test.cards.operations
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "operations"))

(deftest ^{:card-title "24/7-news-cycle"}
  twenty-four-seven-news
  ;; 24/7 News Cycle
  (testing "Breaking News interaction"
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
        (is (zero? (:tag (get-runner)))) ; tags cleared
        (take-credits state :runner)
        (play-from-hand state :corp "24/7 News Cycle")
        (prompt-select :corp (find-card "Breaking News" (:scored (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Forfeited Breaking News")
        (prompt-select :corp (find-card "Breaking News" (:scored (get-corp))))
        (is (= 2 (:tag (get-runner))) "Runner given 2 tags")
        (take-credits state :corp 2)
        (is (= 2 (:tag (get-runner))) "Tags remained after Corp ended turn"))))
  (testing "Posted Bounty interaction -- Issue #1043"
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
        (prompt-select :corp (find-card "Posted Bounty" (:scored (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Forfeited Posted Bounty")
        (prompt-select :corp (find-card "Posted Bounty" (:scored (get-corp))))
        (prompt-choice :corp "Yes") ; "Forfeit Posted Bounty to give 1 tag?"
        (is (= 1 (:tag (get-runner))) "Runner given 1 tag")
        (is (= 1 (:bad-publicity (get-corp))) "Corp has 1 bad publicity")
        (is (zero? (:agenda-point (get-corp))) "Forfeited Posted Bounty to 24/7 News Cycle"))))
  (testing "Swapped agendas are able to be used. #1555"
    (do-game
      (new-game (default-corp ["24/7 News Cycle" "Chronos Project"
                               "Philotic Entanglement" "Profiteering"])
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
      (prompt-select :corp (find-card "Chronos Project" (:scored (get-corp))))
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
      (is (= 23 (:credit (get-corp))) "Gained 15 credits"))))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Accelerated Diagnostics" "Cerebral Overwriter" "Shipment from SanSan"
                               "Hedge Fund" "Back Channels"])
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
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (prompt-select :corp hf)
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")
        (prompt-select :corp bc)
        (prompt-select :corp (refresh co))
        (is (= 15 (:credit (get-corp))) "Corp gained 6 credits for Back Channels"))))
  (testing "Interaction with Current"
    (do-game
      (new-game (default-corp ["Accelerated Diagnostics" "Cerebral Overwriter"
                               "Enhanced Login Protocol" "Shipment from SanSan"
                               "Hedge Fund"])
                (default-runner))
      (starting-hand state :corp ["Accelerated Diagnostics" "Cerebral Overwriter"])
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (core/gain state :corp :credit 3)
      (play-from-hand state :corp "Accelerated Diagnostics")
      (let [playarea (get-in @state [:corp :play-area])
            hf (find-card "Hedge Fund" playarea)
            ss (find-card "Shipment from SanSan" playarea)
            elp (find-card "Enhanced Login Protocol" playarea)
            co (get-content state :remote1 0)]
        (is (= 3 (count playarea)) "3 cards in play area")
        (prompt-select :corp elp)
        (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:corp :current]))))
            "Enhanced Login Protocol active in Current area")
        (prompt-select :corp ss)
        (prompt-choice :corp "2")
        (prompt-select :corp co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (prompt-select :corp hf)
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")))))

(deftest an-offer-you-can't-refuse
  ;; An Offer You Can't Refuse - exact card added to score area, not the last discarded one
  (do-game
    (new-game (default-corp ["Celebrity Gift" "An Offer You Can't Refuse"])
              (default-runner))
    (play-from-hand state :corp "An Offer You Can't Refuse")
    (prompt-choice :corp "R&D")
    (core/move state :corp (find-card "Celebrity Gift" (:hand (get-corp))) :discard)
    (is (= 2 (count (:discard (get-corp)))))
    (prompt-choice :runner "No")
    (is (= 1 (:agenda-point (get-corp))) "An Offer the Runner refused")
    (is (= 1 (count (:scored (get-corp)))))
    (is (find-card "An Offer You Can't Refuse" (:scored (get-corp))))
    (is (= 1 (count (:discard (get-corp)))))
    (is (find-card "Celebrity Gift" (:discard (get-corp))))))

(deftest biased-reporting
  ;; Biased Reporting
  (do-game
    (new-game (default-corp ["Biased Reporting"])
              (default-runner [(qty "Fan Site" 5)]))
    (take-credits state :corp)
    (starting-hand state :runner (repeat 5 "Fan Site"))
    (core/gain state :runner :click 10)
    (dotimes [_ 5]
      (play-from-hand state :runner "Fan Site"))
    (take-credits state :runner)
    (play-from-hand state :corp "Biased Reporting")
    (let [cc (:credit (get-corp))
          rc (:credit (get-runner))]
      (prompt-choice :corp "Resource")
      (prompt-select :runner (get-resource state 0))
      (prompt-choice :runner "Done")
      (is (= (inc rc) (:credit (get-runner))) "Runner should gain 1 credit for trashing a Fan Site")
      (is (= (+ (* 4 2) cc) (:credit (get-corp))) "Corp should gain 8 credits for remaining 4 Fan Sites"))))

(deftest big-brother
  ;; Big Brother - Give the Runner 2 tags if already tagged
  (do-game
    (new-game (default-corp ["Big Brother"])
              (default-runner))
    (play-from-hand state :corp "Big Brother")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Big Brother")
    (is (= 3 (:tag (get-runner))) "Runner gained 2 tags")))

(deftest biotic-labor
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game (default-corp ["Biotic Labor"])
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

(deftest building-blocks
  ;; Building Blocks - install and rez a barrier from HQ at no cost
  (testing "Basic behavior"
    (do-game
      (new-game (default-corp ["Building Blocks" "Ice Wall"])
                (default-runner))
      (core/gain state :corp :credit 1)
      (is (= 6 (:credit (get-corp))) "Corp starts with 6 credits")
      (play-from-hand state :corp "Building Blocks")
      (is (= 1 (:credit (get-corp))) "Spent 5 credits on Building Blocks")
      (prompt-select :corp (find-card "Ice Wall" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (let [iw (get-ice state :remote1 0)]
        (is (= 1 (:credit (get-corp))) "Corp spent no credits installing ice")
        (is (:rezzed (refresh iw)) "Ice Wall is installed and rezzed"))))
  (testing "Select invalid card"
    (do-game
      (new-game (default-corp ["Building Blocks" "Hedge Fund" "Cortex Lock"])
                (default-runner))
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Building Blocks")
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Starting prompt is correct")
      (prompt-select :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Cannot select non-ICE")
      (prompt-select :corp (find-card "Cortex Lock" (:hand (get-corp))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Cannot select non-barrier ICE"))))

(deftest casting-call
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
  (do-game
    (new-game (default-corp [(qty "Casting Call" 2) "Oaktown Renovation"
                             "Improved Tracers" "Hunter"])
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
        (is (:rezzed (refresh imptrac)) "Improved Tracers is faceup")
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

(deftest cerebral-cast
  ;; Cerebral Cast
  (testing "Runner wins"
    (do-game
      (new-game (default-corp ["Cerebral Cast"])
                (default-runner))
      (play-from-hand state :corp "Cerebral Cast")
      (is (= 3 (:click (get-corp))) "Cerebral Cast precondition not met; card not played")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Cerebral Cast")
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "0 [Credits]")
      (is (zero? (count (:discard (get-runner)))) "Runner took no damage")
      (is (zero? (:tag (get-runner))) "Runner took no tags")))
  (testing "Corp wins"
    (do-game
      (new-game (default-corp [(qty "Cerebral Cast" 2)])
                (default-runner))
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Cerebral Cast")
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "1 [Credits]")
      (prompt-choice :runner "1 brain damage")
      (is (= 1 (count (:discard (get-runner)))) "Runner took a brain damage")
      (is (zero? (:tag (get-runner))) "Runner took no tags from brain damage choice")
      (play-from-hand state :corp "Cerebral Cast")
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "1 [Credits]")
      (prompt-choice :runner "1 tag")
      (is (= 1 (count (:discard (get-runner)))) "Runner took no additional damage")
      (is (= 1 (:tag (get-runner))) "Runner took a tag from Cerebral Cast choice"))))

(deftest cerebral-static
  ;; Cerebral Static
  (testing "vs Chaos Theory"
    (do-game
      (new-game (default-corp ["Cerebral Static" "Lag Time"])
                (make-deck "Chaos Theory: Wünderkind" [(qty "Sure Gamble" 3)]))
      (is (= 5 (core/available-mu state)) "CT starts with 5 memory")
      (play-from-hand state :corp "Cerebral Static")
      (is (= 4 (core/available-mu state)) "Cerebral Static causes CT to have 4 memory")
      (play-from-hand state :corp "Lag Time")
      (is (= 5 (core/available-mu state)) "CT 5 memory restored"))))

(deftest closed-accounts
  ;; Closed Accounts - Play if Runner is tagged to make Runner lose all credits
  (do-game
    (new-game (default-corp ["Closed Accounts"])
              (default-runner))
    (play-from-hand state :corp "Closed Accounts")
    (is (and (= 3 (:click (get-corp)))
             (= 5 (:credit (get-runner))))
        "Closed Accounts precondition not met; card not played")
    (core/gain state :runner :tag 1)
    (play-from-hand state :corp "Closed Accounts")
    (is (zero? (:credit (get-runner))) "Runner lost all credits")))

(deftest commercialization
  ;; Commercialization
  (testing "Single advancement token"
    (do-game
      (new-game (default-corp ["Commercialization"
                               "Ice Wall"])
                (default-runner))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 1)
      (play-from-hand state :corp "Commercialization")
      (prompt-select :corp (refresh (get-ice state :hq 0)))
      (is (= 6 (:credit (get-corp))) "Gained 1 for single advanced ice from Commercialization")))
  (testing "Two advancement tokens"
    (do-game
      (new-game (default-corp ["Commercialization"
                               "Ice Wall"])
                (default-runner))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 2)
      (play-from-hand state :corp "Commercialization")
      (prompt-select :corp (refresh (get-ice state :hq 0)))
      (is (= 7 (:credit (get-corp))) "Gained 2 for double advanced ice from Commercialization"))))

(deftest consulting-visit
  ;; Consulting Visit - Only show single copies of operations corp can afford as choices. Play chosen operation
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Consulting Visit"
                               (qty "Beanstalk Royalties" 2)
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"])
                (default-runner))
      (is (= 5 (:credit (get-corp))))
      (starting-hand state :corp ["Consulting Visit"])
      (play-from-hand state :corp "Consulting Visit")
      (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (prompt-card :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (= 6 (:credit (get-corp)))))))
  (testing "Works properly when played with Mumbad City Hall"
    (do-game
      (new-game (default-corp ["Mumbad City Hall"
                               "Beanstalk Royalties"
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"
                               "Consulting Visit"
                               "Mumba Temple"])
                (default-runner))
      (is (= 5 (:credit (get-corp))))
      (starting-hand state :corp ["Mumbad City Hall"])
      (play-from-hand state :corp "Mumbad City Hall" "New remote")
      (let [hall (get-content state :remote1 0)
            get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (card-ability state :corp hall 0)
        (is (= (list "Consulting Visit" "Mumba Temple" nil) (prompt-names)))
        (prompt-card :corp (find-card "Consulting Visit" (:deck (get-corp))))
        (is (= 3 (:credit (get-corp))))
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (prompt-card :corp (find-card "Green Level Clearance" (:deck (get-corp))))
        (is (= 5 (:credit (get-corp))))))))

(deftest death-and-taxes
  ;; Death and Taxes gain credit on runner install, runner trash installed card
  ;; Also regression test for #3160
  (do-game
    (new-game (default-corp ["Death and Taxes" "PAD Campaign"])
              (default-runner ["Aumakua" "DaVinci" "Fall Guy"]))
    (play-from-hand state :corp "Death and Taxes")
    (is (= (- 5 2) (:credit (get-corp))) "Corp paid 2 to play Death and Taxes")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (let [corp-creds (:credit (get-corp))]
      (trash-from-hand state :runner "DaVinci")
      (is (= corp-creds (:credit (get-corp))) "Corp did not gain credit when runner trashes / discards from hand")
      (play-from-hand state :runner "Aumakua")
      (is (= (+ 1 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner installed Aumakua")
      (play-from-hand state :runner "Fall Guy")
      (is (= (+ 2 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner installed Fall Guy")
      (card-ability state :runner (get-resource state 0) 1)
      (is (= (+ 3 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner trashed Fall Guy")
      (run-empty-server state :remote1)
      (prompt-choice-partial :runner "Pay")  ;; Runner trashes PAD Campaign
      (is (= (+ 4 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner trashed PAD Campaign"))))

(deftest defective-brainchips
  ;; Defective Brainchips - Do 1 add'l brain damage the first time Runner takes some each turn
  (do-game
    (new-game (default-corp ["Defective Brainchips" "Viktor 1.0"])
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

(deftest distract-the-masses
  (do-game
    (new-game (default-corp [(qty "Distract the Masses" 2) (qty "Hedge Fund" 3)])
              (default-runner))
    (starting-hand state :corp ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Distract the Masses" "Distract the Masses"])
    (play-from-hand state :corp "Distract the Masses")
    (prompt-select :corp (first (:hand (get-corp))))
    (prompt-select :corp (first (next (:hand (get-corp)))))
    (prompt-select :corp (first (:discard (get-corp))))
    (prompt-choice :corp "Done")
    (is (= 1 (count (:discard (get-corp)))) "1 card still discarded")
    (is (= 1 (count (:deck (get-corp)))) "1 card shuffled into R&D")
    (is (= 1 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-runner))) "Runner gained 2 credits")
    (play-from-hand state :corp "Distract the Masses")
    (prompt-select :corp (first (:hand (get-corp))))
    (prompt-choice :corp "Done")
    (prompt-select :corp (first (:discard (get-corp))))
    (prompt-select :corp (first (next (:discard (get-corp)))))
    (is (zero? (count (:discard (get-corp)))) "No cards left in archives")
    (is (= 3 (count (:deck (get-corp)))) "2 more cards shuffled into R&D")
    (is (= 2 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 9 (:credit (get-runner))) "Runner gained 2 credits")))

(deftest diversified-portfolio
  (do-game
    (new-game (default-corp ["Diversified Portfolio"
                             "Paper Wall"
                             (qty "PAD Campaign" 3)])
              (default-runner))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Paper Wall" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Diversified Portfolio")
    (is (= 7 (:credit (get-corp))) "Ignored remote with ICE but no server contents")))

(deftest divert-power
  (do-game
    (new-game (default-corp [(qty "Divert Power" 2) "Paper Wall" (qty "Eve Campaign" 3) ])
              (default-runner))
    (core/gain state :corp :click 3 :credit 11)
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (let [pw (get-ice state :hq 0)
          ec1 (get-content state :remote1 0)
          ec2 (get-content state :remote2 0)
          ec3 (get-content state :remote3 0)]
      (core/rez state :corp pw)
      (core/rez state :corp ec1)
      (core/rez state :corp ec2)
      (play-from-hand state :corp "Divert Power")
      (is (= 4 (:credit (get-corp))) "Corp has 4 credits after rezzes and playing Divert Power")
      (testing "Choose 2 targets to derez"
        (prompt-select :corp (refresh pw))
        (prompt-select :corp (refresh ec1))
        (prompt-choice :corp "Done"))

      (testing "Choose a target to rez for -6 cost"
        (prompt-select :corp (refresh ec3)))

      (is (core/rezzed? (refresh ec3)) "Eve Campaign was rezzed")
      (is (= 4 (:credit (get-corp))) "Rezzed Eve Campaign for 0 credits")
      (is (not (core/rezzed? (refresh pw))) "Paper Wall was derezzed")
      (is (not (core/rezzed? (refresh ec1))) "First Eve Campaign was derezzed")
      (is (= 16 (get-counters (refresh ec3) :credit)) "Eve gained 16 credits on rez")

      (play-from-hand state :corp "Divert Power")
      (testing "Choose 1 target to derez"
        (prompt-select :corp (refresh ec2))
        (prompt-choice :corp "Done"))

      (testing "Choose a target to rez for -3 cost"
        (prompt-select :corp (refresh ec1)))

      (is (core/rezzed? (refresh ec1)) "First Eve Campaign was rezzed")
      (is (= 0 (:credit (get-corp))) "Rezzed Eve Campaign for 2 credits")
      (is (not (core/rezzed? (refresh ec2))) "Second Eve Campaign was derezzed")
      (is (= 32 (get-counters (refresh ec1) :credit)) "First Eve gained 16  more credits on rez"))))

(deftest door-to-door
  ;; Door to Door
  (do-game
    (new-game (default-corp ["Door to Door"])
              (default-runner))
    (play-from-hand state :corp "Door to Door")
    (take-credits state :corp)
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (is (= 3 (-> (get-runner) :hand count)) "Runner should start with 3 cards in hand")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:tag (get-runner))) "Runner should gain 1 tag from Door to Door")
    (is (= 3 (-> (get-runner) :hand count)) "Runner should start with 3 cards in hand")
    (take-credits state :runner)
    (take-credits state :corp)
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:tag (get-runner))) "Runner should still have 1 tag")
    (is (= 2 (-> (get-runner) :hand count)) "Runner should take 1 meat damage from Door to Door")))

(deftest economic-warfare
  ;; Economic Warfare - If successful run last turn, make the runner lose 4 credits if able
  (do-game
    (new-game (default-corp [(qty "Economic Warfare" 3)])
              (default-runner))
    (play-from-hand state :corp "Economic Warfare")
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
    (is (= 3 (count (:hand (get-corp)))) "Corp still has 3 cards")
    (take-credits state :corp)
    (run-on state :archives)
    (run-successful state)
    (take-credits state :runner)
    (play-from-hand state :corp "Economic Warfare")
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (play-from-hand state :corp "Economic Warfare")
    (is (zero? (:credit (get-runner))) "Runner has 0 credits")
    (take-credits state :corp)
    (run-on state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "Economic Warfare")
    (is (= 3 (:credit (get-runner))) "Runner has 3 credits")))

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

(deftest enforcing-loyalty
  ;; Enforcing Loyalty - Win trace to trash installed card not of Runner's faction
  (do-game
    (new-game (default-corp [(qty "Enforcing Loyalty" 2)])
              (make-deck "Chaos Theory: Wünderkind" ["Inti" "Caldera"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "Caldera")
    (take-credits state :runner)
    (play-from-hand state :corp "Enforcing Loyalty")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-program state 0))
    (is (empty? (:discard (get-runner))) "Can't target Inti; matches Runner faction")
    (prompt-select :corp (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))) "Caldera trashed")))

(deftest enhanced-login-protocol
  ;; Enhanced Login Protocol
  (testing "First click run each turn costs an additional click"
    (do-game
      (new-game (default-corp ["Enhanced Login Protocol"])
                (default-runner ["Employee Strike"]))
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make the first run")
      (run-successful state)
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner doesn't spend 1 additional click to make the second run")
      (run-successful state)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner 3)
      (is (= 1 (:click (get-runner))) "Runner has 1 click")
      (run-on state :archives)
      (is (not (:run @state)) "No run was initiated")
      (is (= 1 (:click (get-runner))) "Runner has 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (is (= 3 (:click (get-runner))) "Runner has 3 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-runner))) "Runner doesn't spend 1 additional click to make a run")))
  (testing "Card ability runs don't cost additional clicks"
    (do-game
      (new-game (default-corp ["Enhanced Login Protocol"])
                (default-runner ["Sneakdoor Beta"]))
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 2 clicks")
      (let [sneakdoor (get-program state 0)]
        (card-ability state :runner sneakdoor 0)
        (is (= 3 (:click (get-runner))) "Runner doesn't spend 1 additional click to run with a card ability")
        (run-successful state)
        (run-on state :archives)
        (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")
        (run-successful state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
        (run-on state :archives)
        (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make a run"))))
  (testing "with New Angeles Sol, Enhanced Login Protocol trashed and reinstalled on steal doesn't double remove penalty"
    (do-game
      (new-game
        (make-deck "New Angeles Sol: Your News" ["Enhanced Login Protocol" "Breaking News"])
        (default-runner))
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (prompt-choice :runner "Steal")
      (prompt-choice :corp "Yes")
      (prompt-select :corp (find-card "Enhanced Login Protocol" (:discard (get-corp))))
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner has 1 click")))
  (testing "Run event don't cost additional clicks"
    (do-game
      (new-game (default-corp ["Enhanced Login Protocol"])
                (default-runner ["Out of the Ashes"]))
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (play-from-hand state :runner "Out of the Ashes")
      (prompt-choice :runner "Archives")
      (is (= 3 (:click (get-runner))) "Runner doesn't spend 1 additional click to run with a run event")
      (run-successful state)
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")
      (run-successful state)
      (take-credits state :runner)
      (take-credits state :corp)
      (prompt-choice :runner "No") ; Out of the Ashes prompt
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make a run")))
  (testing "Works when played on the runner's turn"
    (do-game
      (new-game (make-deck "New Angeles Sol: Your News"
                           ["Enhanced Login Protocol"
                            "Breaking News"])
                (default-runner ["Hades Shard"]))
      (trash-from-hand state :corp "Breaking News")
      (take-credits state :corp)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Hades Shard")
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-choice :runner "Steal")
      (prompt-choice :corp "Yes")
      (prompt-select :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
      (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "Enhanced Login Protocol is in play")
      (is (= 3 (:click (get-runner))) "Runner has 3 clicks")
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")))
(testing "Doesn't fire if already run when played on the runner's turn"
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News"
                         ["Enhanced Login Protocol"
                          "Breaking News"])
              (default-runner ["Hades Shard"]))
    (trash-from-hand state :corp "Breaking News")
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (prompt-choice :runner "No action")
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Hades Shard")
    (card-ability state :runner (get-resource state 0) 0)
    (prompt-choice :runner "Steal")
    (prompt-choice :corp "Yes")
    (prompt-select :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
    (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "Enhanced Login Protocol is in play")
    (is (= 2 (:click (get-runner))) "Runner has 2 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-runner))) "Runner doesn't spend 1 additional click to make a run"))))

(deftest exchange-of-information
  ;; Exchange of Information
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"])
                (default-runner))
      (score-agenda state :corp (find-card "Market Research" (:hand (get-corp))))
      (score-agenda state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner gained 2 tags")
      (take-credits state :corp)
      (is (zero? (:tag (get-runner))) "Runner lost 2 tags")
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
  (testing "Swapping a just scored Breaking News keeps the tags"
    (do-game
      (new-game (default-corp ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"])
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
  (testing "Swapping a 15 Minutes still keeps the ability. #1783"
    (do-game
      (new-game (default-corp [(qty "Exchange of Information" 2) "15 Minutes"
                               "Project Beale"])
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
      (is (zero? (count (:deck (get-corp)))))
      ;; shuffle back into R&D from runner's scored area
      (let [fifm (get-scored state :runner 0)]
        (card-ability state :corp fifm 0))
      (is (= 2 (:agenda-point (get-corp))))
      (is (zero? (:agenda-point (get-runner))))
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
      (let [fifm (get-scored state :corp 0)]
        (card-ability state :corp fifm 0))
      (is (= "15 Minutes" (:title (first (:deck (get-corp))))))))
  (testing "Swapping a Mandatory Upgrades gives the Corp an additional click per turn. #1687"
    (do-game
      (new-game (default-corp [(qty "Exchange of Information" 2) "Mandatory Upgrades"
                               "Global Food Initiative"])
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
      (is (= 3 (:click-per-turn (get-corp)))))))

(deftest foxfire
  ;; Foxfire
  (do-game
    (new-game (default-corp [(qty "Foxfire" 2)])
              (default-runner ["Dyson Mem Chip" "Ice Carver"]))
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Dyson Mem Chip")
    (play-from-hand state :runner "Ice Carver")
    (take-credits state :runner)
    (play-from-hand state :corp "Foxfire")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-hardware state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Corp should trash Dyson Mem Chip from winning Foxfire trace")
    (play-from-hand state :corp "Foxfire")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-resource state 0))
    (is (= 2 (-> (get-runner) :discard count)) "Corp should trash Ice Carver from winning Foxfire trace")))

(deftest hard-hitting-news
  ;; Hard-Hitting News
  (do-game
    (new-game (default-corp ["Hard-Hitting News"])
              (default-runner))
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should start with 3 clicks")
    (play-from-hand state :corp "Hard-Hitting News")
    (is (zero? (:click (get-corp))) "Playing Hard-Hitting News should lose all remaining clicks")
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 4 (:tag (get-runner))) "Runner should gain 4 tags from losing Hard-Hitting News trace")))

(deftest hatchet-job
  ;; Hatchet Job - Win trace to add installed non-virtual to grip
  (do-game
    (new-game (default-corp ["Hatchet Job"])
              (default-runner ["Upya" "Ghost Runner"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Ghost Runner")
    (play-from-hand state :runner "Upya")
    (take-credits state :runner)
    (play-from-hand state :corp "Hatchet Job")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-resource state 0))
    (is (empty? (:hand (get-runner))) "Can't choose virtual card")
    (is (not (empty? (:prompt (get-corp)))))
    (prompt-select :corp (get-program state 0))
    (is (= 1 (count (:hand (get-runner)))) "Upya returned to grip")))

(deftest hedge-fund
  (do-game
    (new-game (default-corp) (default-runner))
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Hedge Fund")
    (is (= 9 (:credit (get-corp))))))

(deftest hellion-alpha-test
  ;; Hellion Alpha Test
  (do-game
    (new-game (default-corp [(qty "Hellion Alpha Test" 2)])
              (default-runner ["Daily Casts"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (play-from-hand state :corp "Hellion Alpha Test")
    (is (zero? (-> (get-runner) :deck count)) "Runner should have no cards in Stack")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-resource state 0))
    (is (= 1 (-> (get-runner) :deck count)) "Runner should have 1 card in Stack from losing Hellion Alpha Test trace")
    (is (= "Daily Casts" (-> (get-runner) :deck first :title))
        "Runner should have Daily Casts on top of Stack from losing Hellion Alpha Test trace")
    (take-credits state :corp)
    (core/draw state :runner)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (play-from-hand state :corp "Hellion Alpha Test")
    (is (zero? (:bad-publicity (get-corp))) "Corp should start with 0 bad publicity")
    (prompt-choice :corp 0)
    (prompt-choice :runner 2)
    (is (= 1 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity from losing Hellion Alpha Test trace")))

(deftest hellion-beta-test
  ;; Hellion Beta Test
  (testing "Winning Trace - Trashing 2 cards"
    (do-game
      (new-game (default-corp ["Dedicated Response Team" "Hellion Beta Test"])
                (default-runner ["Daily Casts" "Dyson Mem Chip"]))
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "Dyson Mem Chip")
      (run-empty-server state :remote1)
      (prompt-choice-partial :runner "Pay")
      (take-credits state :runner)
      (is (zero? (-> (get-runner) :discard count)) "Runner's heap should be empty")
      (play-from-hand state :corp "Hellion Beta Test")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (prompt-select :corp (get-resource state 0))
      (prompt-select :corp (get-hardware state 0))
      (is (= 2 (-> (get-runner) :discard count)) "Runner should have 2 cards in heap after losing Hellion Beta Test trace")))
  (testing "Losing trace - Gaining bad publicity"
    (do-game
      (new-game (default-corp ["Dedicated Response Team" "Hellion Beta Test"])
                (default-runner ["Daily Casts" "Dyson Mem Chip"]))
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "Dyson Mem Chip")
      (run-empty-server state :remote1)
      (prompt-choice-partial :runner "Pay")
      (take-credits state :runner)
      (is (zero? (:bad-publicity (get-corp))) "Corp should start with 0 bad publicity")
      (play-from-hand state :corp "Hellion Beta Test")
      (prompt-choice :corp 0)
      (prompt-choice :runner 2)
      (is (= 1 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity from losing Hellion Beta Test trace"))))

(deftest high-profile-target
  (testing "when the runner has no tags"
    (do-game
      (new-game (default-corp [(qty "High-Profile Target" 6)])
                (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))
  (testing "when the runner has one tag"
    (do-game
      (new-game (default-corp [(qty "High-Profile Target" 6)])
                (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (count (:hand (get-runner)))) "Runner has 3 cards in hand")))
  (testing "when the runner has two tags"
    (do-game
      (new-game (default-corp [(qty "High-Profile Target" 6)])
                (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (core/gain state :runner :tag 2)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))
  (testing "When the runner has three tags, gg"
    (do-game
      (new-game (default-corp [(qty "High-Profile Target" 10)])
                (default-runner))
      (core/gain state :runner :tag 3)
      (play-from-hand state :corp "High-Profile Target")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest housekeeping
  ;; Housekeeping - Runner must trash a card from Grip on first install of a turn
  (do-game
    (new-game (default-corp ["Housekeeping"])
              (default-runner [(qty "Cache" 2) "Fall Guy" "Mr. Li"]))
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
              (default-runner [(qty "Sure Gamble" 2) "Fall Guy" (qty "Cache" 2)]))
    (core/gain state :corp :click 3 :credit 6)
    ;; trash 2 cards
    (play-from-hand state :corp "Invasion of Privacy")
    (prompt-choice :corp 0) ; default trace
    (prompt-choice :runner 0) ; Runner won't match
    (is (= 5 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (prompt-card :corp (find-card "Sure Gamble" (:hand (get-runner))))
      (prompt-card :corp (find-card "Sure Gamble" (:hand (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))))
    ;; able to trash 2 cards but only 1 available target in Runner's hand
    (play-from-hand state :corp "Invasion of Privacy")
    (prompt-choice :corp 0) ; default trace
    (prompt-choice :runner 0) ; Runner won't match
    (is (= 3 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (prompt-card :corp (find-card "Fall Guy" (:hand (get-runner))))
      (is (empty? (get-in @state [:corp :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-runner)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :corp "Invasion of Privacy")
    (prompt-choice :corp 0) ; default trace
    (prompt-choice :runner 2) ; Runner matches
    (is (= 1 (:bad-publicity (get-corp))))))

(deftest ipo
  ;; IPO - credits with Terminal operations
  (do-game
    (new-game
      (default-corp ["IPO"])
      (default-runner))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "IPO")
    (is (= 13 (:credit (get-corp))))
    (is (zero? (:click (get-corp))) "Terminal ends turns")))

(deftest kill-switch
  ;; Kill Switch
  (do-game
    (new-game (default-corp ["Kill Switch" (qty "Hostile Takeover" 2)])
              (default-runner))
    (play-from-hand state :corp "Kill Switch")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 brain damage")
    (play-and-score state "Hostile Takeover")
    (prompt-choice :corp "Hostile Takeover")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:brain-damage (get-runner))) "Runner should get 1 brain damage from Kill Switch after Corp scores an agenda")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 2 (:brain-damage (get-runner))) "Runner should get 1 brain damage from Kill Switch after accecssing an agenda")))

(deftest lag-time
  (do-game
    (new-game (default-corp ["Lag Time" "Vanilla" "Lotus Field"])
              (default-runner))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Lotus Field" "R&D")
    (play-from-hand state :corp "Lag Time")
    (core/rez state :corp (get-ice state :hq 0))
    (core/rez state :corp (get-ice state :rd 0))
    (is (= 1 (:current-strength (get-ice state :hq 0))) "Vanilla at 1 strength")
    (is (= 5 (:current-strength (get-ice state :rd 0))) "Lotus Field at 5 strength")))

(deftest lateral-growth
  (do-game
    (new-game (default-corp ["Lateral Growth" "Breaking News"])
              (default-runner))
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Lateral Growth")
    (prompt-select :corp (find-card "Breaking News" (:hand (get-corp))))
    (prompt-choice :corp "New remote")
    (is (= "Breaking News" (:title (get-content state :remote1 0)))
      "Breaking News installed by Lateral Growth")
    (is (= 7 (:credit (get-corp))))))

(deftest manhunt
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game (default-corp ["Manhunt" (qty "Hedge Fund" 3)])
              (default-runner))
    (play-from-hand state :corp "Manhunt")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (is (:prompt (get-corp)) "Manhunt trace initiated")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
    (prompt-choice :runner "No action")
    (is (not (:run @state)) "Run ended")
    (run-empty-server state "HQ")
    (is (empty? (:prompt (get-corp))) "No Manhunt trace on second run")
    (prompt-choice :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest market-forces
  (testing "Full test"
    (letfn [(market-forces-credit-test
              [{:keys [tag-count runner-creds expected-credit-diff]}]
              (testing (str "when the runner has " tag-count " tags and " runner-creds " credits")
                (do-game
                  (new-game (default-corp [(qty "Market Forces" 6)])
                            (default-runner))
                  (swap! state assoc-in [:corp :credit] 0)
                  (swap! state assoc-in [:runner :credit] runner-creds)
                  (core/gain state :runner :tag tag-count)
                  (play-from-hand state :corp "Market Forces")
                  (is (= expected-credit-diff (:credit (get-corp)))
                      (str "the corp gains " expected-credit-diff " credits"))
                  (is (= expected-credit-diff (- runner-creds (:credit (get-runner))))
                      (str "the runner loses " expected-credit-diff " credits")))))]
      (doall (map market-forces-credit-test
                  [{:tag-count            1
                    :runner-creds         10
                    :expected-credit-diff 3}
                   {:tag-count            2
                    :runner-creds         10
                    :expected-credit-diff 6}
                   {:tag-count            3
                    :runner-creds         10
                    :expected-credit-diff 9}
                   {:tag-count            3
                    :runner-creds         0
                    :expected-credit-diff 0}
                   {:tag-count            3
                    :runner-creds         5
                    :expected-credit-diff 5}]))))
  (testing "when the runner is not tagged"
    (do-game
      (new-game (default-corp [(qty "Market Forces" 6)])
                (default-runner))
      (play-from-hand state :corp "Market Forces")
      (is (= 6 (count (:hand (get-corp))))
          "Market Forces is not played")
      (is (= 3 (:click (get-corp)))
          "the corp does not spend a click")
      (is (= 5 (:credit (get-corp)) (:credit (get-runner)))
          "credits are unaffected"))))

(deftest mass-commercialization
  ;; Mass Commercialization
  (do-game
    (new-game (default-corp ["Mass Commercialization"
                             (qty "Ice Wall" 3)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :runner)
    (core/advance state :corp {:card (refresh (get-ice state :hq 0))})
    (core/advance state :corp {:card (refresh (get-ice state :archives 0))})
    (core/advance state :corp {:card (refresh (get-ice state :rd 0))})
    (take-credits state :runner)
    (play-from-hand state :corp "Mass Commercialization")
    (is (= 8 (:credit (get-corp))) "Gained 6 for 3 advanced ice from Mass Commercialization")))

(deftest medical-research-fundraiser
  ;; Medical Research Fundraiser - runner gains 8creds, runner gains 3creds
  (do-game
    (new-game (default-corp ["Medical Research Fundraiser"])
              (default-runner))
    (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-from-hand state :corp "Medical Research Fundraiser")
    (is (= 10 (:credit (get-corp))) "Corp gains 8 credits")
    (is (= 8 (:credit (get-runner))) "Runner gains 3 credits")))

(deftest midseason-replacements
  ;; Midseason Replacements - Trace to give Runner tags after they steal an agenda
  (do-game
    (new-game (default-corp ["Midseason Replacements" "Breaking News"])
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
    (new-game (default-corp [(qty "Mushin No Shin" 2) "Ronin" "Profiteering"])
              (default-runner))
    (play-from-hand state :corp "Mushin No Shin")
    (prompt-select :corp (find-card "Ronin" (:hand (get-corp))))
    (let [ronin (get-content state :remote1 0)]
      (is (= 3 (get-counters (refresh ronin) :advancement)) "3 advancements placed on Ronin")
      (core/rez state :corp (refresh ronin))
      (is (not (:rezzed (refresh ronin))) "Ronin did not rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh ronin))
      (is (:rezzed (refresh ronin)) "Ronin now rezzed")
      (play-from-hand state :corp "Mushin No Shin")
      (prompt-select :corp (find-card "Profiteering" (:hand (get-corp))))
      (let [prof (get-content state :remote2 0)]
        (core/score state :corp (refresh prof))
        (is (empty? (:scored (get-corp))) "Profiteering not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/score state :corp (refresh prof))
        (prompt-choice :corp "0")
        (is (= 1 (:agenda-point (get-corp))) "Profiteering was able to be scored")))))

(deftest mutate
  ;; Mutate - trash a rezzed piece of ice, install and rez one from R&D
  (testing "Basic operation"
    (do-game
      (new-game (default-corp ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"])
                (default-runner))
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (prompt-select :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma is installed")
      (is (:rezzed (get-ice state :hq 0)) "Enigma is rezzed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Installed card name was logged")))
  (testing "No ice in R&D"
    (do-game
      (new-game (default-corp ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"])
                (default-runner))
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (prompt-select :corp (get-ice state :hq 0))
      (is (empty? (get-ice state :hq)) "No ice installed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged"))))

(deftest neural-emp
  ;; Neural EMP - Play if Runner made a run the previous turn to do 1 net damage
  (do-game
    (new-game (default-corp ["Neural EMP"])
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
    (new-game (default-corp ["Oversight AI" "Archer"])
              (default-runner))
    (play-from-hand state :corp "Archer" "R&D")
    (let [archer (get-ice state :rd 0)]
      (play-from-hand state :corp "Oversight AI")
      (prompt-select :corp archer)
      (is (:rezzed (refresh archer)))
      (is (= 4 (:credit (get-corp))) "Archer rezzed at no credit cost")
      (is (= "Oversight AI" (:title (first (:hosted (refresh archer)))))
          "Archer hosting OAI as a condition"))))

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game (default-corp ["Patch" "Vanilla"])
              (default-runner))
    (play-from-hand state :corp "Vanilla" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Patch")
    (prompt-select :corp (get-ice state :hq 0))
    (is (= 2 (:current-strength (get-ice state :hq 0))) "Vanilla at 2 strength")))

(deftest paywall-implementation
  ;; Paywall Implementation - Gain 1 credit for every successful run
  (do-game
    (new-game (default-corp ["Paywall Implementation"])
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
    (new-game (default-corp ["Peak Efficiency" (qty "Paper Wall" 3) "Wraparound"])
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
              (default-runner ["Grimoire" "Cache"]))
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
    (prompt-select :runner (get-hardware state 0)) ; try targeting Grimoire
    (is (empty? (:discard (get-runner))) "Grimoire too expensive to be targeted")
    (prompt-select :runner (get-program state 0))
    (is (= 1 (count (:discard (get-runner)))) "Cache trashed")))

(deftest power-grid-overload
  ;; Power Grid Overload
  (do-game
    (new-game (default-corp ["Power Grid Overload"])
              (default-runner ["Dyson Mem Chip"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Dyson Mem Chip")
    (run-empty-server state :rd)
    (take-credits state :runner)
    (play-from-hand state :corp "Power Grid Overload")
    (prompt-choice :corp 3)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-hardware state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Dyson Mem Chip should be in heap after Runner loses Power Grid Overload trace")))

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game (default-corp ["Precognition" "Caprice Nisei" "Adonis Campaign"
                             "Quandary" "Jackson Howard" "Global Food Initiative"])
              (default-runner))
    (starting-hand state :corp ["Precognition"])
    (play-from-hand state :corp "Precognition")
    (prompt-card :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (prompt-card :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (prompt-card :corp (find-card "Quandary" (:deck (get-corp))))
    (prompt-card :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (prompt-card :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (prompt-choice :corp "Start over")
    (prompt-card :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (prompt-card :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (prompt-card :corp (find-card "Quandary" (:deck (get-corp))))
    (prompt-card :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (prompt-card :corp (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (prompt-choice :corp "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Subliminal Messaging" 3)
                               "Preemptive Action"])
                (default-runner))
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (prompt-select :corp (first (:discard (get-corp))))
      (prompt-select :corp (second (:discard (get-corp))))
      (prompt-select :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))
  (testing "forces you to take 3 if there are three, and removes itself from game"
    (do-game
      (new-game (default-corp [(qty "Subliminal Messaging" 3)
                               (qty "Preemptive Action" 1)])
                (default-runner))
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (prompt-select :corp (first (:discard (get-corp))))
      (prompt-select :corp (last (:discard (get-corp))))
      (is (= 3 (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))
  (testing "Shuffles all archives cards into R&D if Archives has less than 3 cards, and removes itself from game"
    (do-game
      (new-game (default-corp [(qty "Subliminal Messaging" 2)
                               (qty "Preemptive Action" 1)])
                (default-runner))
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (prompt-select :corp (first (:discard (get-corp))))
      (prompt-select :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp))))))))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Runner tags on a card
  (do-game
    (new-game (default-corp ["Psychographics" "Project Junebug"])
              (default-runner))
    (core/gain state :runner :tag 4)
    (play-from-hand state :corp "Project Junebug" "New remote")
    (let [pj (get-content state :remote1 0)]
      (play-from-hand state :corp "Psychographics")
      (prompt-choice :corp 4)
      (prompt-select :corp pj)
      (is (= 1 (:credit (get-corp))) "Spent 4 credits")
      (is (= 4 (get-counters (refresh pj) :advancement)) "Junebug has 4 advancements"))))

(deftest psychokinesis
  ;; Pyschokinesis - Terminal Event (end the turn); Look at R&D, install an Asset, Agenda, or Upgrade in a Remote Server
  (do-game
    (new-game (default-corp [(qty "Psychokinesis" 3) "Caprice Nisei" "Adonis Campaign"
                              "Global Food Initiative" "Mwanza City Grid"])
              (default-runner))
    (starting-hand state :corp ["Psychokinesis" "Psychokinesis" "Psychokinesis"])
    ;; Test installing an Upgrade
    (play-from-hand state :corp "Psychokinesis")
    (is (not-any? #{"Mwanza City Grid"} (map :title (-> (get-corp) :prompt first :choices)))
        "Mwanza City Grid is not on the list of installable cards")
    (prompt-card :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (prompt-choice :corp "New remote")
    (is (= "Caprice Nisei" (:title (get-content state :remote1 0)))
      "Caprice Nisei installed by Psychokinesis")
    ;; Test installing an Asset
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (prompt-card :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (prompt-choice :corp "New remote")
    (is (= "Adonis Campaign" (:title (get-content state :remote2 0)))
      "Adonis Campaign installed by Psychokinesis")
    ;; Test installing an Agenda
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (prompt-card :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (prompt-choice :corp "New remote")
    (is (= "Global Food Initiative" (:title (get-content state :remote3 0)))
      "Global Food Initiative installed by Psychokinesis")
    ;; Test selecting "None"
    (core/gain state :corp :click 1)
    (core/move state :corp (find-card "Psychokinesis" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Psychokinesis")
    (prompt-choice :corp "None")
    (is (= nil (:title (get-content state :remote4 0)))
      "Nothing is installed by Psychokinesis")))

(deftest punitive-counterstrike
  ;; Punitive Counterstrike - deal meat damage equal to printed agenda points
  (do-game
    (new-game (default-corp ["Global Food Initiative" "Punitive Counterstrike"])
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

(deftest red-planet-couriers
  ;; Red Planet Couriers - Move all advancements on cards to 1 advanceable card
  (do-game
    (new-game (default-corp ["Red Planet Couriers" (qty "Ice Wall" 2)
                             "GRNDL Refinery" "Government Takeover"])
              (default-runner))
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Government Takeover" "New remote")
    (play-from-hand state :corp "GRNDL Refinery" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [gt (get-content state :remote1 0)
          gr (get-content state :remote2 0)
          iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)]
      (core/add-prop state :corp gr :advance-counter 3)
      (core/add-prop state :corp iw1 :advance-counter 2)
      (core/add-prop state :corp iw2 :advance-counter 1)
      (play-from-hand state :corp "Red Planet Couriers")
      (prompt-select :corp gt)
      (is (zero? (get-counters (refresh gr) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw1) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw2) :advancement)) "Advancements removed")
      (is (= 6 (get-counters (refresh gt) :advancement)) "Gained 6 advancements"))))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card trashed from HQ
  (do-game
    (new-game (default-corp [(qty "Reuse" 2) "Hive" "IQ"
                             "Ice Wall"])
              (default-runner))
    (play-from-hand state :corp "Reuse")
    (prompt-select :corp (find-card "Ice Wall" (:hand (get-corp))))
    (prompt-select :corp (find-card "Hive" (:hand (get-corp))))
    (prompt-select :corp (find-card "IQ" (:hand (get-corp))))
    (prompt-choice :corp "Done")
    (is (= 4 (count (:discard (get-corp)))) "3 cards trashed plus operation played")
    (is (= 11 (:credit (get-corp))) "Gained 6 credits")
    (is (= 1 (:click (get-corp))) "Spent 2 clicks")))

(deftest reverse-infection
  ;; Reverse Infection - purge and trash 1 card from stack for every 3 counters purged - or gain 2 credits
  (do-game
    (new-game (default-corp [(qty "Reverse Infection" 2)])
              (default-runner ["Virus Breeding Ground" "Datasucker" (qty "Sure Gamble" 3)]))
    (starting-hand state :runner ["Virus Breeding Ground" "Datasucker"])
    (play-from-hand state :corp "Reverse Infection")
    (prompt-choice :corp "Gain 2 [Credits]")
    (is (= 7 (:credit (get-corp))) "Corp gained 2 credits")
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Datasucker")
    (take-credits state :runner)
    (core/add-counter state :runner (get-resource state 0) :virus 4)
    (core/add-counter state :runner (get-program state 0) :virus 3)
    (play-from-hand state :corp "Reverse Infection")
    (prompt-choice :corp "Purge virus counters.")
    (is (= 9 (:credit (get-corp))) "Corp did not gain credits")
    (is (zero? (get-counters (get-resource state 0) :virus)) "Viruses purged from VBG")
    (is (zero? (get-counters (get-program state 0) :virus)) "Viruses purged from Datasucker")
    (is (= 2 (count (:discard (get-runner)))) "Two cards trashed from stack")))

(deftest riot-suppression
  ;; Riot Suppression - lose 3 clicks or take 1 brain damage
  (testing "Take 1 brain damage"
    (do-game
      (new-game (default-corp ["Riot Suppression" "Adonis Campaign"])
                (default-runner))
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no brain damage")
      (prompt-choice-partial :runner "Yes")
      (is (= 1 (count (:discard (get-runner)))) "1 card lost to brain damage")
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppestion removed from game")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has all clicks the following turn")))
  (testing "Lose 3 clicks"
    (do-game
      (new-game (default-corp ["Riot Suppression" "Adonis Campaign"])
                (default-runner))
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no brain damage")
      (prompt-choice-partial :runner "No")
      (is (empty? (:discard (get-runner))) "Runner discard statys empty")
      (is (zero? (:brain-damage (get-runner))) "Runner takes no brain damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppestion removed from game")
      (take-credits state :corp)
      (is (= 1 (:click (get-runner))) "Runner has 3 fewer clicks following turn"))))

(deftest rolling-brownout
  ;; Rolling Brownout - Increase cost of events/operations by 1, gain 1c on first Runner event of turn
  (do-game
    (new-game (default-corp ["Rolling Brownout" "Beanstalk Royalties"
                             "Domestic Sleepers"])
              (default-runner [(qty "Easy Mark" 3)]))
    (play-from-hand state :corp "Rolling Brownout")
    (play-from-hand state :corp "Beanstalk Royalties")
    (is (= 5 (:credit (get-corp))) "Beanstalk netted only 2c")
    (play-from-hand state :corp "Domestic Sleepers" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Easy Mark")
    (is (= 7 (:credit (get-runner))) "Easy Mark netted only 2c")
    (is (= 6 (:credit (get-corp))) "Corp gained 1c from Brownout")
    (play-from-hand state :runner "Easy Mark")
    (is (= 6 (:credit (get-corp))) "No Corp credit gain from 2nd event")
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Steal")
    (play-from-hand state :runner "Easy Mark")
    (is (= 12 (:credit (get-runner))) "Easy Mark netted 3c after Brownout trashed")))

(deftest salem's-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game (default-corp [(qty "Salem's Hospitality" 3)])
              (default-runner [(qty "I've Had Worse" 3) "Faust"
                               "Levy AR Lab Access"]))
    (play-from-hand state :corp "Salem's Hospitality")
    (is (= 5 (count (:hand (get-runner)))))
    (prompt-choice :corp "I've Had Worse")
    (is (= 2 (count (:hand (get-runner)))))
    (play-from-hand state :corp "Salem's Hospitality")
    (prompt-choice :corp "Plascrete Carapace")
    (is (= 2 (count (:hand (get-runner)))))))

(deftest scorched-earth
  ;; Scorched Earth
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Scorched Earth"])
                (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))
  (testing "not tagged"
    (do-game
      (new-game (default-corp ["Scorched Earth"])
                (default-runner [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (play-from-hand state :corp "Scorched Earth")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))
  (testing "flatline"
    (do-game
      (new-game (default-corp [(qty "Scorched Earth" 10)])
                (default-runner))
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest sea-source
  ;; SEA Source
  (do-game
    (new-game (default-corp ["SEA Source"])
              (default-runner))
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (play-from-hand state :corp "SEA Source")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:tag (get-runner))) "Runner should get 1 tag from losing SEA Source trace")))

(deftest self-growth-program
  ;; Self-Growth Program - Add 2 installed cards to grip if runner is tagged
  (do-game
    (new-game (default-corp ["Self-Growth Program"])
              (default-runner ["Clone Chip" "Inti"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Inti")
    (take-credits state :runner)
    (play-from-hand state :corp "Self-Growth Program")
    (is (= 3 (:click (get-corp))) "Self-Growth Program precondition not met; card not played")
    (core/gain state :runner :tag 1)
    (is (zero? (count (:hand (get-runner)))) "Runner hand is empty")
    (let [inti (get-program state 0)
          cc (get-hardware state 0)]
      (play-from-hand state :corp "Self-Growth Program")
      (prompt-select :corp inti)
      (prompt-select :corp cc))
    (is (= 2 (count (:hand (get-runner)))) "2 cards returned to hand")
    (is (zero? (count (get-program state))) "No programs installed")
    (is (zero? (count (get-hardware state))) "No hardware installed")))

(deftest service-outage
  ;; Service Outage
  (testing "First click run each turn costs a credit"
    (do-game
      (new-game (default-corp ["Service Outage"])
                (default-runner ["Employee Strike"]))
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
      (is (zero? (:credit (get-runner))) "Runner has 0 credits")
      (run-on state :archives)
      (is (not (:run @state)) "No run was initiated")
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (zero? (:credit (get-runner))) "Runner has 0 credits")
      (take-credits state :runner)
      (take-credits state :corp)
      (core/lose state :runner :credit 2)
      (play-from-hand state :runner "Employee Strike")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (run-on state :archives)
      (is (= 1 (:credit (get-runner)))
          "Runner doesn't spend 1 credit to make a run")))
  (testing "First card ability run each turn costs an additional credit"
    (do-game
      (new-game (default-corp ["Service Outage"])
                (default-runner ["Sneakdoor Beta"]))
      (play-from-hand state :corp "Service Outage")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (take-credits state :runner 1)
      (is (= 2 (:credit (get-runner))) "Runner has 2 credits")
      (let [sneakdoor (get-program state 0)]
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
        (is (zero? (:credit (get-runner))) "Runner has 0 credits")
        (card-ability state :runner sneakdoor 0)
        (is (not (:run @state)) "No run was initiated")
        (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
        (is (zero? (:credit (get-runner))) "Runner has 0 credits"))))
  (testing "First run event each turn costs an additional credit"
    (do-game
      (new-game (default-corp ["Service Outage"])
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
  (testing "Works when played on the runner's turn"
    (do-game
      (new-game (make-deck "New Angeles Sol: Your News" ["Service Outage"
                                                         "Breaking News"])
                (default-runner ["Hades Shard"]))
      (trash-from-hand state :corp "Breaking News")
      (take-credits state :corp)
      (core/gain state :runner :credit 3)
      (play-from-hand state :runner "Hades Shard")
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-choice :runner "Steal")
      (prompt-choice :corp "Yes")
      (prompt-select :corp (find-card "Service Outage" (:hand (get-corp))))
      (is (find-card "Service Outage" (:current (get-corp)))
          "Service Outage is in play")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (run-on state :archives)
      (is (zero? (:credit (get-runner)))
          "Runner spends 1 additional credit to make a run")))
(testing "Doesn't fire if already run when played on the runner's turn"
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News" ["Service Outage"
                                                       "Breaking News"])
              (default-runner ["Hades Shard"]))
    (trash-from-hand state :corp "Breaking News")
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (prompt-choice :runner "No action")
    (core/gain state :runner :credit 3)
    (play-from-hand state :runner "Hades Shard")
    (card-ability state :runner (get-resource state 0) 0)
    (prompt-choice :runner "Steal")
    (prompt-choice :corp "Yes")
    (prompt-select :corp (find-card "Service Outage" (:hand (get-corp))))
    (is (find-card "Service Outage" (:current (get-corp)))
        "Service Outage is in play")
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (run-on state :archives)
    (is (= 1 (:credit (get-runner)))
        "Runner doesn't spend 1 additional credit to make a run")))
(testing "trashed and reinstalled on steal doesn't double remove penalty"
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" ["Service Outage"
                                               "Breaking News"])
      (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Service Outage")
    (take-credits state :corp)
    (run-on state :remote1)
    (run-successful state)
    (prompt-choice :runner "Steal")
    (prompt-choice :corp "Yes")
    (prompt-select :corp (find-card "Service Outage" (:discard (get-corp))))
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 7 (:credit (get-runner))) "Runner has 7 credits")
    (run-on state :archives)
    (is (= 6 (:credit (get-runner))) "Runner spends 1 credit to make a run"))))

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
      (is (= 2 (get-counters (refresh iwall) :advancement))))))

(deftest snatch-and-grab
  ;; Snatch and Grab
  (do-game
    (new-game (default-corp [(qty "Snatch and Grab" 2)])
              (default-runner ["Scrubber"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Scrubber")
    (take-credits state :runner)
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (play-from-hand state :corp "Snatch and Grab")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-resource state 0))
    (prompt-choice :runner "Yes")
    (is (= 1 (:tag (get-runner))) "Runner should get 1 tag from losing Snatch and Grab trace and opting to take the tag")
    (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
    (play-from-hand state :corp "Snatch and Grab")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-resource state 0))
    (prompt-choice :runner "No")
    (is (= 1 (-> (get-runner) :discard count)) "Scrubber should be in Runner's heap after losing Snatch and Grab trace")))

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
  ;; Sub Boost - Give ICE Barrier
  (do-game
    (new-game (default-corp ["Sub Boost" "Quandary"])
              (default-runner))
    (play-from-hand state :corp "Quandary" "HQ")
    (let [qu (get-ice state :hq 0)]
      (core/rez state :corp qu)
      (is (not (core/has-subtype? (refresh qu) "Barrier")) "Quandry starts without Barrier")
      (is (= 1 (count (:subroutines (refresh qu)))) "Quandry has 1 subroutine")
      (play-from-hand state :corp "Sub Boost")
      (prompt-select :corp (refresh qu))
      (is (core/has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (core/has-subtype? (refresh qu) "Barrier") "Quandary ICE Barrier")
      (is (= 2 (count (:subroutines (refresh qu)))) "Quandry gains a subroutine"))))

(deftest subcontract
  ;; Subcontract
  (testing "Don't allow second operation until damage prevention completes"
    (do-game
      (new-game (default-corp [(qty "Scorched Earth" 2) "Subcontract"])
                (default-runner ["Plascrete Carapace"]))
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
  (testing "interaction with Terminal operations"
    (do-game
      (new-game
        (default-corp [(qty "Hard-Hitting News" 2) "Subcontract"])
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
      (is (empty? (:prompt (get-corp))) "Corp does not have a second Subcontract selection prompt"))))

(deftest subliminal-messaging
  ;; Subliminal Messaging - Playing/trashing/milling will all prompt returning to hand
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Subliminal Messaging" 3)])
                (make-deck "Noise: Hacker Extraordinaire" [(qty "Cache" 3) "Utopia Shard"]))
      (play-from-hand state :corp "Subliminal Messaging")
      (is (= 6 (:credit (get-corp))))
      (is (= 3 (:click (get-corp))) "First Subliminal Messaging gains 1 click")
      (play-from-hand state :corp "Subliminal Messaging")
      (is (= 7 (:credit (get-corp))))
      (is (= 2 (:click (get-corp))) "Second Subliminal Messaging does not gain 1 click")
      (trash-from-hand state :corp "Subliminal Messaging")
      (is (zero? (count (:hand (get-corp)))))
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
      (let [utopia (get-resource state 0)]
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
  (testing "Scenario involving Subliminal being added to HQ with Archived Memories"
    (do-game
      (new-game (default-corp [(qty "Subliminal Messaging" 2) "Archived Memories"])
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
  (testing "Scenario involving Subliminal being reshuffled into R&D with Jackson"
    (do-game
      (new-game (default-corp ["Subliminal Messaging" "Jackson Howard"])
                (default-runner))
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp)
      (let [jhow (get-content state :remote1 0)]
        (core/rez state :corp jhow)
        (card-ability state :corp jhow 1)
        (prompt-select :corp (find-card "Subliminal Messaging" (:discard (get-corp))))
        (prompt-choice :corp "Done")
        (is (zero? (count (:discard (get-corp)))))
        (is (= 1 (count (:rfg (get-corp))))))
      (take-credits state :runner)
      (play-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (take-credits state :runner)
      (prompt-choice :corp "Yes")
      (is (= 1 (count (:hand (get-corp)))) "Subliminal returned to HQ")
      (is (empty? (get-in @state [:corp :prompt]))
          "Subliminal prompt cleared - there will be a second prompt if flag not cleared")))
  (testing "Runner made run, ensure game asks again next turn"
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
      (is (zero? (count (:discard (get-corp)))) "No Subliminals in Archives")))
  (testing "User declines to return to hand, ensure game asks again next turn"
    (do-game
      (new-game (default-corp [(qty "Subliminal Messaging" 2)])
                (default-runner))
      (play-from-hand state :corp "Subliminal Messaging")
      (trash-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (take-credits state :runner)
      (prompt-choice :corp "No")
      (prompt-choice :corp "No")
      (is (zero? (count (:hand (get-corp)))) "Neither Subliminal returned to HQ")
      (is (= 2 (count (:discard (get-corp)))) "Both Subliminals in Archives")
      (take-credits state :corp)
      (take-credits state :runner)
      (prompt-choice :corp "Yes")
      (prompt-choice :corp "Yes")
      (is (= 2 (count (:hand (get-corp)))) "Both Subliminals returned to HQ")
      (is (zero? (count (:discard (get-corp)))) "No Subliminals in Archives"))))

(deftest success
  ;; Success
  (testing "Works with bad publicity"
    (do-game
      (new-game (default-corp ["NAPD Contract" "Project Beale" "Success"])
                (default-runner))
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (play-from-hand state :corp "Project Beale" "New remote")
      (core/gain state :corp :bad-publicity 9)
      (core/gain state :corp :credit 8)
      (core/gain state :corp :click 15)
      (let [napd (get-content state :remote1 0)
            beale (get-content state :remote2 0)]
        (dotimes [_ 13] (core/advance state :corp {:card (refresh napd)}))
        (is (= 13 (get-counters (refresh napd) :advancement)))
        (core/score state :corp {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-corp))))
        (play-from-hand state :corp "Success")
        (prompt-select :corp (get-scored state :corp 0))
        (is (= "NAPD Contract" (:title (first (:rfg (get-corp))))))
        (prompt-select :corp (refresh beale))
        (is (= 13 (get-counters (refresh beale) :advancement)))
        (core/score state :corp {:card (refresh beale)})
        (is (= 7 (:agenda-point (get-corp)))))))
  (testing "Works with public agendas"
    (do-game
      (new-game (default-corp ["Oaktown Renovation" "Vanity Project" "Success"])
                (default-runner))
      (core/gain state :corp :click 1)
      (score-agenda state :corp (find-card "Vanity Project" (:hand (get-corp))))
      (is (= 4 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (= 5 (:credit (get-corp))))
      (play-from-hand state :corp "Success")
      (prompt-select :corp (get-scored state :corp 0))
      (is (= "Vanity Project" (:title (first (:rfg (get-corp))))))
      (let [oaktown (get-content state :remote1 0)]
        (prompt-select :corp (refresh oaktown))
        (is (= 6 (get-counters (refresh oaktown) :advancement)))
        (is (= 19 (:credit (get-corp))) "Gain 2 + 2 + 2 + 2 + 3 + 3 = 14 credits for advancing Oaktown")
        (core/score state :corp {:card (refresh oaktown)})
        (is (= 2 (:agenda-point (get-corp)))))))
  (testing "interaction with Jemison, regression test for issue #2704"
    (do-game
      (new-game (make-deck "Jemison Astronautics: Sacrifice. Audacity. Success."
                           ["Success"
                            "High-Risk Investment"
                            "Government Takeover"])
                (default-runner))
      (core/gain state :corp :click 1)
      (score-agenda state :corp (find-card "High-Risk Investment" (:hand (get-corp))))
      (play-from-hand state :corp "Government Takeover" "New remote")
      (play-from-hand state :corp "Success")
      (prompt-select :corp (get-in (get-corp) [:scored 0]))
      (let [gto (get-content state :remote1 0)]
        ;; Prompt for Jemison
        (prompt-select :corp (refresh gto))
        (is (= 4 (get-counters (refresh gto) :advancement)) "Added 4 counters from Jemison trigger")
        ;; Prompt for Success
        (prompt-select :corp (refresh gto))
        (is (= (+ 4 5) (get-counters (refresh gto) :advancement)) "Advance 5 times from Success")))))

(deftest successful-demonstration
  ;; Successful Demonstration - Play if only Runner made unsuccessful run last turn; gain 7 credits
  (do-game
    (new-game (default-corp ["Successful Demonstration"])
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

(deftest surveillance-sweep
  ;; Surveillance Sweep
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Restructured Datapool" "Surveillance Sweep" "Data Raven"])
                (default-runner ["Scrubbed"]))
      (is (zero? (:tag (get-runner))) "Runner should start with no tags")
      (play-from-hand state :corp "Surveillance Sweep")
      (play-and-score state "Restructured Datapool")
      (let [rd-scored (get-scored state :corp 0)]
        (card-ability state :corp rd-scored 0)
        (is (not= :waiting (-> (get-corp) :prompt first :prompt-type)) "Surveillance Sweep only works during a run")
        (prompt-choice :corp 0)
        (prompt-choice :runner 0)
        (is (= 1 (:tag (get-runner))) "Runner should gain a tag from Restructured Datapool ability"))
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Data Raven" "HQ")
      (take-credits state :corp)
      (let [dr (get-ice state :hq 0)]
        (core/rez state :corp (refresh dr))
        (run-on state :hq)
        (card-subroutine state :corp dr 0)
        (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "During a run, Corp should wait on Runner first")
        (prompt-choice :runner 0)
        (prompt-choice :corp 0)
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (play-from-hand state :runner "Scrubbed")
        (run-on state :hq)
        (card-subroutine state :corp dr 0)
        (is (not= :waiting (-> (get-corp) :prompt first :prompt-type)) "Runner should now be waiting on Corp")
        (prompt-choice :corp 0)
        (prompt-choice :runner 0)
        (is (= 2 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state))))
  (testing "trace during run after stealing an agenda"
    (do-game
      (new-game (default-corp ["Surveillance Sweep" "Breaking News" "Forced Connection" "Data Raven"])
                (default-runner))
      (core/gain state :corp :click 4)
      (core/gain state :corp :credit 20)
      (play-from-hand state :corp "Surveillance Sweep")
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Forced Connection" "Server 1")
      (play-from-hand state :corp "Data Raven" "Server 1")
      (take-credits state :corp)
      (let [dr (get-ice state :remote1 0)
            bn (get-content state :remote1 0)
            fc (get-content state :remote1 1)]
        (core/rez state :corp (refresh dr))
        (run-on state :remote1)
        (card-subroutine state :corp dr 0)
        (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "During a run, Corp should wait on Runner first")
        (prompt-choice :runner 0)
        (prompt-choice :corp 0)
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (prompt-select :runner bn)
        (prompt-choice :runner "Steal")
        (prompt-select :runner fc)
        (is (not= :waiting (-> (get-corp) :prompt first :prompt-type)) "After steal, Surveillance Sweep leaves play and Runner waits on Corp")))))

(deftest the-all-seeing-i
  (testing "Counts number of cards if one card is prevented trashed with fall guy"
    (do-game
      (new-game (default-corp ["The All-Seeing I"])
                (default-runner ["Fall Guy" (qty "Same Old Thing" 2)]))
      (letfn [(res [] (count (get-in (get-runner) [:rig :resource])))]
        (take-credits state :corp)
        (play-from-hand state :runner "Same Old Thing")
        (play-from-hand state :runner "Fall Guy")
        (play-from-hand state :runner "Same Old Thing")
        (take-credits state :runner)
        (play-from-hand state :corp "The All-Seeing I")
        (is (= 1 (count (:hand (get-corp)))) "Corp could not play All Seeing I when runner was not tagged")
        (core/gain state :runner :tag 1)
        (play-from-hand state :corp "The All-Seeing I")
        (let [fall-guy (get-resource state 1)]
          (card-ability state :runner fall-guy 0))
        (prompt-choice :runner "Done")
        (is (= 1 (res)) "One installed resource saved by Fall Guy")
        (is (= 2 (count (:discard (get-runner)))) "Two cards in heap"))))
  (testing "Checks that All-seeing I does not double-trash hosted cards, trashes hosted cards"
    (do-game
      (new-game (default-corp ["The All-Seeing I"])
                (default-runner [(qty "Fall Guy" 2) "Off-Campus Apartment"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)
            fg1 (get-in (get-runner) [:hand 0])
            fg2 (get-in (get-runner) [:hand 1])]
        (card-ability state :runner oca 0)
        (prompt-select :runner fg1)
        (card-ability state :runner oca 0)
        (prompt-select :runner fg2))
      (core/gain state :runner :tag 1)
      (take-credits state :runner)
      (play-from-hand state :corp "The All-Seeing I")
      (prompt-choice :runner "Done")
      (prompt-choice :runner "Done")
      (let  [fall-guy (find-card "Fall Guy" (core/all-active-installed state :runner))]
        (card-ability state :runner fall-guy 0))
      (prompt-choice :runner "Done") ;; This assumes hosted cards get put in trash-list before host
      (is (= 1 (count (core/all-active-installed state :runner))) "One installed card (Off-Campus)")
      (is  (= 2 (count (:discard (get-runner)))) "Two cards in heap")))
  (testing "should not trash Jarogniew Mercs if there are other installed resources"
    (do-game
      (new-game (default-corp [(qty "The All-Seeing I" 4)])
                (default-runner [(qty "Jarogniew Mercs" 2) (qty "Same Old Thing" 2)]))
      (letfn [(res [] (count (get-in (get-runner) [:rig :resource])))]
        (take-credits state :corp)
        (play-from-hand state :runner "Same Old Thing")
        (play-from-hand state :runner "Jarogniew Mercs")
        (take-credits state :runner)
        (is (= 2 (res)) "There are two installed resources")
        (play-from-hand state :corp "The All-Seeing I")
        (is (= 1 (res)) "Jarogniew Mercs still installed")
        (play-from-hand state :corp "The All-Seeing I")
        (is (zero? (res)) "There are no installed resources")
        (take-credits state :corp)
        (play-from-hand state :runner "Jarogniew Mercs") ;; Testing if order matters
        (play-from-hand state :runner "Same Old Thing")
        (take-credits state :runner)
        (is (= 2 (res)) "There are two installed resources")
        (play-from-hand state :corp "The All-Seeing I")
        (is (= 1 (res)) "Jarogniew Mercs still installed")
        (play-from-hand state :corp "The All-Seeing I")
        (is (zero? (res)) "There are no installed resources")))))

(deftest threat-assessment
  ;; Threat Assessment - play only if runner trashed a card last turn, move a card to the stack or take 2 tags
  (do-game
    (new-game (default-corp [(qty "Threat Assessment" 3) "Adonis Campaign"])
              (default-runner ["Desperado" "Corroder"]))
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (take-credits state :corp)
    (run-on state :remote1)
    (run-successful state)
    (prompt-choice-partial :runner "Pay") ;trash
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Desperado")
    (play-from-hand state :runner "Corroder")
    (take-credits state :runner)
    (is (zero? (:tag (get-runner))) "Runner starts with 0 tags")
    (play-from-hand state :corp "Threat Assessment")
    (prompt-select :corp (find-card "Desperado" (-> (get-runner) :rig :hardware)))
    (prompt-choice :runner "2 tags")
    (is (= 2 (:tag (get-runner))) "Runner took 2 tags")
    (is (= 1 (count (-> (get-runner) :rig :hardware))) "Didn't trash Desperado")
    (is (= "Threat Assessment" (:title (first (:rfg (get-corp))))) "Threat Assessment removed from game")
    (play-from-hand state :corp "Threat Assessment")
    (prompt-select :corp (find-card "Corroder" (-> (get-runner) :rig :program)))
    (prompt-choice :runner "Move Corroder")
    (is (= 2 (:tag (get-runner))) "Runner didn't take tags")
    (is (= "Corroder" (:title (first (:deck (get-runner))))) "Moved Corroder to the deck")
    (is (= 2 (count (:rfg (get-corp)))))
    (take-credits state :runner)
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Threat Assessment")
    (is (empty? (:prompt (get-corp))) "Threat Assessment triggered with no trash")))

(deftest threat-level-alpha
  ;; Threat Level Alpha - Win trace to give tags = Runner tags; or 1 tag if 0
  (do-game
    (new-game (default-corp [(qty "Threat Level Alpha" 2)])
              (default-runner))
    (core/gain state :corp :click 2)
    (core/gain state :corp :credit 2)
    (is (zero? (:tag (get-runner))))
    (play-from-hand state :corp "Threat Level Alpha")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag because they had 0")
    (core/gain state :runner :tag 2)
    (play-from-hand state :corp "Threat Level Alpha")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 6 (:tag (get-runner))) "Runner took 3 tag because they had 3")))

(deftest transparency-initiative
  ;; Transparency Initiative - Full test
  (do-game
    (new-game (default-corp ["Transparency Initiative" "Oaktown Renovation"
                             "Project Atlas" "Hostile Takeover" "Casting Call"])
              (default-runner))
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (play-from-hand state :corp "Casting Call")
    (prompt-select :corp (find-card "Project Atlas" (:hand (get-corp))))
    (prompt-choice :corp "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [oaktown (get-content state :remote1 0)
          atlas (get-content state :remote2 0)
          hostile (get-content state :remote3 0)]
      (play-from-hand state :corp "Transparency Initiative")
      (prompt-select :corp (refresh oaktown))
      ;; doesn't work on face-up agendas
      (is (zero? (count (:hosted (refresh oaktown)))))
      (prompt-select :corp (refresh atlas))
      (is (= 1 (count (:hosted (refresh atlas)))) "Casting Call")
      ;; works on facedown agenda
      (prompt-select :corp (refresh hostile))
      (is (= 1 (count (:hosted (refresh hostile)))))
      ;; gains Public subtype
      (is (core/has-subtype? (refresh hostile) "Public"))
      ;; gain 1 credit when advancing
      (is (= 5 (:credit (get-corp))))
      (core/advance state :corp {:card (refresh hostile)})
      (is (= 5 (:credit (get-corp))))
      ;; make sure advancing other agendas doesn't gain 1
      (core/advance state :corp {:card (refresh oaktown)})
      (is (= 6 (:credit (get-corp))) "Transparency initiative didn't fire")
      (core/advance state :corp {:card (refresh atlas)})
      (is (= 5 (:credit (get-corp))) "Transparency initiative didn't fire"))))

(deftest trojan-horse
  ;; Trojan Horse
  (do-game
    (new-game (default-corp ["Trojan Horse" "Dedicated Response Team"])
              (default-runner ["Wyrm"]))
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Wyrm")
    (run-empty-server state :remote1)
    (take-credits state :runner)
    (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
    (play-from-hand state :corp "Trojan Horse")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (prompt-select :corp (get-program state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Wyrm should be in heap after Runner loses Trojan Horse trace")))

(deftest under-the-bus
  ;; Under the Bus
  (do-game
    (new-game (default-corp ["Under the Bus"])
              (default-runner ["Film Critic"]))
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (prompt-choice :runner "No action")
    (play-from-hand state :runner "Film Critic")
    (take-credits state :runner)
    (is (= 1 (count (get-resource state))) "Runner has 1 resource installed")
    (is (zero? (:bad-publicity (get-corp))) "Corp has no bad pub")
    (play-from-hand state :corp "Under the Bus")
    (prompt-select :corp (get-resource state 0))
    (is (empty? (get-resource state)) "Runner has no resource installed")
    (is (= 1 (count (:discard (get-runner)))) "Runner has 1 trashed card")
    (is (= 1 (:bad-publicity (get-corp))) "Corp takes 1 bad pub")))

(deftest wake-up-call
  ;; Wake Up Call
  (testing "should fire after using En Passant to trash ice"
    (do-game
      (new-game (default-corp ["Enigma" "Wake Up Call"])
                (default-runner ["En Passant" "Maya"]))
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Maya")
      (run-on state :hq)
      (run-successful state)
      (prompt-choice :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "Corp starts with no discards")
      (play-from-hand state :runner "En Passant")
      (prompt-select :runner (get-ice state :hq 0))
      (is (= 1 (count (:discard (get-corp)))) "Corp trashes installed ice")
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))) "Runner starts with 1 trashed card (En Passant)")
      (play-from-hand state :corp "Wake Up Call")
      (prompt-select :corp (get-hardware state 0))
      (prompt-choice :runner "Trash Maya")
      (is (= 2 (count (:discard (get-runner)))) "Maya is trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Wake Up Call is removed from the game"))))

(deftest wetwork-refit
  ;; Wetwork Refit - Only works on Bioroid ICE and adds a subroutine
  (do-game
    (new-game (default-corp ["Eli 1.0"
                             "Vanilla"
                             (qty "Wetwork Refit" 3)])
              (default-runner))
    (core/gain state :corp :credit 20)
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "Eli 1.0" "R&D")
    (play-from-hand state :corp "Vanilla" "HQ")
    (let [eli (get-ice state :rd 0)
          vanilla (get-ice state :hq 0)]
      (play-from-hand state :corp "Wetwork Refit")
      (is (not-any? #{"Eli 1.0"} (get-in @state [:corp :prompt :choices]))
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
      (is (= "[Wetwork Refit] Do 1 brain damage" (:label (first (:subroutines (refresh eli)))))
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
