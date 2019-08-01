(ns game-test.cards.operations
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ^{:card-title "24-7-news-cycle"}
  twenty-four-seven-news-cycle
  ;; 24/7 News Cycle
  (testing "Breaking News interaction"
    (do-game
      (new-game {:corp {:deck [(qty "Breaking News" 2) (qty "24/7 News Cycle" 3)]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Breaking News" "New remote")
      (let [ag1 (get-content state :remote1 0)
            ag2 (get-content state :remote2 0)]
        (score-agenda state :corp ag1)
        (score-agenda state :corp ag2)
        (take-credits state :corp)
        (is (zero? (count-tags state))) ; tags cleared
        (take-credits state :runner)
        (play-from-hand state :corp "24/7 News Cycle")
        (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Forfeited Breaking News")
        (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
        (is (= 2 (count-tags state)) "Runner given 2 tags")
        (take-credits state :corp 2)
        (is (= 2 (count-tags state)) "Tags remained after Corp ended turn"))))
  (testing "Posted Bounty interaction -- Issue #1043"
    (do-game
      (new-game {:corp {:deck [(qty "Posted Bounty" 2) (qty "24/7 News Cycle" 3)]}})
      (play-from-hand state :corp "Posted Bounty" "New remote")
      (play-from-hand state :corp "Posted Bounty" "New remote")
      (let [ag1 (get-content state :remote1 0)
            ag2 (get-content state :remote2 0)]
        (score-agenda state :corp ag1)
        (click-prompt state :corp "No")
        (score-agenda state :corp ag2)
        (click-prompt state :corp "No")
        (play-from-hand state :corp "24/7 News Cycle")
        (click-card state :corp (find-card "Posted Bounty" (:scored (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Forfeited Posted Bounty")
        (click-card state :corp (find-card "Posted Bounty" (:scored (get-corp))))
        (click-prompt state :corp "Yes") ; "Forfeit Posted Bounty to give 1 tag?"
        (is (= 1 (count-tags state)) "Runner given 1 tag")
        (is (= 1 (count-bad-pub state)) "Corp has 1 bad publicity")
        (is (zero? (:agenda-point (get-corp))) "Forfeited Posted Bounty to 24/7 News Cycle"))))
  (testing "Swapped agendas are able to be used. #1555"
    (do-game
      (new-game {:corp {:deck ["24/7 News Cycle" "Chronos Project"
                               "Philotic Entanglement" "Profiteering"]}
                 :runner {:deck [(qty "Turntable" 3)]}})
      (score-agenda state :corp (find-card "Chronos Project" (:hand (get-corp))))
      (score-agenda state :corp (find-card "Philotic Entanglement" (:hand (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (core/steal state :runner (find-card "Profiteering" (:hand (get-corp))))
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Philotic Entanglement" (:scored (get-corp))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (take-credits state :runner)
      (play-from-hand state :corp "24/7 News Cycle")
      (click-card state :corp (find-card "Chronos Project" (:scored (get-corp))))
      (is (= "Chronos Project" (:title (first (:rfg (get-corp))))))
      ;; shouldn't work on an agenda in the Runner's scored area
      (is (= 2 (count (:hand (get-runner)))))
      (click-card state :corp (find-card "Philotic Entanglement" (:scored (get-runner))))
      (is (= 2 (count (:hand (get-runner)))))
      ;; resolve 'when scored' ability on swapped Profiteering
      (is (= 8 (:credit (get-corp))))
      (click-card state :corp (find-card "Profiteering" (:scored (get-corp))))
      (click-prompt state :corp "3")
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 3 (count-bad-pub state)))
      (is (= 23 (:credit (get-corp))) "Gained 15 credits"))))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Accelerated Diagnostics" "Cerebral Overwriter" "Shipment from SanSan"
                               "Hedge Fund" "Back Channels"]}})
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
        (click-card state :corp ss)
        (click-prompt state :corp "2")
        (click-card state :corp co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-card state :corp hf)
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")
        (click-card state :corp bc)
        (click-card state :corp (refresh co))
        (is (= 15 (:credit (get-corp))) "Corp gained 6 credits for Back Channels"))))
  (testing "Interaction with Current"
    (do-game
      (new-game {:corp {:deck ["Accelerated Diagnostics" "Cerebral Overwriter"
                               "Enhanced Login Protocol" "Shipment from SanSan"
                               "Hedge Fund"]}})
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
        (click-card state :corp elp)
        (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:corp :current]))))
            "Enhanced Login Protocol active in Current area")
        (click-card state :corp ss)
        (click-prompt state :corp "2")
        (click-card state :corp co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-card state :corp hf)
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")))))

(deftest an-offer-you-can-t-refuse
  ;; An Offer You Can't Refuse - exact card added to score area, not the last discarded one
  (do-game
    (new-game {:corp {:deck ["Celebrity Gift" "An Offer You Can't Refuse"]}})
    (play-from-hand state :corp "An Offer You Can't Refuse")
    (click-prompt state :corp "R&D")
    (core/move state :corp (find-card "Celebrity Gift" (:hand (get-corp))) :discard)
    (is (= 2 (count (:discard (get-corp)))))
    (click-prompt state :runner "No")
    (is (= 1 (:agenda-point (get-corp))) "An Offer the Runner refused")
    (is (= 1 (count (:scored (get-corp)))))
    (is (find-card "An Offer You Can't Refuse" (:scored (get-corp))))
    (is (= 1 (count (:discard (get-corp)))))
    (is (find-card "Celebrity Gift" (:discard (get-corp))))))

(deftest attitude-adjustment
  ;; Attitude Adjustment
  (do-game
    (new-game {:corp {:deck ["Attitude Adjustment"
                             (qty "Hostile Takeover" 2)
                             (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Attitude Adjustment" "Hostile Takeover" "Hostile Takeover"])
    (trash-from-hand state :corp "Hostile Takeover")
    (let [hand (-> (get-corp) :hand count dec)] ;; cuz we're playing Attitude Adjustment
      (play-from-hand state :corp "Attitude Adjustment")
      (is (= (+ 2 hand) (-> (get-corp) :hand count)) "Corp should draw 2 cards"))
    (let [credits (:credit (get-corp))
          hand (-> (get-corp) :hand count)
          discard (-> (get-corp) :discard count)
          deck (-> (get-corp) :deck count)]
      (click-card state :corp (find-card "Hostile Takeover" (:hand (get-corp))))
      (click-card state :corp (find-card "Hostile Takeover" (:discard (get-corp))))
      (is (= (+ 4 credits) (:credit (get-corp))) "Corp should gain 4 [Credits] for two revealed agendas")
      (is (= (dec hand) (-> (get-corp) :hand count)) "One card from HQ is shuffled into R&D")
      (is (= (dec discard) (-> (get-corp) :discard count)) "One card from Archives should be shuffled into R&D")
      (is (= (+ 2 deck) (-> (get-corp) :deck count)) "Corp should draw two cards and shuffle two cards into R&D"))))

(deftest biased-reporting
  ;; Biased Reporting
  (do-game
    (new-game {:corp {:deck ["Biased Reporting"]}
               :runner {:deck [(qty "Fan Site" 5)]}})
    (take-credits state :corp)
    (starting-hand state :runner (repeat 5 "Fan Site"))
    (core/gain state :runner :click 10)
    (dotimes [_ 5]
      (play-from-hand state :runner "Fan Site"))
    (take-credits state :runner)
    (play-from-hand state :corp "Biased Reporting")
    (let [cc (:credit (get-corp))
          rc (:credit (get-runner))]
      (click-prompt state :corp "Resource")
      (click-card state :runner (get-resource state 0))
      (click-prompt state :runner "Done")
      (is (= (inc rc) (:credit (get-runner))) "Runner should gain 1 credit for trashing a Fan Site")
      (is (= (+ (* 4 2) cc) (:credit (get-corp))) "Corp should gain 8 credits for remaining 4 Fan Sites"))))

(deftest big-brother
  ;; Big Brother - Give the Runner 2 tags if already tagged
  (do-game
    (new-game {:corp {:deck ["Big Brother"]}})
    (play-from-hand state :corp "Big Brother")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (core/gain-tags state :runner 1)
    (play-from-hand state :corp "Big Brother")
    (is (= 3 (count-tags state)) "Runner gained 2 tags")))

(deftest biotic-labor
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game {:corp {:deck ["Biotic Labor"]}})
    (play-from-hand state :corp "Biotic Labor")
    (is (= 1 (:credit (get-corp))))
    (is (= 4 (:click (get-corp))) "Spent 1 click to gain 2 additional clicks")))

(deftest blue-level-clearance
  ;; Blue Level Clearance - Gain 5 credits and draw 2 cards
  (do-game
    (new-game {:corp {:deck [(qty "Blue Level Clearance" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Sweeps Week" 2)]}})
    (play-from-hand state :corp "Blue Level Clearance")
    (is (= 8 (:credit (get-corp))) "Gained 5 credits")
    (is (= 1 (:click (get-corp))))
    (is (= 7 (count (:hand (get-corp)))) "Drew 2 cards")))

(deftest building-blocks
  ;; Building Blocks - install and rez a barrier from HQ at no cost
  (testing "Basic behavior"
    (do-game
      (new-game {:corp {:deck ["Building Blocks" "Ice Wall"]}})
      (core/gain state :corp :credit 1)
      (is (= 6 (:credit (get-corp))) "Corp starts with 6 credits")
      (play-from-hand state :corp "Building Blocks")
      (is (= 1 (:credit (get-corp))) "Spent 5 credits on Building Blocks")
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [iw (get-ice state :remote1 0)]
        (is (= 1 (:credit (get-corp))) "Corp spent no credits installing ice")
        (is (rezzed? (refresh iw)) "Ice Wall is installed and rezzed"))))
  (testing "Select invalid card"
    (do-game
      (new-game {:corp {:deck ["Building Blocks" "Hedge Fund" "Cortex Lock"]}})
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Building Blocks")
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Starting prompt is correct")
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Cannot select non-ICE")
      (click-card state :corp (find-card "Cortex Lock" (:hand (get-corp))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-corp))))) "Cannot select non-barrier ICE"))))

(deftest casting-call
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
  (do-game
    (new-game {:corp {:deck [(qty "Casting Call" 2) "Oaktown Renovation"
                             "Improved Tracers" "Hunter"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Hunter" "HQ")
    (let [hunter (get-ice state :hq 0)]
      (core/rez state :corp hunter)
      (is (= 4 (:current-strength (refresh hunter))))
      (play-from-hand state :corp "Casting Call")
      (click-card state :corp (find-card "Improved Tracers" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [imptrac (get-content state :remote1 0)]
        (is (rezzed? (refresh imptrac)) "Improved Tracers is faceup")
        (is (= 4 (:current-strength (refresh hunter))) "Hunter hasn't gained strength")
        (play-from-hand state :corp "Casting Call")
        (click-card state :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (let [oak (get-content state :remote2 0)]
          (core/advance state :corp {:card (refresh oak)})
          (is (= 5 (:credit (get-corp))) "Events on Public agenda work; gained 2 credits from advancing")
          (take-credits state :corp)
          (run-empty-server state "Server 2")
          (click-card state :runner oak)
          (click-prompt state :runner "Steal")
          (is (= 2 (count-tags state)) "Runner took 2 tags from accessing agenda with Casting Call hosted on it"))))))

(deftest cerebral-cast
  ;; Cerebral Cast
  (testing "Runner wins"
    (do-game
      (new-game {:corp {:deck ["Cerebral Cast"]}})
      (play-from-hand state :corp "Cerebral Cast")
      (is (= 3 (:click (get-corp))) "Cerebral Cast precondition not met; card not played")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (zero? (count (:discard (get-runner)))) "Runner took no damage")
      (is (zero? (count-tags state)) "Runner took no tags")))
  (testing "Corp wins"
    (do-game
      (new-game {:corp {:deck [(qty "Cerebral Cast" 2)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "1 brain damage")
      (is (= 1 (count (:discard (get-runner)))) "Runner took a brain damage")
      (is (zero? (count-tags state)) "Runner took no tags from brain damage choice")
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "1 tag")
      (is (= 1 (count (:discard (get-runner)))) "Runner took no additional damage")
      (is (= 1 (count-tags state)) "Runner took a tag from Cerebral Cast choice"))))

(deftest cerebral-static
  ;; Cerebral Static
  (testing "vs Chaos Theory"
    (do-game
      (new-game {:corp {:deck ["Cerebral Static" "Lag Time"]}
                 :runner {:id "Chaos Theory: Wünderkind"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 5 (core/available-mu state)) "CT starts with 5 memory")
      (play-from-hand state :corp "Cerebral Static")
      (is (= 4 (core/available-mu state)) "Cerebral Static causes CT to have 4 memory")
      (play-from-hand state :corp "Lag Time")
      (is (= 5 (core/available-mu state)) "CT 5 memory restored"))))

(deftest closed-accounts
  ;; Closed Accounts - Play if Runner is tagged to make Runner lose all credits
  (do-game
    (new-game {:corp {:deck ["Closed Accounts"]}})
    (play-from-hand state :corp "Closed Accounts")
    (is (and (= 3 (:click (get-corp)))
             (= 5 (:credit (get-runner))))
        "Closed Accounts precondition not met; card not played")
    (core/gain-tags state :runner 1)
    (play-from-hand state :corp "Closed Accounts")
    (is (zero? (:credit (get-runner))) "Runner lost all credits")))

(deftest commercialization
  ;; Commercialization
  (testing "Single advancement token"
    (do-game
      (new-game {:corp {:deck ["Commercialization"
                               "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 1)
      (play-from-hand state :corp "Commercialization")
      (click-card state :corp (refresh (get-ice state :hq 0)))
      (is (= 6 (:credit (get-corp))) "Gained 1 for single advanced ice from Commercialization")))
  (testing "Two advancement tokens"
    (do-game
      (new-game {:corp {:deck ["Commercialization"
                               "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 2)
      (play-from-hand state :corp "Commercialization")
      (click-card state :corp (refresh (get-ice state :hq 0)))
      (is (= 7 (:credit (get-corp))) "Gained 2 for double advanced ice from Commercialization"))))

(deftest complete-image
  ;; Complete Image
  (testing "Correctly guessing"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Priority Requisition"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (-> (get-runner) :discard count zero?) "Runner's heap should be empty")
      (click-prompt state :corp "Sure Gamble")
      (is (seq (:prompt (get-corp))) "Corp guessed right so should have another choice")
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :corp "Sure Gamble")
      (is (seq (:prompt (get-corp))) "Even when the runner has no cards in hand, Corp must choose again")
      (click-prompt state :corp "Sure Gamble")
      (is (empty? (:prompt (get-corp))) "Runner is flatlined so no more choices")
      (is (= 5 (-> (get-runner) :discard count)) "Runner's heap should have 5 cards")))
  (testing "Incorrectly guessing"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Priority Requisition"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (-> (get-runner) :discard count zero?) "Runner's heap should be empty")
      (click-prompt state :corp "Easy Mark")
      (is (empty? (:prompt (get-corp))) "Corp guessed incorrectly so shouldn't have another choice")
      (is (= 1 (-> (get-runner) :discard count)) "Runner's heap should have 1 card")))
  (testing "Not enough agenda points"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Hostile Takeover"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (empty? (:prompt (get-corp))) "Corp shouldn't be able to play Complete Image")))
  (testing "Didn't run last turn"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Priority Requisition"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (empty? (:prompt (get-corp))) "Corp shouldn't be able to play Complete Image"))))

(deftest consulting-visit
  ;; Consulting Visit - Only show single copies of operations corp can afford as choices. Play chosen operation
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Consulting Visit"
                               (qty "Beanstalk Royalties" 2)
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"]}})
      (is (= 5 (:credit (get-corp))))
      (starting-hand state :corp ["Consulting Visit"])
      (play-from-hand state :corp "Consulting Visit")
      (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
            prompt-names (fn [] (map :title (:choices (get-prompt))))]
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (click-prompt state :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (= 6 (:credit (get-corp)))))))
  (testing "Works properly when played with Mumbad City Hall"
    (do-game
      (new-game {:corp {:deck ["Mumbad City Hall"
                               "Beanstalk Royalties"
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"
                               "Consulting Visit"
                               "Mumba Temple"]}})
      (is (= 5 (:credit (get-corp))))
      (starting-hand state :corp ["Mumbad City Hall"])
      (play-from-hand state :corp "Mumbad City Hall" "New remote")
      (let [hall (get-content state :remote1 0)
            get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
            prompt-names (fn [] (map :title (:choices (get-prompt))))]
        (card-ability state :corp hall 0)
        (is (= (list "Consulting Visit" "Mumba Temple" nil) (prompt-names)))
        (click-prompt state :corp (find-card "Consulting Visit" (:deck (get-corp))))
        (is (= 3 (:credit (get-corp))))
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (click-prompt state :corp (find-card "Green Level Clearance" (:deck (get-corp))))
        (is (= 5 (:credit (get-corp))))))))

(deftest death-and-taxes
  ;; Death and Taxes gain credit on runner install, runner trash installed card
  ;; Also regression test for #3160
  (do-game
    (new-game {:corp {:deck ["Death and Taxes" "PAD Campaign"]}
               :runner {:deck ["Aumakua" "DaVinci" "Fall Guy"]}})
    (play-from-hand state :corp "Death and Taxes")
    (is (= (- 5 2) (:credit (get-corp))) "Corp paid 2 to play Death and Taxes")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (let [corp-creds (:credit (get-corp))]
      (trash-from-hand state :runner "DaVinci")
      (is (= corp-creds (:credit (get-corp))) "Corp did not gain credit when runner trashes / discards from hand")
      (play-from-hand state :runner "Aumakua")
      (is (= (inc corp-creds) (:credit (get-corp))) "Corp gained 1 when runner installed Aumakua")
      (play-from-hand state :runner "Fall Guy")
      (is (= (+ 2 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner installed Fall Guy")
      (card-ability state :runner (get-resource state 0) 1)
      (is (= (+ 3 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner trashed Fall Guy")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (is (= (+ 4 corp-creds) (:credit (get-corp))) "Corp gained 1 when runner trashed PAD Campaign"))))

(deftest defective-brainchips
  ;; Defective Brainchips - Do 1 add'l brain damage the first time Runner takes some each turn
  (do-game
    (new-game {:corp {:deck ["Defective Brainchips" "Viktor 1.0"]}
               :runner {:deck [(qty "Sure Gamble" 2) (qty "Shiv" 2)]}})
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
    (new-game {:corp {:deck [(qty "Distract the Masses" 2) (qty "Hedge Fund" 3)]}})
    (starting-hand state :corp ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Distract the Masses" "Distract the Masses"])
    (play-from-hand state :corp "Distract the Masses")
    (click-card state :corp (first (:hand (get-corp))))
    (click-card state :corp (fnext (:hand (get-corp))))
    (click-card state :corp (first (:discard (get-corp))))
    (click-prompt state :corp "Done")
    (is (= 1 (count (:discard (get-corp)))) "1 card still discarded")
    (is (= 1 (count (:deck (get-corp)))) "1 card shuffled into R&D")
    (is (= 1 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-runner))) "Runner gained 2 credits")
    (play-from-hand state :corp "Distract the Masses")
    (click-card state :corp (first (:hand (get-corp))))
    (click-prompt state :corp "Done")
    (click-card state :corp (first (:discard (get-corp))))
    (click-card state :corp (fnext (:discard (get-corp))))
    (is (zero? (count (:discard (get-corp)))) "No cards left in archives")
    (is (= 3 (count (:deck (get-corp)))) "2 more cards shuffled into R&D")
    (is (= 2 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 9 (:credit (get-runner))) "Runner gained 2 credits")))

(deftest diversified-portfolio
  (do-game
    (new-game {:corp {:deck ["Diversified Portfolio"
                             "Paper Wall"
                             (qty "PAD Campaign" 3)]}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Paper Wall" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Diversified Portfolio")
    (is (= 7 (:credit (get-corp))) "Ignored remote with ICE but no server contents")))

(deftest divert-power
  (do-game
    (new-game {:corp {:deck [(qty "Divert Power" 2) "Paper Wall" (qty "Eve Campaign" 3) ]}})
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
        (click-card state :corp (refresh pw))
        (click-card state :corp (refresh ec1))
        (click-prompt state :corp "Done"))
      (testing "Choose a target to rez for -6 cost"
        (click-card state :corp (refresh ec3)))
      (is (rezzed? (refresh ec3)) "Eve Campaign was rezzed")
      (is (= 4 (:credit (get-corp))) "Rezzed Eve Campaign for 0 credits")
      (is (not (rezzed? (refresh pw))) "Paper Wall was derezzed")
      (is (not (rezzed? (refresh ec1))) "First Eve Campaign was derezzed")
      (is (= 16 (get-counters (refresh ec3) :credit)) "Eve gained 16 credits on rez")
      (play-from-hand state :corp "Divert Power")
      (testing "Choose 1 target to derez"
        (click-card state :corp (refresh ec2))
        (click-prompt state :corp "Done"))
      (testing "Choose a target to rez for -3 cost"
        (click-card state :corp (refresh ec1)))
      (is (rezzed? (refresh ec1)) "First Eve Campaign was rezzed")
      (is (zero? (:credit (get-corp))) "Rezzed Eve Campaign for 2 credits")
      (is (not (rezzed? (refresh ec2))) "Second Eve Campaign was derezzed")
      (is (= 32 (get-counters (refresh ec1) :credit)) "First Eve gained 16  more credits on rez"))))

(deftest door-to-door
  ;; Door to Door
  (do-game
    (new-game {:corp {:deck ["Door to Door"]}})
    (play-from-hand state :corp "Door to Door")
    (take-credits state :corp)
    (is (zero? (count-tags state)) "Runner should start with 0 tags")
    (is (= 3 (-> (get-runner) :hand count)) "Runner should start with 3 cards in hand")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count-tags state)) "Runner should gain 1 tag from Door to Door")
    (is (= 3 (-> (get-runner) :hand count)) "Runner should start with 3 cards in hand")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count-tags state)) "Runner should still have 1 tag")
    (is (= 2 (-> (get-runner) :hand count)) "Runner should take 1 meat damage from Door to Door")))

(deftest economic-warfare
  ;; Economic Warfare - If successful run last turn, make the runner lose 4 credits if able
  (do-game
    (new-game {:corp {:deck [(qty "Economic Warfare" 3)]}})
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
    (new-game {:corp {:deck [(qty "Election Day" 7)]}})
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
    (new-game {:corp {:deck [(qty "Enforcing Loyalty" 2)]}
               :runner {:id "Chaos Theory: Wünderkind"
                        :deck ["Inti" "Caldera"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "Caldera")
    (take-credits state :runner)
    (play-from-hand state :corp "Enforcing Loyalty")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-program state 0))
    (is (empty? (:discard (get-runner))) "Can't target Inti; matches Runner faction")
    (click-card state :corp (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))) "Caldera trashed")))

(deftest enhanced-login-protocol
  ;; Enhanced Login Protocol
  (testing "First click run each turn costs an additional click"
    (do-game
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Employee Strike"]}})
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
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Sneakdoor Beta"]}})
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
      (new-game {:corp {:id "New Angeles Sol: Your News"
                        :deck ["Enhanced Login Protocol" "Breaking News"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enhanced Login Protocol" (:discard (get-corp))))
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner has 1 click")))
  (testing "Run event don't cost additional clicks"
    (do-game
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Out of the Ashes"]}})
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (play-from-hand state :runner "Out of the Ashes")
      (click-prompt state :runner "Archives")
      (is (= 3 (:click (get-runner))) "Runner doesn't spend 1 additional click to run with a run event")
      (run-successful state)
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")
      (run-successful state)
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "No") ; Out of the Ashes prompt
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make a run")))
  (testing "Works when played on the runner's turn"
    (do-game
      (new-game {:corp {:id "New Angeles Sol: Your News"
                        :deck ["Enhanced Login Protocol"
                               "Breaking News"]}
                 :runner {:deck ["Hades Shard"]}})
      (trash-from-hand state :corp "Breaking News")
      (take-credits state :corp)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Hades Shard")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
      (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "Enhanced Login Protocol is in play")
      (is (= 3 (:click (get-runner))) "Runner has 3 clicks")
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")))
(testing "Doesn't fire if already run when played on the runner's turn"
  (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck ["Enhanced Login Protocol"
                             "Breaking News"]}
               :runner {:deck ["Hades Shard"]}})
    (trash-from-hand state :corp "Breaking News")
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :runner "No action")
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Hades Shard")
    (card-ability state :runner (get-resource state 0) 0)
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (click-card state :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
    (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "Enhanced Login Protocol is in play")
    (is (= 2 (:click (get-runner))) "Runner has 2 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-runner))) "Runner doesn't spend 1 additional click to make a run"))))

(deftest exchange-of-information
  ;; Exchange of Information
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"]}})
      (score-agenda state :corp (find-card "Market Research" (:hand (get-corp))))
      (score-agenda state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (count-tags state)) "Runner gained 2 tags")
      (take-credits state :corp)
      (is (zero? (count-tags state)) "Runner lost 2 tags")
      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (find-card "Explode-a-palooza" (:hand (get-corp))))
      (take-credits state :runner)
      (is (= 4 (:agenda-point (get-runner))))
      (is (= 3 (:agenda-point (get-corp))))
      (core/gain-tags state :runner 1)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 3 (:agenda-point (get-runner))))
      (is (= 4 (:agenda-point (get-corp))))))
  (testing "Swapping a just scored Breaking News keeps the tags"
    (do-game
      (new-game {:corp {:deck ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"]}})
      (take-credits state :corp)
      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (find-card "Explode-a-palooza" (:hand (get-corp))))
      (take-credits state :runner)
      (score-agenda state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (count-tags state)) "Runner gained 2 tags")
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 2 (count-tags state)) "Still has tags after swap and before end of turn")
      (take-credits state :corp)
      (is (= 3 (:agenda-point (get-runner))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 2 (count-tags state)) "Runner does not lose tags at end of turn")))
  (testing "Swapping a 15 Minutes still keeps the ability. #1783"
    (do-game
      (new-game {:corp {:deck [(qty "Exchange of Information" 2) "15 Minutes"
                               "Project Beale"]}})
      (score-agenda state :corp (find-card "15 Minutes" (:hand (get-corp))))
      (take-credits state :corp)
      (core/gain-tags state :runner 1)
      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (take-credits state :runner)
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "15 Minutes" (:scored (get-corp))))
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
      (click-card state :corp (find-card "15 Minutes" (:scored (get-runner))))
      (click-card state :corp (find-card "Project Beale" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      ;; shuffle back into R&D from corp's scored area
      (let [fifm (get-scored state :corp 0)]
        (card-ability state :corp fifm 0))
      (is (= "15 Minutes" (:title (first (:deck (get-corp))))))))
  (testing "Swapping a Mandatory Upgrades gives the Corp an additional click per turn. #1687"
    (do-game
      (new-game {:corp {:deck [(qty "Exchange of Information" 2) "Mandatory Upgrades"
                               "Global Food Initiative"]}})
      (score-agenda state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
      (take-credits state :corp)
      (core/gain-tags state :runner 1)
      (core/steal state :runner (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (take-credits state :runner)
      (is (= 3 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (is (= 3 (:click (get-corp))))
      (is (= 3 (:click-per-turn (get-corp))))
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Mandatory Upgrades" (:scored (get-runner))))
      (click-card state :corp (find-card "Global Food Initiative" (:scored (get-corp))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (is (= 3 (:click (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 4 (:click (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))))
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Global Food Initiative" (:scored (get-runner))))
      (click-card state :corp (find-card "Mandatory Upgrades" (:scored (get-corp))))
      (is (= 3 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (is (= 2 (:click (get-corp))))
      (is (= 3 (:click-per-turn (get-corp))))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 3 (:click (get-corp))))
      (is (= 3 (:click-per-turn (get-corp)))))))

(deftest fast-break
  ;; Fast Break
  (do-game
    (new-game {:corp {:deck ["Fast Break" "Hostile Takeover" "Keegan Lane" "Haas Arcology AI"
                             "Research Station" (qty "Ice Wall" 10)]}
               :runner {:deck [(qty "Fan Site" 3)]}})
    (starting-hand state :corp ["Fast Break" "Hostile Takeover" "Keegan Lane"
                                "Haas Arcology AI" "Research Station"])
    (take-credits state :corp)
    (dotimes [_ 3]
      (play-from-hand state :runner "Fan Site"))
    (take-credits state :runner)
    (play-and-score state "Hostile Takeover")
    (is (= 3 (count (get-scored state :runner))) "Runner should have 3 agendas in score area")
    (play-from-hand state :corp "Fast Break")
    (let [hand (-> (get-corp) :hand count)
          credits (:credit (get-corp))]
      (click-prompt state :corp "3")
      (is (= (+ hand 3) (-> (get-corp) :hand count)) "Corp should draw 3 cards from Fast Break")
      (click-prompt state :corp "New remote")
      (click-card state :corp (find-card "Keegan Lane" (:hand (get-corp))))
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (is (= (dec credits) (:credit (get-corp))) "Corp should pay 1 credit to install second Ice Wall"))
    (core/move state :corp (find-card "Fast Break" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Fast Break")
    (let [hand (-> (get-corp) :hand count)
          credits (:credit (get-corp))]
      (click-prompt state :corp "0")
      (is (= hand (-> (get-corp) :hand count)) "Corp should draw no cards as they're allowed to draw no cards")
      (is (some #{"Server 2"} (:choices (prompt-map :corp))) "Corp should be able to choose existing remotes")
      (click-prompt state :corp "Server 2")
      (click-card state :corp (find-card "Haas Arcology AI" (:hand (get-corp))))
      (click-card state :corp (find-card "Research Station" (:hand (get-corp))))
      (is (= 2 (count (get-content state :remote2))) "Corp can't choose Research Station to install in a remote")
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-prompt state :corp "Done")
      (is (= (- credits 2) (:credit (get-corp))) "Corp should pay 2 credits to install third Ice Wall")
      (is (empty? (:prompt (get-corp))) "Corp should be able to stop installing early"))))

(deftest focus-group
  ;; Focus Group
  (testing "Regular scenario - can afford"
    (do-game
      (new-game {:corp {:deck ["Focus Group" "Enigma"]}
                 :runner {:deck ["Sure Gamble" "Dirty Laundry" "Corroder" "Datasucker" "Turntable"]}})
      (play-from-hand state :corp "Focus Group")
      (is (not (= "Focus Group" (-> (get-corp) :discard first :title))) "Runner didn't steal an agenda last turn")
      (take-credits state :corp)
      (run-empty-server state :rd)
      (take-credits state :runner)
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Focus Group")
      (click-prompt state :corp "Event")
      (is (= 5 (:credit (get-corp))))
      (click-prompt state :corp "2")
      (is (= 3 (:credit (get-corp))))
      (let [enigma (get-ice state :hq 0)]
        (is (zero? (get-counters (refresh enigma) :advancement)))
        (click-card state :corp enigma)
        (is (= 2 (get-counters (refresh enigma) :advancement))))))
  (testing "0 valid targets, gracefully end"
    (do-game
      (new-game {:corp {:deck ["Focus Group"]}
                 :runner {:deck ["Sure Gamble" "Dirty Laundry" "Corroder" "Datasucker" "Black Orchestra"]}
                 :options {:start-as :runner}})
      (run-empty-server state :rd)
      (take-credits state :runner)
      (play-from-hand state :corp "Focus Group")
      (is (= 5 (:credit (get-corp))))
      (click-prompt state :corp "Hardware")
      (is (empty? (:prompt (get-corp))) "No hardware in Runner's Grip so just end the interaction")
      (is (= 5 (:credit (get-corp))))))
  (testing "Can't afford to pay to place advancement tokens, gracefully end"
    (do-game
      (new-game {:corp {:deck ["Focus Group"]}
                 :runner {:deck ["Liberated Account" "Daily Casts" "Bhagat" "Datasucker" "Turntable"]}
                 :options {:start-as :runner}})
      (run-empty-server state :rd)
      (take-credits state :runner)
      (play-from-hand state :corp "Focus Group")
      (click-prompt state :corp "Resource")
      (core/lose state :corp :credit 3)
      (is (= 2 (:credit (get-corp))))
      (click-prompt state :corp "3") ;; want to place 3 advancement tokens
      (is (empty? (:prompt (get-corp))) "Corp can't afford to pay so just end the interaction")
      (is (= 2 (:credit (get-corp))) "Didn't pay to place advancement tokens"))))

(deftest foxfire
  ;; Foxfire
  (do-game
    (new-game {:corp {:deck [(qty "Foxfire" 2)]}
               :runner {:deck ["Dyson Mem Chip" "Ice Carver"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Dyson Mem Chip")
    (play-from-hand state :runner "Ice Carver")
    (take-credits state :runner)
    (play-from-hand state :corp "Foxfire")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-hardware state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Corp should trash Dyson Mem Chip from winning Foxfire trace")
    (play-from-hand state :corp "Foxfire")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (is (= 2 (-> (get-runner) :discard count)) "Corp should trash Ice Carver from winning Foxfire trace")))

(deftest fully-operational
  ;; Fully Operational
  (testing "Gain 2 credits"
    (do-game
      (new-game {:corp {:deck ["Fully Operational"]}})
      (play-from-hand state :corp "Fully Operational")
      (is (= 4 (:credit (get-corp))) "Cost 1 credit to play")
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 6 (:credit (get-corp))) "Corp gained 2 credits")
      (is (empty? (:prompt (get-corp))) "No lingering prompt after making single choice")))
  (testing "Draw 2 cards"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) "Fully Operational"]}})
      (starting-hand state :corp ["Fully Operational"])
      (play-from-hand state :corp "Fully Operational")
      (click-prompt state :corp "Draw 2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp drew 2 cards")
      (is (empty? (:prompt (get-corp))) "No lingering prompt after making single choice")))
  (testing "Extra choices from remote servers"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Breaker Bay Grid" 2) "Fully Operational"]}})
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Breaker Bay Grid" "Server 1")
      (play-from-hand state :corp "Breaker Bay Grid" "Server 2")
      (play-from-hand state :corp "Fully Operational")
      (dotimes [_ 3]
        (click-prompt state :corp "Gain 2 [Credits]"))
      (is (= 10 (:credit (get-corp))) "Corp gained 6 credits")
      (is (empty? (:prompt (get-corp))) "No lingering prompt after making repeated choices"))))

(deftest game-changer
  (letfn [(game-changer-test [num-agenda]
            (do-game
              (new-game {:corp {:deck ["Game Changer" "Hostile Takeover"]}
                         :runner {:deck [(qty "Fan Site" num-agenda)]}})
              (take-credits state :corp)
              (core/gain state :runner :click num-agenda)
              (dotimes [_ num-agenda]
                (play-from-hand state :runner "Fan Site"))
              (take-credits state :runner)
              (play-and-score state "Hostile Takeover")
              (is (= num-agenda (count (get-scored state :runner)))
                  (str "Runner should have " (utils/quantify num-agenda "Fan Site") " in play"))
              (let [clicks (:click (get-corp))
                    n (dec num-agenda)]
                (play-from-hand state :corp "Game Changer")
                (is (= (+ n clicks) (:click (get-corp))) (str "Corp should gain " (utils/quantify n "click")))
                (is (= 1 (-> (get-corp) :rfg count)) "Game Changer should be in rfg zone now"))))]
    (doall (map game-changer-test (range 5)))))

(deftest game-over
  ;; Game Over
  (testing "Can't play unless Runner stole an agenda last turn"
    (do-game
      (new-game {:corp {:deck ["Project Beale" "Game Over"]}})
      (play-from-hand state :corp "Game Over")
      (is (not (= "Loot Box" (-> (get-corp) :discard first :title))) "Runner didn't steal an agenda last turn")))
  (testing "Trash all non-Icebreaker programs, hardware and resources not trashed"
    (do-game
      (new-game {:corp {:deck ["Project Beale" "Game Over"]}
                 :runner {:deck ["Nyashia" "Takobi" "Misdirection" "Gordian Blade" "Astrolabe" "Daily Casts"]}})
      (starting-hand state :runner ["Nyashia" "Takobi" "Misdirection" "Gordian Blade" "Astrolabe" "Daily Casts"])
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (core/gain state :runner :click 3)
      (core/gain state :runner :credit 12)
      (play-from-hand state :runner "Nyashia")
      (play-from-hand state :runner "Takobi")
      (play-from-hand state :runner "Misdirection")
      (play-from-hand state :runner "Gordian Blade")
      (play-from-hand state :runner "Astrolabe")
      (play-from-hand state :runner "Daily Casts")
      (take-credits state :runner)
      (play-from-hand state :corp "Game Over")
      (click-prompt state :corp "Program")
      (click-card state :runner (find-card "Gordian Blade" (:program (:rig (get-runner))))) ;; should do nothing
      (click-card state :runner (find-card "Astrolabe" (:hardware (:rig (get-runner))))) ;; should do nothing
      (click-card state :runner (find-card "Daily Casts" (:resource (:rig (get-runner))))) ;; should do nothing
      (is (= 5 (:credit (get-runner))))
      (click-card state :runner (find-card "Misdirection" (:program (:rig (get-runner))))) ;; Misdirection
      (is (= 2 (:credit (get-runner))) "Prevent the trash of Misdirection by paying 3 credits")
      (is (= 2 (-> (get-runner) :discard count)) "2 programs trashed")
      (is (some? (find-card "Nyashia" (:discard (get-runner)))) "Nyashia trashed")
      (is (some? (find-card "Takobi" (:discard (get-runner)))) "Takobi trashed")
      (is (= "Misdirection" (-> (get-runner) :rig :program first :title)) "Misdirection not trashed")
      (is (= "Gordian Blade" (-> (get-runner) :rig :program second :title)) "Gordian Blade not trashed")
      (is (= "Astrolabe" (-> (get-runner) :rig :hardware first :title)) "Astrolabe not trashed")
      (is (= "Daily Casts" (-> (get-runner) :rig :resource first :title)) "Daily Casts not trashed")
      (is (= 1 (count-bad-pub state)))))
  (testing "Can't afford to prevent any trashes"
    (do-game
      (new-game {:corp {:deck ["Project Beale" "Game Over"]}
                 :runner {:deck ["Nyashia" "Takobi"]}})
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (play-from-hand state :runner "Nyashia")
      (play-from-hand state :runner "Takobi")
      (take-credits state :runner)
      (play-from-hand state :corp "Game Over")
      (click-prompt state :corp "Program")
      (is (empty? (:prompt (get-runner))) "No prevention prompt for the Runner")
      (is (= 2 (:credit (get-runner))))
      (is (= 2 (-> (get-runner) :discard count)) "2 programs trashed")
      (is (some? (find-card "Nyashia" (:discard (get-runner)))) "Nyashia trashed")
      (is (some? (find-card "Takobi" (:discard (get-runner)))) "Takobi trashed")
      (is (= 1 (count-bad-pub state))))))

(deftest hangeki
  ;; Hangeki
  (doseq [choice ["Yes" "No"]]
    (testing (str "choosing to " (when (= choice "No") "not ") "access card")
      (do-game
        (new-game {:corp {:deck ["Hostile Takeover" "Dedicated Response Team" "Hangeki"]}})
        (play-from-hand state :corp "Hostile Takeover" "New remote")
        (play-from-hand state :corp "Dedicated Response Team" "New remote")
        (take-credits state :corp)
        (run-on state :remote2)
        (run-successful state)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (take-credits state :runner)
        (play-from-hand state :corp "Hangeki")
        (click-card state :corp (get-content state :remote1 0))
        (click-prompt state :runner choice)
        (if (= "Yes" choice)
          (do (click-prompt state :runner "Steal")
              (is (= 1 (:agenda-point (get-runner))) "Runner should steal Hostile Takeover")
              (is (= 1 (-> (get-corp) :rfg count)) "Hangeki should be removed from the game"))
          (do (is (empty? (:prompt (get-runner))) "Runner should have no more prompts as access ended")
              (is (= -1 (:agenda-point (get-runner))) "Runner should add Hangeki to their score area worth -1 agenda point")
              (is (zero? (-> (get-corp) :rfg count)) "Hangeki shouldn't be removed from the game")))))))

(deftest hard-hitting-news
  ;; Hard-Hitting News
  (do-game
    (new-game {:corp {:deck ["Hard-Hitting News"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should start with 3 clicks")
    (play-from-hand state :corp "Hard-Hitting News")
    (is (zero? (:click (get-corp))) "Playing Hard-Hitting News should lose all remaining clicks")
    (is (zero? (count-tags state)) "Runner should start with 0 tags")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 4 (count-tags state)) "Runner should gain 4 tags from losing Hard-Hitting News trace")))

(deftest hatchet-job
  ;; Hatchet Job - Win trace to add installed non-virtual to grip
  (do-game
    (new-game {:corp {:deck ["Hatchet Job"]}
               :runner {:deck ["Upya" "Ghost Runner"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Ghost Runner")
    (play-from-hand state :runner "Upya")
    (take-credits state :runner)
    (play-from-hand state :corp "Hatchet Job")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (is (empty? (:hand (get-runner))) "Can't choose virtual card")
    (is (seq (:prompt (get-corp))))
    (click-card state :corp (get-program state 0))
    (is (= 1 (count (:hand (get-runner)))) "Upya returned to grip")))

(deftest hedge-fund
  (do-game
    (new-game)
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Hedge Fund")
    (is (= 9 (:credit (get-corp))))))

(deftest hellion-alpha-test
  ;; Hellion Alpha Test
  (do-game
    (new-game {:corp {:deck [(qty "Hellion Alpha Test" 2)]}
               :runner {:deck ["Daily Casts"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (play-from-hand state :corp "Hellion Alpha Test")
    (is (zero? (-> (get-runner) :deck count)) "Runner should have no cards in Stack")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (is (= 1 (-> (get-runner) :deck count)) "Runner should have 1 card in Stack from losing Hellion Alpha Test trace")
    (is (= "Daily Casts" (-> (get-runner) :deck first :title))
        "Runner should have Daily Casts on top of Stack from losing Hellion Alpha Test trace")
    (take-credits state :corp)
    (core/draw state :runner)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (play-from-hand state :corp "Hellion Alpha Test")
    (is (zero? (count-bad-pub state)) "Corp should start with 0 bad publicity")
    (click-prompt state :corp "0")
    (click-prompt state :runner "2")
    (is (= 1 (count-bad-pub state)) "Corp should gain 1 bad publicity from losing Hellion Alpha Test trace")))

(deftest hellion-beta-test
  ;; Hellion Beta Test
  (testing "Winning Trace - Trashing 2 cards"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team" "Hellion Beta Test"]}
                 :runner {:deck ["Daily Casts" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "Dyson Mem Chip")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (is (zero? (-> (get-runner) :discard count)) "Runner's heap should be empty")
      (play-from-hand state :corp "Hellion Beta Test")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-resource state 0))
      (click-card state :corp (get-hardware state 0))
      (is (= 2 (-> (get-runner) :discard count)) "Runner should have 2 cards in heap after losing Hellion Beta Test trace")))
  (testing "Losing trace - Gaining bad publicity"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team" "Hellion Beta Test"]}
                 :runner {:deck ["Daily Casts" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "Dyson Mem Chip")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (is (zero? (count-bad-pub state)) "Corp should start with 0 bad publicity")
      (play-from-hand state :corp "Hellion Beta Test")
      (click-prompt state :corp "0")
      (click-prompt state :runner "2")
      (is (= 1 (count-bad-pub state)) "Corp should gain 1 bad publicity from losing Hellion Beta Test trace"))))

(deftest high-profile-target
  (testing "when the runner has no tags"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))
  (testing "when the runner has one tag"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (core/gain-tags state :runner 1)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (count (:hand (get-runner)))) "Runner has 3 cards in hand")))
  (testing "when the runner has two tags"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (core/gain-tags state :runner 2)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))
  (testing "When the runner has three tags, gg"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 10)]}})
      (core/gain-tags state :runner 3)
      (play-from-hand state :corp "High-Profile Target")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest housekeeping
  ;; Housekeeping - Runner must trash a card from Grip on first install of a turn
  (do-game
    (new-game {:corp {:deck ["Housekeeping"]}
               :runner {:deck [(qty "Cache" 2) "Fall Guy" "Mr. Li"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Fall Guy")
    (take-credits state :runner)
    (play-from-hand state :corp "Housekeeping")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (click-card state :runner (find-card "Mr. Li" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "Fall Guy prevention didn't trigger")
    (is (= 1 (count (:discard (get-runner)))) "Card trashed")
    (play-from-hand state :runner "Cache")
    (is (empty? (:prompt (get-runner))) "Housekeeping didn't trigger on 2nd install")))

(deftest invasion-of-privacy
  ;; Invasion of Privacy - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Invasion of Privacy" 3)]}
               :runner {:deck [(qty "Sure Gamble" 2) "Fall Guy" (qty "Cache" 2)]}})
    (core/gain state :corp :click 3 :credit 6)
    ;; trash 2 cards
    (play-from-hand state :corp "Invasion of Privacy")
    (click-prompt state :corp "0") ; default trace
    (click-prompt state :runner "0") ; Runner won't match
    (is (= 5 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map :title (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner))))
      (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))))
    ;; able to trash 2 cards but only 1 available target in Runner's hand
    (play-from-hand state :corp "Invasion of Privacy")
    (click-prompt state :corp "0") ; default trace
    (click-prompt state :runner "0") ; Runner won't match
    (is (= 3 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map :title (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (click-prompt state :corp (find-card "Fall Guy" (:hand (get-runner))))
      (is (empty? (get-in @state [:corp :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-runner)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :corp "Invasion of Privacy")
    (click-prompt state :corp "0") ; default trace
    (click-prompt state :runner "2") ; Runner matches
    (is (= 1 (count-bad-pub state)))))

(deftest ipo
  ;; IPO - credits with Terminal operations
  (do-game
    (new-game {:corp {:deck ["IPO"]}})
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "IPO")
    (is (= 13 (:credit (get-corp))))
    (is (zero? (:click (get-corp))) "Terminal ends turns")))

(deftest kill-switch
  ;; Kill Switch
  (do-game
    (new-game {:corp {:deck ["Kill Switch" (qty "Hostile Takeover" 2)]}})
    (play-from-hand state :corp "Kill Switch")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 brain damage")
    (play-and-score state "Hostile Takeover")
    (click-prompt state :corp "Hostile Takeover")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner should get 1 brain damage from Kill Switch after Corp scores an agenda")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 2 (:brain-damage (get-runner))) "Runner should get 1 brain damage from Kill Switch after accecssing an agenda")))

(deftest lag-time
  (do-game
    (new-game {:corp {:deck ["Lag Time" "Vanilla" "Lotus Field"]}})
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
    (new-game {:corp {:deck ["Lateral Growth" "Breaking News"]}})
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Lateral Growth")
    (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Breaking News" (:title (get-content state :remote1 0)))
        "Breaking News installed by Lateral Growth")
    (is (= 7 (:credit (get-corp))))))

(deftest manhunt
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game {:corp {:deck ["Manhunt" (qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Manhunt")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (is (:prompt (get-corp)) "Manhunt trace initiated")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count-tags state)) "Runner took 1 tag")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (run-empty-server state "HQ")
    (is (empty? (:prompt (get-corp))) "No Manhunt trace on second run")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest market-forces
  (testing "Full test"
    (letfn [(market-forces-credit-test
              [{:keys [tag-count runner-creds expected-credit-diff]}]
              (testing (str "when the runner has " tag-count " tags and " runner-creds " credits")
                (do-game
                  (new-game {:corp {:deck [(qty "Market Forces" 6)]}})
                  (swap! state assoc-in [:corp :credit] 0)
                  (swap! state assoc-in [:runner :credit] runner-creds)
                  (core/gain-tags state :runner tag-count)
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
      (new-game {:corp {:deck [(qty "Market Forces" 6)]}})
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
    (new-game {:corp {:deck ["Mass Commercialization"
                             (qty "Ice Wall" 3)]}})
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

(deftest mca-informant
  ;; MCA Informant
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["MCA Informant"]}
               :runner {:hand ["Beth Kilrain-Chang"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Beth Kilrain-Chang")
    (take-credits state :runner)
    (play-from-hand state :corp "MCA Informant")
    (click-card state :corp "Beth Kilrain-Chang")
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (card-side-ability state :runner (-> (get-resource state 0) :hosted first) 0)
      (is (nil? (get-resource state 0)) "Beth should now be trashed")
      (is (= (- credits 2) (:credit (get-runner))) "Runner should pay 2 credits to trash MCA"))))

(deftest medical-research-fundraiser
  ;; Medical Research Fundraiser - runner gains 8creds, runner gains 3creds
  (do-game
    (new-game {:corp {:deck ["Medical Research Fundraiser"]}})
    (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-from-hand state :corp "Medical Research Fundraiser")
    (is (= 10 (:credit (get-corp))) "Corp gains 8 credits")
    (is (= 8 (:credit (get-runner))) "Runner gains 3 credits")))

(deftest midseason-replacements
  ;; Midseason Replacements - Trace to give Runner tags after they steal an agenda
  (do-game
    (new-game {:corp {:deck ["Midseason Replacements" "Breaking News"]}})
    (play-from-hand state :corp "Midseason Replacements")
    (is (= 3 (:click (get-corp))) "Midseason precondition not met; Corp not charged a click")
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (let [bn (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Stole Breaking News")
      (take-credits state :runner)
      (play-from-hand state :corp "Midseason Replacements")
      (click-prompt state :corp "0") ; default trace
      (click-prompt state :runner "0") ; Runner won't match
      (is (= 6 (count-tags state)) "Runner took 6 tags"))))

(deftest mushin-no-shin
  ;; Mushin No Shin - Add 3 advancements to a card; prevent rez/score of that card the rest of the turn
  (do-game
    (new-game {:corp {:deck [(qty "Mushin No Shin" 2) "Ronin" "Profiteering"]}})
    (play-from-hand state :corp "Mushin No Shin")
    (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
    (let [ronin (get-content state :remote1 0)]
      (is (= 3 (get-counters (refresh ronin) :advancement)) "3 advancements placed on Ronin")
      (core/rez state :corp (refresh ronin))
      (is (not (rezzed? (refresh ronin))) "Ronin did not rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh ronin))
      (is (rezzed? (refresh ronin)) "Ronin now rezzed")
      (play-from-hand state :corp "Mushin No Shin")
      (click-card state :corp (find-card "Profiteering" (:hand (get-corp))))
      (let [prof (get-content state :remote2 0)]
        (core/score state :corp (refresh prof))
        (is (empty? (:scored (get-corp))) "Profiteering not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/score state :corp (refresh prof))
        (click-prompt state :corp "0")
        (is (= 1 (:agenda-point (get-corp))) "Profiteering was able to be scored")))))

(deftest mutate
  ;; Mutate - trash a rezzed piece of ice, install and rez one from R&D
  (testing "Basic operation"
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma is installed")
      (is (rezzed? (get-ice state :hq 0)) "Enigma is rezzed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Installed card name was logged")))
  (testing "No ice in R&D"
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :hq 0))
      (is (empty? (get-ice state :hq)) "No ice installed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")))
  (testing "Remote server"
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "New remote")
      (core/rez state :corp (get-ice state :remote1 0))
      (is (= 1 (count (get-ice state :remote1))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :remote1 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :remote1 0))
      (is (= 1 (count (get-ice state :remote1))) "1 ice installed")
      (is (= "Enigma" (:title (get-ice state :remote1 0))) "Enigma is installed")
      (is (rezzed? (get-ice state :remote1 0)) "Enigma is rezzed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Installed card name was logged"))))

(deftest neural-emp
  ;; Neural EMP - Play if Runner made a run the previous turn to do 1 net damage
  (do-game
    (new-game {:corp {:deck ["Neural EMP"]}})
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
    (new-game {:corp {:deck ["Oversight AI" "Archer"]}})
    (play-from-hand state :corp "Archer" "R&D")
    (let [archer (get-ice state :rd 0)]
      (play-from-hand state :corp "Oversight AI")
      (click-card state :corp archer)
      (is (rezzed? (refresh archer)))
      (is (= 4 (:credit (get-corp))) "Archer rezzed at no credit cost")
      (is (= "Oversight AI" (:title (first (:hosted (refresh archer)))))
          "Archer hosting OAI as a condition"))))

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game {:corp {:deck ["Patch" "Vanilla"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Patch")
    (click-card state :corp (get-ice state :hq 0))
    (is (= 2 (:current-strength (get-ice state :hq 0))) "Vanilla at 2 strength")))

(deftest paywall-implementation
  ;; Paywall Implementation - Gain 1 credit for every successful run
  (do-game
    (new-game {:corp {:deck ["Paywall Implementation"]}})
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
    (new-game {:corp {:deck ["Peak Efficiency" (qty "Paper Wall" 3) "Wraparound"]}})
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

(deftest power-grid-overload
  ;; Power Grid Overload
  (do-game
    (new-game {:corp {:deck ["Power Grid Overload"]}
               :runner {:deck ["Dyson Mem Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Dyson Mem Chip")
    (run-empty-server state :rd)
    (take-credits state :runner)
    (play-from-hand state :corp "Power Grid Overload")
    (click-prompt state :corp "3")
    (click-prompt state :runner "0")
    (click-card state :corp (get-hardware state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Dyson Mem Chip should be in heap after Runner loses Power Grid Overload trace")))

(deftest power-shutdown
  ;; Power Shutdown - Trash cards from R&D to force Runner to trash a program or hardware
  (testing "Default behavior"
    (do-game
      (new-game {:corp {:deck ["Power Shutdown"]
                        :hand [(qty "Power Shutdown" 2) (qty "Hive" 3)]}
                 :runner {:deck ["Grimoire" "Cache"]}})
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
      (click-prompt state :corp "2")
      (is (= 3 (count (:discard (get-corp)))) "2 cards trashed from R&D")
      (is (= 1 (count (:deck (get-corp)))) "1 card remaining in R&D")
      (click-card state :runner (get-hardware state 0)) ; try targeting Grimoire
      (is (empty? (:discard (get-runner))) "Grimoire too expensive to be targeted")
      (click-card state :runner (get-program state 0))
      (is (= 1 (count (:discard (get-runner)))) "Cache trashed")))
  (testing "No installed runner cards"
    (do-game
      (new-game {:corp {:deck ["Power Shutdown"]}})
      (take-credits state :corp)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (play-from-hand state :corp "Power Shutdown"))))

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game {:corp {:deck ["Precognition" "Caprice Nisei" "Adonis Campaign"
                             "Quandary" "Jackson Howard" "Global Food Initiative"]}})
    (starting-hand state :corp ["Precognition"])
    (play-from-hand state :corp "Precognition")
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (click-prompt state :corp "Start over")
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (click-prompt state :corp "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 3)
                               "Preemptive Action"]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (second (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))
  (testing "forces you to take 3 if there are three, and removes itself from game"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 3)
                               (qty "Preemptive Action" 1)]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (= 3 (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))
  (testing "Shuffles all archives cards into R&D if Archives has less than 3 cards, and removes itself from game"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 2)
                               (qty "Preemptive Action" 1)]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp))))))))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Runner tags on a card
  (do-game
    (new-game {:corp {:deck ["Psychographics" "Project Junebug"]}})
    (core/gain-tags state :runner 4)
    (play-from-hand state :corp "Project Junebug" "New remote")
    (let [pj (get-content state :remote1 0)]
      (play-from-hand state :corp "Psychographics")
      (click-prompt state :corp "4")
      (click-card state :corp pj)
      (is (= 1 (:credit (get-corp))) "Spent 4 credits")
      (is (= 4 (get-counters (refresh pj) :advancement)) "Junebug has 4 advancements"))))

(deftest psychokinesis
  ;; Pyschokinesis - Terminal Event (end the turn); Look at R&D, install an Asset, Agenda, or Upgrade in a Remote Server
  (do-game
    (new-game {:corp {:deck [(qty "Psychokinesis" 3) "Caprice Nisei" "Adonis Campaign"
                             "Global Food Initiative" "Mwanza City Grid"]}})
    (starting-hand state :corp ["Psychokinesis" "Psychokinesis" "Psychokinesis"])
    ;; Test installing an Upgrade
    (play-from-hand state :corp "Psychokinesis")
    (is (not-any? #{"Mwanza City Grid"} (map :title (-> (get-corp) :prompt first :choices)))
        "Mwanza City Grid is not on the list of installable cards")
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Caprice Nisei" (:title (get-content state :remote1 0)))
        "Caprice Nisei installed by Psychokinesis")
    ;; Test installing an Asset
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Adonis Campaign" (:title (get-content state :remote2 0)))
        "Adonis Campaign installed by Psychokinesis")
    ;; Test installing an Agenda
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Global Food Initiative" (:title (get-content state :remote3 0)))
        "Global Food Initiative installed by Psychokinesis")
    ;; Test selecting "None"
    (core/gain state :corp :click 1)
    (core/move state :corp (find-card "Psychokinesis" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp "None")
    (is (nil? (:title (get-content state :remote4 0)))
        "Nothing is installed by Psychokinesis")))

(deftest punitive-counterstrike
  ;; Punitive Counterstrike - deal meat damage equal to printed agenda points
  (do-game
    (new-game {:corp {:deck ["Global Food Initiative" "Punitive Counterstrike"]}})
    (play-from-hand state :corp "Global Food Initiative" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Runner scored 2 points")
    (take-credits state :runner)
    (play-from-hand state :corp "Punitive Counterstrike")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (empty? (:hand (get-runner))) "Runner took 3 meat damage")))

(deftest red-level-clearance
  ;; Red Level Clearance
  (testing "Basic test"
    (do-game
      (new-game {:corp {:hand [(qty "Red Level Clearance" 2) "Hedge Fund" "Merger" "Plan B"]
                        :deck [(qty "Beanstalk Royalties" 5)]}})
      (play-from-hand state :corp "Red Level Clearance")
      (let [credits (:credit (get-corp))]
        (click-prompt state :corp "Gain 2 [Credits]")
        (is (= (+ credits 2) (:credit (get-corp)))))
      (let [hand (count (:hand (get-corp)))]
        (click-prompt state :corp "Draw 2 cards")
        (is (= (+ hand 2) (count (:hand (get-corp))))))
      (play-from-hand state :corp "Red Level Clearance")
      (let [clicks (:click (get-corp))]
        (click-prompt state :corp "Gain [Click]")
        (is (= (inc clicks) (:click (get-corp)))))
      (click-prompt state :corp "Install a non-agenda from hand")
      (click-card state :corp "Merger")
      (is (find-card "Merger" (:hand (get-corp))))
      (click-card state :corp "Hedge Fund")
      (is (find-card "Merger" (:hand (get-corp))))
      (click-card state :corp "Plan B")
      (click-prompt state :corp "New remote")
      (is (not (find-card "Plan B" (:hand (get-corp)))))))
  (testing "Can't choose same option twice. Issue #4150"
    (do-game
      (new-game {:corp {:deck [(qty "Beanstalk Royalties" 5)]
                        :hand ["Red Level Clearance"]}})
      (play-from-hand state :corp "Red Level Clearance")
      (is (prompt-is-type? state :runner :waiting))
      (is (= 4 (count (:choices (prompt-map :corp)))))
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 3 (count (:choices (prompt-map :corp)))))
      (is (= ["Draw 2 cards" "Gain [Click]" "Install a non-agenda from hand"]
             (vec (:choices (prompt-map :corp)))))
      (click-prompt state :corp "Gain [Click]")
      (is (empty? (:prompt (get-runner))) "Runner should have no more prompt"))))

(deftest red-planet-couriers
  ;; Red Planet Couriers - Move all advancements on cards to 1 advanceable card
  (do-game
    (new-game {:corp {:deck ["Red Planet Couriers" (qty "Ice Wall" 2)
                             "GRNDL Refinery" "Government Takeover"]}})
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
      (click-card state :corp gt)
      (is (zero? (get-counters (refresh gr) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw1) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw2) :advancement)) "Advancements removed")
      (is (= 6 (get-counters (refresh gt) :advancement)) "Gained 6 advancements"))))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card trashed from HQ
  (do-game
    (new-game {:corp {:deck [(qty "Reuse" 2) "Hive" "IQ"
                             "Ice Wall"]}})
    (play-from-hand state :corp "Reuse")
    (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
    (click-card state :corp (find-card "Hive" (:hand (get-corp))))
    (click-card state :corp (find-card "IQ" (:hand (get-corp))))
    (click-prompt state :corp "Done")
    (is (= 4 (count (:discard (get-corp)))) "3 cards trashed plus operation played")
    (is (= 11 (:credit (get-corp))) "Gained 6 credits")
    (is (= 1 (:click (get-corp))) "Spent 2 clicks")))

(deftest reverse-infection
  ;; Reverse Infection
  (testing "Gain 2 credits"
    (do-game
      (new-game {:corp {:deck ["Reverse Infection"]}})
      (play-from-hand state :corp "Reverse Infection")
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 7 (:credit (get-corp))) "Corp gained 2 credits")))
  (testing "Purge virus counters, counts both Runner and Corp side (Sandstone)")
  (do-game
    (new-game {:corp {:deck ["Reverse Infection" "Sandstone"]}
               :runner {:deck ["Virus Breeding Ground" "Datasucker" (qty "Sure Gamble" 4)]
                        :hand ["Virus Breeding Ground" "Datasucker"]}})
    (play-from-hand state :corp "Sandstone" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Datasucker")
    (take-credits state :runner)
    (core/add-counter state :corp (get-ice state :hq 0) :virus 3)
    (core/add-counter state :runner (get-resource state 0) :virus 3)
    (core/add-counter state :runner (get-program state 0) :virus 3)
    (play-from-hand state :corp "Reverse Infection")
    (click-prompt state :corp "Purge virus counters")
    (is (= 7 (:credit (get-corp))) "Corp did not gain credits")
    (is (zero? (get-counters (get-ice state :hq 0) :virus)) "Viruses purged from Sandstone")
    (is (zero? (get-counters (get-resource state 0) :virus)) "Viruses purged from VBG")
    (is (zero? (get-counters (get-program state 0) :virus)) "Viruses purged from Datasucker")
    (is (= 3 (count (:discard (get-runner)))) "Three cards trashed from stack")))

(deftest riot-suppression
  ;; Riot Suppression - lose 3 clicks or take 1 brain damage
  (testing "Take 1 brain damage"
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no brain damage")
      (click-prompt state :runner "Yes")
      (is (= 1 (count (:discard (get-runner)))) "1 card lost to brain damage")
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppestion removed from game")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has all clicks the following turn")))
  (testing "Lose 3 clicks"
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no brain damage")
      (click-prompt state :runner "No")
      (is (empty? (:discard (get-runner))) "Runner discard statys empty")
      (is (zero? (:brain-damage (get-runner))) "Runner takes no brain damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppestion removed from game")
      (take-credits state :corp)
      (is (= 1 (:click (get-runner))) "Runner has 3 fewer clicks following turn"))))

(deftest rolling-brownout
  ;; Rolling Brownout - Increase cost of events/operations by 1, gain 1c on first Runner event of turn
  (do-game
    (new-game {:corp {:deck ["Rolling Brownout" "Beanstalk Royalties"
                             "Domestic Sleepers"]}
               :runner {:deck [(qty "Easy Mark" 3)]}})
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
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner "Easy Mark")
    (is (= 12 (:credit (get-runner))) "Easy Mark netted 3c after Brownout trashed")))

(deftest sacrifice
  ;; Sacrifice - Remove BP for each agenda point sacrificed and gain a credit
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Sacrifice"]}})
      (play-and-score state "Hostile Takeover")
      (is (= 1 (count-bad-pub state)) "Hostile Takeover gives one bad publicity")
      (let [ht (get-scored state :corp 0)
            credits (:credit (get-corp))]
        (play-from-hand state :corp "Sacrifice")
        (click-card state :corp ht)
        (is (zero? (count-bad-pub state)) "Sacrifice removes one bad publicity for forfeiting Hostile Takeover")
        (is (= (inc credits) (:credit (get-corp))) "Corp gained one credit from removing one bad pub with Sacrifice"))))
  (testing "Play restrictions"
    (do-game
      (new-game {:corp {:deck ["Standoff" "Hostile Takeover" "Sacrifice"]}})
      (play-and-score state "Standoff")
      (core/gain state :corp :bad-publicity 1)
      (play-from-hand state :corp "Sacrifice")
      (is (= 2 (count (:hand (get-corp)))) "Can not play Sacrifice with no 1+ agenda in score area")
      (play-and-score state "Hostile Takeover")
      ;; Remove BP
      (core/gain state :corp :bad-publicity -2)
      (play-from-hand state :corp "Sacrifice")
      (is (= 1 (count (:hand (get-corp)))) "Can not play Sacrifice with no bad publicity in score area"))))

(deftest salem-s-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Salem's Hospitality" 3)]}
               :runner {:deck [(qty "I've Had Worse" 3) "Faust"
                               "Levy AR Lab Access"]}})
    (play-from-hand state :corp "Salem's Hospitality")
    (is (= 5 (count (:hand (get-runner)))))
    (click-prompt state :corp "I've Had Worse")
    (is (= 2 (count (:hand (get-runner)))))
    (play-from-hand state :corp "Salem's Hospitality")
    (click-prompt state :corp "Plascrete Carapace")
    (is (= 2 (count (:hand (get-runner)))))))

(deftest scorched-earth
  ;; Scorched Earth
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (core/gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))
  (testing "not tagged"
    (do-game
      (new-game {:corp {:deck ["Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (play-from-hand state :corp "Scorched Earth")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))
  (testing "flatline"
    (do-game
      (new-game {:corp {:deck [(qty "Scorched Earth" 10)]}})
      (core/gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest sea-source
  ;; SEA Source
  (do-game
    (new-game {:corp {:deck ["SEA Source"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (is (zero? (count-tags state)) "Runner should start with 0 tags")
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count-tags state)) "Runner should get 1 tag from losing SEA Source trace")))

(deftest secure-and-protect
  ;; Secure and Protect
  (testing "With ice in deck"
    (do-game
      (new-game {:corp {:hand ["Secure and Protect"]
                        :deck [(qty "Hedge Fund" 10) "Ice Wall" "Afshar"]}})
      (play-from-hand state :corp "Secure and Protect")
      (is (= ["Afshar" "Ice Wall"] (vec (sort (prompt-titles :corp)))))
      (click-prompt state :corp "Ice Wall")
      (is (nil? (get-ice state :hq 0)) "No ice installed on HQ")
      (click-prompt state :corp "HQ")
      (is (get-ice state :hq 0) "Ice Wall is installed on HQ")))
  (testing "With no ice in deck"
    (do-game
      (new-game {:corp {:hand ["Secure and Protect"]
                        :deck [(qty "Hedge Fund" 10)]}})
      (play-from-hand state :corp "Secure and Protect")
      (is (nil? (seq (:prompt (get-corp)))) "Corp should have no prompts")))
  (testing "With varying install costs"
    (letfn [(sp-test [amt]
              (do-game
                (new-game {:corp {:hand ["Secure and Protect" (qty "Ice Wall" amt)]
                                  :deck [(qty "Hedge Fund" 10) "Ice Wall" "Afshar"]}})
                (core/gain state :corp :click amt :credit amt)
                (doseq [_ (range amt)]
                  (play-from-hand state :corp "Ice Wall" "HQ"))
                (play-from-hand state :corp "Secure and Protect")
                (let [credits (:credit (get-corp))]
                  (click-prompt state :corp "Ice Wall")
                  (click-prompt state :corp "HQ")
                  (is (= credits (:credit (get-corp))) "Corp should get 3 credit install discount"))))]
      (doall (map sp-test (range 4))))))

(deftest self-growth-program
  ;; Self-Growth Program - Add 2 installed cards to grip if runner is tagged
  (do-game
    (new-game {:corp {:deck ["Self-Growth Program"]}
               :runner {:deck ["Clone Chip" "Inti"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Inti")
    (take-credits state :runner)
    (play-from-hand state :corp "Self-Growth Program")
    (is (= 3 (:click (get-corp))) "Self-Growth Program precondition not met; card not played")
    (core/gain-tags state :runner 1)
    (is (zero? (count (:hand (get-runner)))) "Runner hand is empty")
    (let [inti (get-program state 0)
          cc (get-hardware state 0)]
      (play-from-hand state :corp "Self-Growth Program")
      (click-card state :corp inti)
      (click-card state :corp cc))
    (is (= 2 (count (:hand (get-runner)))) "2 cards returned to hand")
    (is (zero? (count (get-program state))) "No programs installed")
    (is (zero? (count (get-hardware state))) "No hardware installed")))

(deftest service-outage
  ;; Service Outage
  (testing "First click run each turn costs a credit"
    (do-game
     (new-game {:corp {:deck ["Service Outage"]}
                :runner {:deck ["Employee Strike"]}})
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
     (new-game {:corp {:deck ["Service Outage"]}
                :runner {:deck ["Sneakdoor Beta"]}})
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
           "Runner doesn't spend 1 credit to make a run"))))
  (testing "First run event each turn costs an additional credit"
    (do-game
     (new-game {:corp {:deck ["Service Outage"]}
                :runner {:deck [(qty "Out of the Ashes" 2)]}})
     (play-from-hand state :corp "Service Outage")
     (take-credits state :corp)
     (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
     (play-from-hand state :runner "Out of the Ashes")
     (click-prompt state :runner "Archives")
     (is (= 3 (:credit (get-runner)))
         "Runner spends 1 additional credit to run with a run event")
     (run-successful state)
     (run-on state :archives)
     (is (= 3 (:credit (get-runner)))
         "Runner doesn't spend 1 credit to make a run")
     (run-successful state)
     (take-credits state :runner)
     (take-credits state :corp)
     (click-prompt state :runner "No") ; Out of the Ashes prompt
     (core/lose state :runner :credit 4)
     (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
     (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
     (play-from-hand state :runner "Out of the Ashes")))
  (testing "Works when played on the runner's turn"
    (do-game
     (new-game {:corp {:id "New Angeles Sol: Your News"
                       :deck ["Service Outage"
                              "Breaking News"]}
                :runner {:deck ["Hades Shard"]}})
     (trash-from-hand state :corp "Breaking News")
     (take-credits state :corp)
     (core/gain state :runner :credit 3)
     (play-from-hand state :runner "Hades Shard")
     (card-ability state :runner (get-resource state 0) 0)
     (click-prompt state :runner "Steal")
     (click-prompt state :corp "Yes")
     (click-card state :corp (find-card "Service Outage" (:hand (get-corp))))
     (is (find-card "Service Outage" (:current (get-corp)))
         "Service Outage is in play")
     (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
     (run-on state :archives)
     (is (zero? (:credit (get-runner)))
         "Runner spends 1 additional credit to make a run")))
  (testing "Doesn't fire if already run when played on the runner's turn"
    (do-game
     (new-game {:corp {:id "New Angeles Sol: Your News"
                       :deck ["Service Outage"
                              "Breaking News"]}
                :runner {:deck ["Hades Shard"]}})
     (trash-from-hand state :corp "Breaking News")
     (take-credits state :corp)
     (run-on state :hq)
     (run-successful state)
     (click-prompt state :runner "No action")
     (core/gain state :runner :credit 3)
     (play-from-hand state :runner "Hades Shard")
     (card-ability state :runner (get-resource state 0) 0)
     (click-prompt state :runner "Steal")
     (click-prompt state :corp "Yes")
     (click-card state :corp (find-card "Service Outage" (:hand (get-corp))))
     (is (find-card "Service Outage" (:current (get-corp)))
         "Service Outage is in play")
     (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
     (run-on state :archives)
     (is (= 1 (:credit (get-runner)))
         "Runner doesn't spend 1 additional credit to make a run")))
  (testing "trashed and reinstalled on steal doesn't double remove penalty"
    (do-game
     (new-game {:corp {:id "New Angeles Sol: Your News"
                       :deck ["Service Outage"
                              "Breaking News"]}})
     (play-from-hand state :corp "Breaking News" "New remote")
     (play-from-hand state :corp "Service Outage")
     (take-credits state :corp)
     (run-on state :remote1)
     (run-successful state)
     (click-prompt state :runner "Steal")
     (click-prompt state :corp "Yes")
     (click-card state :corp (find-card "Service Outage" (:discard (get-corp))))
     (take-credits state :runner)
     (take-credits state :corp)
     (is (= 7 (:credit (get-runner))) "Runner has 7 credits")
     (run-on state :archives)
     (is (= 6 (:credit (get-runner))) "Runner spends 1 credit to make a run")))
  (testing "Interaction with temp credits"
    (do-game
      (new-game {:corp {:deck ["Service Outage"]}
                 :runner {:deck [(qty "Stimhack" 3)]}})
      (play-from-hand state :corp "Service Outage")
      (take-credits state :corp)
      (play-from-hand state :runner "Stimhack")
      (click-prompt state :runner "HQ")
      (run-jack-out state)
      (is (= 4 (:credit (get-runner))) (str "An additional real cred spent, not Stimhack money"))
      (run-on state "R&D")
      (is (= 4 (:credit (get-runner))) "Second run fine"))))

(deftest shipment-from-sansan
  ;; Shipment from SanSan - placing advancements
  (do-game
    (new-game {:corp {:deck [(qty "Shipment from SanSan" 3) (qty "Ice Wall" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iwall (get-ice state :hq 0)]
      (play-from-hand state :corp "Shipment from SanSan")
      (click-prompt state :corp "2")
      (click-card state :corp iwall)
      (is (= 5 (:credit (get-corp))))
      (is (= 2 (get-counters (refresh iwall) :advancement))))))

(deftest snatch-and-grab
  ;; Snatch and Grab
  (do-game
    (new-game {:corp {:deck [(qty "Snatch and Grab" 2)]}
               :runner {:deck ["Scrubber"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scrubber")
    (take-credits state :runner)
    (is (zero? (count-tags state)) "Runner should start with 0 tags")
    (play-from-hand state :corp "Snatch and Grab")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (click-prompt state :runner "Yes")
    (is (= 1 (count-tags state)) "Runner should get 1 tag from losing Snatch and Grab trace and opting to take the tag")
    (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
    (play-from-hand state :corp "Snatch and Grab")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (click-prompt state :runner "No")
    (is (= 1 (-> (get-runner) :discard count)) "Scrubber should be in Runner's heap after losing Snatch and Grab trace")))

(deftest stock-buy-back
  ;; Stock Buy-Back - Gain 3c for every agenda in Runner's area
  (do-game
    (new-game {:corp {:deck [(qty "Hostile Takeover" 2) (qty "Stock Buy-Back" 3)]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (is (= 2 (count (:scored (get-runner)))))
    (play-from-hand state :corp "Stock Buy-Back")
    (is (= 11 (:credit (get-corp))))))

(deftest sub-boost
  ;; Sub Boost - Give ICE Barrier
  (do-game
    (new-game {:corp {:deck ["Sub Boost" "Quandary"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (let [qu (get-ice state :hq 0)]
      (core/rez state :corp qu)
      (is (not (has-subtype? (refresh qu) "Barrier")) "Quandry starts without Barrier")
      (is (= 1 (count (:subroutines (refresh qu)))) "Quandry has 1 subroutine")
      (play-from-hand state :corp "Sub Boost")
      (click-card state :corp (refresh qu))
      (is (has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (has-subtype? (refresh qu) "Barrier") "Quandary ICE Barrier")
      (is (= 2 (count (:subroutines (refresh qu)))) "Quandry gains a subroutine"))))

(deftest subcontract
  ;; Subcontract
  (testing "Don't allow second operation until damage prevention completes"
    (do-game
      (new-game {:corp {:deck [(qty "Scorched Earth" 2) "Subcontract"]}
                 :runner {:deck ["Plascrete Carapace"]}})
      (take-credits state :corp)
      (core/gain-tags state :runner 1)
      (play-from-hand state :runner "Plascrete Carapace")
      (take-credits state :runner)
      (play-from-hand state :corp "Subcontract")
      (click-card state :corp (find-card "Scorched Earth" (:hand (get-corp))))
      (is (and (= 1 (count (:prompt (get-corp)))) (= :waiting (-> (get-corp) :prompt first :prompt-type)))
          "Corp does not have Subcontract prompt until damage prevention completes")
      (click-prompt state :runner "Done")
      (is (not-empty (:prompt (get-corp))) "Corp can now play second Subcontract operation")))
  (testing "interaction with Terminal operations"
    (do-game
      (new-game {:corp {:deck [(qty "Hard-Hitting News" 2) "Subcontract"]}})
      (core/gain-tags state :runner 1)
      (take-credits state :corp)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (play-from-hand state :corp "Subcontract")
      (click-card state :corp (find-card "Hard-Hitting News" (:hand (get-corp))))
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 5 (count-tags state)) "Runner has 5 tags")
      (is (empty? (:prompt (get-corp))) "Corp does not have a second Subcontract selection prompt"))))

(deftest subliminal-messaging
  ;; Subliminal Messaging - Playing/trashing/milling will all prompt returning to hand
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 3)]}
                 :runner {:id "Noise: Hacker Extraordinaire"
                          :deck [(qty "Cache" 3) "Utopia Shard"]}})
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
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (is (= 3 (count (:hand (get-corp)))) "All 3 Subliminals returned to HQ")
      (core/move state :corp (find-card "Subliminal Messaging" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Utopia Shard")
      (let [utopia (get-resource state 0)]
        (card-ability state :runner utopia 0))
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
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
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 2) "Archived Memories"]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Archived Memories")
      (click-card state :corp (find-card "Subliminal Messaging" (:discard (get-corp))))
      (is (= 2 (count (:discard (get-corp)))))
      (is (= 1 (count (:hand (get-corp)))))
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "No")
      (is (empty? (get-in @state [:corp :prompt])) "Only 1 Subliminal prompt")
      (play-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (is (empty? (get-in @state [:corp :prompt]))
          "Only 2 Subliminal prompts - there will be a third if flag not cleared")))
  (testing "Scenario involving Subliminal being reshuffled into R&D with Jackson"
    (do-game
      (new-game {:corp {:deck ["Subliminal Messaging" "Jackson Howard"]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp)
      (let [jhow (get-content state :remote1 0)]
        (core/rez state :corp jhow)
        (card-ability state :corp jhow 1)
        (click-card state :corp (find-card "Subliminal Messaging" (:discard (get-corp))))
        (click-prompt state :corp "Done")
        (is (zero? (count (:discard (get-corp)))))
        (is (= 1 (count (:rfg (get-corp))))))
      (take-credits state :runner)
      (play-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (is (= 1 (count (:hand (get-corp)))) "Subliminal returned to HQ")
      (is (empty? (get-in @state [:corp :prompt]))
          "Subliminal prompt cleared - there will be a second prompt if flag not cleared")))
  (testing "Runner made run, ensure game asks again next turn"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 2)]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (trash-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (run-on state "R&D")
      (run-jack-out state)
      (take-credits state :runner)
      (is (empty? (get-in @state [:corp :prompt])) "No prompt here because runner made a run last turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (is (= 2 (count (:hand (get-corp)))) "Both Subliminals returned to HQ")
      (is (zero? (count (:discard (get-corp)))) "No Subliminals in Archives")))
  (testing "User declines to return to hand, ensure game asks again next turn"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 2)]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (trash-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "No")
      (click-prompt state :corp "No")
      (is (zero? (count (:hand (get-corp)))) "Neither Subliminal returned to HQ")
      (is (= 2 (count (:discard (get-corp)))) "Both Subliminals in Archives")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (is (= 2 (count (:hand (get-corp)))) "Both Subliminals returned to HQ")
      (is (zero? (count (:discard (get-corp)))) "No Subliminals in Archives"))))

(deftest success
  ;; Success
  (testing "Works with bad publicity"
    (do-game
      (new-game {:corp {:deck ["NAPD Contract" "Project Beale" "Success"]}})
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
        (click-card state :corp (get-scored state :corp 0))
        (is (= "NAPD Contract" (:title (first (:rfg (get-corp))))))
        (click-card state :corp (refresh beale))
        (is (= 13 (get-counters (refresh beale) :advancement)))
        (core/score state :corp {:card (refresh beale)})
        (is (= 7 (:agenda-point (get-corp)))))))
  (testing "Works with public agendas"
    (do-game
      (new-game {:corp {:deck ["Oaktown Renovation" "Vanity Project" "Success"]}})
      (core/gain state :corp :click 1)
      (score-agenda state :corp (find-card "Vanity Project" (:hand (get-corp))))
      (is (= 4 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (= 5 (:credit (get-corp))))
      (play-from-hand state :corp "Success")
      (click-card state :corp (get-scored state :corp 0))
      (is (= "Vanity Project" (:title (first (:rfg (get-corp))))))
      (let [oaktown (get-content state :remote1 0)]
        (click-card state :corp (refresh oaktown))
        (is (= 6 (get-counters (refresh oaktown) :advancement)))
        (is (= 19 (:credit (get-corp))) "Gain 2 + 2 + 2 + 2 + 3 + 3 = 14 credits for advancing Oaktown")
        (core/score state :corp {:card (refresh oaktown)})
        (is (= 2 (:agenda-point (get-corp)))))))
  (testing "interaction with Jemison, regression test for issue #2704"
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Success"
                               "High-Risk Investment"
                               "Government Takeover"]}})
      (core/gain state :corp :click 1)
      (score-agenda state :corp (find-card "High-Risk Investment" (:hand (get-corp))))
      (play-from-hand state :corp "Government Takeover" "New remote")
      (play-from-hand state :corp "Success")
      (click-card state :corp (get-in (get-corp) [:scored 0]))
      (let [gto (get-content state :remote1 0)]
        ;; Prompt for Jemison
        (click-card state :corp (refresh gto))
        (is (= 4 (get-counters (refresh gto) :advancement)) "Added 4 counters from Jemison trigger")
        ;; Prompt for Success
        (click-card state :corp (refresh gto))
        (is (= (+ 4 5) (get-counters (refresh gto) :advancement)) "Advance 5 times from Success")))))

(deftest successful-demonstration
  ;; Successful Demonstration - Play if only Runner made unsuccessful run last turn; gain 7 credits
  (do-game
    (new-game {:corp {:deck ["Successful Demonstration"]}})
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
      (new-game {:corp {:deck ["Restructured Datapool" "Surveillance Sweep" "Data Raven"]}
                 :runner {:deck ["Scrubbed"]}})
      (is (zero? (count-tags state)) "Runner should start with no tags")
      (play-from-hand state :corp "Surveillance Sweep")
      (play-and-score state "Restructured Datapool")
      (let [rd-scored (get-scored state :corp 0)]
        (card-ability state :corp rd-scored 0)
        (is (not= :waiting (-> (get-corp) :prompt first :prompt-type)) "Surveillance Sweep only works during a run")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (count-tags state)) "Runner should gain a tag from Restructured Datapool ability"))
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Data Raven" "HQ")
      (take-credits state :corp)
      (let [dr (get-ice state :hq 0)]
        (core/rez state :corp (refresh dr))
        (run-on state :hq)
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :corp :waiting) "During a run, Corp should wait on Runner first")
        (click-prompt state :runner "0")
        (click-prompt state :corp "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (play-from-hand state :runner "Scrubbed")
        (run-on state :hq)
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :runner :waiting) "Runner should now be waiting on Corp")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state))))
  (testing "trace during run after stealing an agenda"
    (do-game
      (new-game {:corp {:hand ["Surveillance Sweep" "Breaking News" "Forced Connection" "Data Raven"]
                        :credits 20}})
      (core/gain state :corp :click 4)
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
        (is (prompt-is-type? state :corp :waiting) "During a run, Corp should wait on Runner first")
        (click-prompt state :runner "0")
        (click-prompt state :corp "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (click-card state :runner bn)
        (click-prompt state :runner "Steal")
        (click-card state :runner fc)
        (is (prompt-is-type? state :runner :waiting) "After steal, Surveillance Sweep leaves play and Runner waits on Corp"))))
  (testing "Interaction with Citadel Sanctuary and Sol"
    (do-game
      (new-game {:corp {:hand ["Hostile Takeover"]
                        :discard ["Surveillance Sweep"]
                        :id "New Angeles Sol: Your News"}
                 :runner {:hand ["Citadel Sanctuary"]
                          :tags 1}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Citadel Sanctuary")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-card state :corp "Surveillance Sweep")
      (core/end-turn state :runner nil)
      (is (prompt-is-type? state :runner :waiting) "Runner is waiting on Corp"))))

(deftest targeted-marketing
  ;; Targeted Marketing
  (do-game
    (new-game {:corp {:deck ["Targeted Marketing"]}})
    (play-from-hand state :corp "Targeted Marketing")
    (click-prompt state :corp "Sure Gamble")
    (take-credits state :corp)
    (let [credits (:credit (get-corp))]
      (play-from-hand state :runner "Sure Gamble")
      (is (= (+ 10 credits) (:credit (get-corp))) "Corp gains 10 credits from Runner playing named card"))))

(deftest the-all-seeing-i
  (testing "Counts number of cards if one card is prevented trashed with fall guy"
    (do-game
      (new-game {:corp {:deck ["The All-Seeing I"]}
                 :runner {:deck ["Fall Guy" (qty "Same Old Thing" 2)]}})
      (letfn [(res [] (count (get-in (get-runner) [:rig :resource])))]
        (take-credits state :corp)
        (play-from-hand state :runner "Same Old Thing")
        (play-from-hand state :runner "Fall Guy")
        (play-from-hand state :runner "Same Old Thing")
        (take-credits state :runner)
        (play-from-hand state :corp "The All-Seeing I")
        (is (= 1 (count (:hand (get-corp)))) "Corp could not play All Seeing I when runner was not tagged")
        (core/gain-tags state :runner 1)
        (play-from-hand state :corp "The All-Seeing I")
        (let [fall-guy (get-resource state 1)]
          (card-ability state :runner fall-guy 0))
        (click-prompt state :runner "Done")
        (is (= 1 (res)) "One installed resource saved by Fall Guy")
        (is (= 2 (count (:discard (get-runner)))) "Two cards in heap"))))
  (testing "Checks that All-seeing I does not double-trash hosted cards, trashes hosted cards"
    (do-game
      (new-game {:corp {:deck ["The All-Seeing I"]}
                 :runner {:deck [(qty "Fall Guy" 2) "Off-Campus Apartment"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)
            fg1 (get-in (get-runner) [:hand 0])
            fg2 (get-in (get-runner) [:hand 1])]
        (card-ability state :runner oca 0)
        (click-card state :runner fg1)
        (card-ability state :runner oca 0)
        (click-card state :runner fg2))
      (core/gain-tags state :runner 1)
      (take-credits state :runner)
      (play-from-hand state :corp "The All-Seeing I")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "Done")
      (let  [fall-guy (find-card "Fall Guy" (core/all-active-installed state :runner))]
        (card-ability state :runner fall-guy 0))
      (click-prompt state :runner "Done") ;; This assumes hosted cards get put in trash-list before host
      (is (= 1 (count (core/all-active-installed state :runner))) "One installed card (Off-Campus)")
      (is  (= 2 (count (:discard (get-runner)))) "Two cards in heap")))
  (testing "should not trash Jarogniew Mercs if there are other installed resources"
    (do-game
      (new-game {:corp {:deck [(qty "The All-Seeing I" 4)]}
                 :runner {:deck [(qty "Jarogniew Mercs" 2) (qty "Same Old Thing" 2)]}})
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
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Threat Assessment" 3) "Adonis Campaign"]}
                 :runner {:deck ["Desperado" "Corroder"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Desperado")
      (play-from-hand state :runner "Corroder")
      (take-credits state :runner)
      (is (zero? (count-tags state)) "Runner starts with 0 tags")
      (play-from-hand state :corp "Threat Assessment")
      (click-card state :corp (find-card "Desperado" (-> (get-runner) :rig :hardware)))
      (click-prompt state :runner "Take 2 tags")
      (is (= 2 (count-tags state)) "Runner took 2 tags")
      (is (= 1 (count (-> (get-runner) :rig :hardware))) "Didn't trash Desperado")
      (is (= "Threat Assessment" (:title (first (:rfg (get-corp))))) "Threat Assessment removed from game")
      (play-from-hand state :corp "Threat Assessment")
      (click-card state :corp (find-card "Corroder" (-> (get-runner) :rig :program)))
      (click-prompt state :runner "Move Corroder")
      (is (= 2 (count-tags state)) "Runner didn't take tags")
      (is (= "Corroder" (:title (first (:deck (get-runner))))) "Moved Corroder to the deck")
      (is (= 2 (count (:rfg (get-corp)))))
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Threat Assessment")
      (is (empty? (:prompt (get-corp))) "Threat Assessment triggered with no trash")))
  (testing "interaction with Hippo. Issue #4049"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Threat Assessment"]}
                 :runner {:hand ["Hippo" "Corroder"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Hippo")
      (run-on state :hq)
      (let [iw (get-ice state :hq 0)]
        (core/rez state :corp (refresh iw))
        (card-ability state :runner (get-hardware state 0) 0)
        (is (empty? (get-ice state :hq)) "Ice Wall is gone"))
      (core/jack-out state :runner nil)
      (take-credits state :runner)
      (play-from-hand state :corp "Threat Assessment")
      (click-card state :corp "Corroder")
      (click-prompt state :runner "Move Corroder")
      (is (zero? (count-tags state)) "Runner didn't take tags")
      (is (empty? (:prompt (get-corp)))))))

(deftest threat-level-alpha
  ;; Threat Level Alpha - Win trace to give tags = Runner tags; or 1 tag if 0
  (do-game
    (new-game {:corp {:deck [(qty "Threat Level Alpha" 2)]}})
    (core/gain state :corp :click 2)
    (core/gain state :corp :credit 2)
    (is (zero? (count-tags state)))
    (play-from-hand state :corp "Threat Level Alpha")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count-tags state)) "Runner took 1 tag because they had 0")
    (core/gain-tags state :runner 2)
    (play-from-hand state :corp "Threat Level Alpha")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 6 (count-tags state)) "Runner took 3 tag because they had 3")))

(deftest transparency-initiative
  ;; Transparency Initiative - Full test
  (do-game
    (new-game {:corp {:deck ["Transparency Initiative" "Oaktown Renovation"
                             "Project Atlas" "Hostile Takeover" "Casting Call"]}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (play-from-hand state :corp "Casting Call")
    (click-card state :corp (find-card "Project Atlas" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [oaktown (get-content state :remote1 0)
          atlas (get-content state :remote2 0)
          hostile (get-content state :remote3 0)]
      (play-from-hand state :corp "Transparency Initiative")
      (click-card state :corp (refresh oaktown))
      ;; doesn't work on face-up agendas
      (is (zero? (count (:hosted (refresh oaktown)))))
      (click-card state :corp (refresh atlas))
      (is (= 1 (count (:hosted (refresh atlas)))) "Casting Call")
      ;; works on facedown agenda
      (click-card state :corp (refresh hostile))
      (is (= 1 (count (:hosted (refresh hostile)))))
      ;; gains Public subtype
      (is (has-subtype? (refresh hostile) "Public"))
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
    (new-game {:corp {:deck ["Trojan Horse" "Dedicated Response Team"]}
               :runner {:deck ["Wyrm"]}})
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Wyrm")
    (run-empty-server state :remote1)
    (take-credits state :runner)
    (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
    (play-from-hand state :corp "Trojan Horse")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-program state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Wyrm should be in heap after Runner loses Trojan Horse trace")))

(deftest ultraviolet-clearance
  ;; Ultraviolet Clearance - Only allow agenda to be installed in remote servers
  (do-game
    (new-game {:corp {:deck [(qty "Ultraviolet Clearance" 2) "Improved Tracers" "Remote Enforcement"]}})
    (core/gain state :corp :click 3 :credit 7)
    (play-from-hand state :corp "Ultraviolet Clearance")
    (click-card state :corp (find-card "Improved Tracers" (:hand (get-corp))))
    (is (= 1 (count (:choices (first (:prompt (get-corp)))))) "Wrong number of options in install prompt")
    (click-prompt state :corp "New remote")
    (play-from-hand state :corp "Ultraviolet Clearance")
    (click-card state :corp (find-card "Remote Enforcement" (:hand (get-corp))))
    (is (= 2 (count (:choices (first (:prompt (get-corp)))))) "Wrong number of options in install prompt")))

(deftest under-the-bus
  ;; Under the Bus
  (testing "Basic test"
    (do-game
    (new-game {:corp {:deck ["Under the Bus"]}
               :runner {:deck ["Film Critic"]}})
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Film Critic")
    (take-credits state :runner)
    (is (= 1 (count (get-resource state))) "Runner has 1 resource installed")
    (is (zero? (count-bad-pub state)) "Corp has no bad pub")
    (play-from-hand state :corp "Under the Bus")
    (click-card state :corp (get-resource state 0))
    (is (empty? (get-resource state)) "Runner has no resource installed")
    (is (= 1 (count (:discard (get-runner)))) "Runner has 1 trashed card")
    (is (= 1 (count-bad-pub state)) "Corp takes 1 bad pub")))
 (testing "With replacement effects"
    (do-game
    (new-game {:corp {:deck ["Ice Wall"]
                      :hand ["Under the Bus"]}
               :runner {:deck ["Film Critic" "Khusyuk"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Film Critic")
    (play-from-hand state :runner "Khusyuk")
    (run-successful state)
    (click-prompt state :runner "Replacement effect")
    (click-prompt state :runner "1 [Credit]: 1 card")
    (click-prompt state :runner "Ice Wall")
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (is (= 1 (count (get-resource state))) "Runner has 1 resource installed")
    (is (zero? (count-bad-pub state)) "Corp has no bad pub")
    (play-from-hand state :corp "Under the Bus")
    (click-card state :corp (get-resource state 0))
    (is (empty? (get-resource state)) "Runner has no resource installed")
    (is (= 2 (count (:discard (get-runner)))) "Runner has 2 trashed card")
    (is (= 1 (count-bad-pub state)) "Corp takes 1 bad pub"))) )

(deftest wake-up-call
  ;; Wake Up Call
  (testing "should fire after using En Passant to trash ice"
    (do-game
      (new-game {:corp {:deck ["Enigma" "Wake Up Call"]}
                 :runner {:deck ["En Passant" "Maya"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Maya")
      (run-on state :hq)
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "Corp starts with no discards")
      (play-from-hand state :runner "En Passant")
      (click-card state :runner (get-ice state :hq 0))
      (is (= 1 (count (:discard (get-corp)))) "Corp trashes installed ice")
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))) "Runner starts with 1 trashed card (En Passant)")
      (play-from-hand state :corp "Wake Up Call")
      (click-card state :corp (get-hardware state 0))
      (click-prompt state :runner "Trash Maya")
      (is (= 2 (count (:discard (get-runner)))) "Maya is trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Wake Up Call is removed from the game")))
  (testing "Should fire after Embezzle. Issue #4188"
    (do-game
      (new-game {:corp {:deck ["Wake Up Call"]
                        :hand [(qty "Hedge Fund" 4)]
                        :credit 10}
                 :runner {:hand ["Embezzle" "Desperado"]
                          :credit 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Desperado")
      (play-from-hand state :runner "Embezzle")
      (run-successful state)
      (click-prompt state :runner "Operation")
      (take-credits state :runner)
      (play-from-hand state :corp "Wake Up Call")
      (click-card state :corp "Desperado")
      (click-prompt state :runner "Trash Desperado")
      (is (= 2 (count (:discard (get-runner)))))))
  (testing "Should fire after Stargate. Issue #4188"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Wake Up Call"]
                        :credit 10}
                 :runner {:hand ["Stargate" "Acacia"]
                          :credit 20}})
      (take-credits state :corp)
      (play-from-hand state :runner "Stargate")
      (play-from-hand state :runner "Acacia")
      (card-ability state :runner (get-program state 0) 0)
      (run-successful state)
      (click-prompt state :runner "Hedge Fund")
      (take-credits state :runner)
      (play-from-hand state :corp "Wake Up Call")
      (click-card state :corp "Acacia")
      (click-prompt state :runner "Trash Acacia")
      (is (= 1 (count (:discard (get-runner))))))))

(deftest wetwork-refit
  ;; Wetwork Refit - Only works on Bioroid ICE and adds a subroutine
  (do-game
    (new-game {:corp {:deck ["Eli 1.0"
                             "Vanilla"
                             (qty "Wetwork Refit" 3)]}})
    (core/gain state :corp :credit 20)
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "Eli 1.0" "R&D")
    (play-from-hand state :corp "Vanilla" "HQ")
    (let [eli (get-ice state :rd 0)
          vanilla (get-ice state :hq 0)]
      (play-from-hand state :corp "Wetwork Refit")
      (is (not-any? #{"Eli 1.0"} (get-in @state [:corp :prompt :choices]))
          "Unrezzed Eli 1.0 is not a choice to host Wetwork Refit")
      (click-prompt state :corp "Done")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh eli))
      (core/rez state :corp (refresh vanilla))
      (play-from-hand state :corp "Wetwork Refit")
      (click-card state :corp (refresh eli))
      (is (= "Wetwork Refit" (:title (first (:hosted (refresh eli)))))
          "Wetwork Refit is hosted on Eli 1.0")
      (is (= 3 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 2 different subroutines")
      (is (= "[Wetwork Refit] Do 1 brain damage" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has a brain damage subroutine as his first subroutine")
      (core/move state :corp (first (:hosted (refresh eli))) :hand)
      (is (empty? (:hosted (refresh eli))) "No cards are hosted on Eli 1.0")
      (is (= 2 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 1 different subroutine")
      (is (= "End the run" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has an end the run subroutine as his first subroutine")
      (play-from-hand state :corp "Wetwork Refit")
      (click-card state :corp (refresh vanilla))
      (is (not= "Wetwork Refit" (:title (first (:hosted (refresh vanilla)))))
          "Wetwork Refit is not hosted on Vanilla"))))
