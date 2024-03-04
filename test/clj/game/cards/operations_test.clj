(ns game.cards.operations-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.eid :refer [make-eid]]
   [game.test-framework :refer :all]
   [game.utils :as utils]))

(deftest twenty-four-seven-news-cycle-breaking-news-interaction
    ;; Breaking News interaction
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

(deftest twenty-four-seven-news-cycle-posted-bounty-interaction-issue-1043
    ;; Posted Bounty interaction -- Issue #1043
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

(deftest twenty-four-seven-news-cycle-swapped-agendas-are-able-to-be-used-1555
    ;; Swapped agendas are able to be used. #1555
    (do-game
      (new-game {:corp {:deck ["24/7 News Cycle" "Chronos Project"
                               "Philotic Entanglement" "Profiteering"]}
                 :runner {:deck [(qty "Turntable" 3)]}})
      (score-agenda state :corp (find-card "Chronos Project" (:hand (get-corp))))
      (score-agenda state :corp (find-card "Philotic Entanglement" (:hand (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (core/steal state :runner (make-eid state) (find-card "Profiteering" (:hand (get-corp))))
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
      (is (= 23 (:credit (get-corp))) "Gained 15 credits")))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
  (do-game
      (new-game {:corp {:deck ["Accelerated Diagnostics" "Cerebral Overwriter" "Shipment from SanSan"
                               "Hedge Fund" "Back Channels"]}})
      (starting-hand state :corp ["Accelerated Diagnostics" "Cerebral Overwriter"])
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Accelerated Diagnostics")
      (click-prompt state :corp "OK")
      (let [co (get-content state :remote1 0)]
        (click-prompt state :corp "Shipment from SanSan")
        (click-prompt state :corp "2")
        (click-card state :corp co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-prompt state :corp "Hedge Fund")
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund")
        (click-prompt state :corp "Back Channels")
        (click-card state :corp (refresh co))
        (is (= 15 (:credit (get-corp))) "Corp gained 6 credits for Back Channels"))))

(deftest accelerated-diagnostics-interaction-with-current
    ;; Interaction with Current
    (do-game
      (new-game {:corp {:deck ["Accelerated Diagnostics" "Cerebral Overwriter"
                               "Enhanced Login Protocol" "Shipment from SanSan"
                               "Hedge Fund"]}})
      (starting-hand state :corp ["Accelerated Diagnostics" "Cerebral Overwriter"])
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (core/gain state :corp :credit 3)
      (play-from-hand state :corp "Accelerated Diagnostics")
      (click-prompt state :corp "OK")
      (let [co (get-content state :remote1 0)]
        (click-prompt state :corp "Enhanced Login Protocol")
        (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:corp :current]))))
            "Enhanced Login Protocol active in Current area")
        (click-prompt state :corp "Shipment from SanSan")
        (click-prompt state :corp "2")
        (click-card state :corp co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-prompt state :corp "Hedge Fund")
        (is (= 9 (:credit (get-corp))) "Corp gained credits from Hedge Fund"))))

(deftest accelerated-diagnostics-no-additional-costs
    ;; No additional costs
    (do-game
      (new-game {:corp {:deck ["Accelerated Diagnostics" "Breaking News"
                               "24/7 News Cycle" "BOOM!"]}})
      (starting-hand state :corp ["Accelerated Diagnostics" "Breaking News"])
      (play-and-score state "Breaking News")
      (core/gain state :corp :credit 10)
      (core/lose state :runner :tag 2)
      (play-from-hand state :corp "Accelerated Diagnostics")
      (click-prompt state :corp "OK")
      (click-prompt state :corp "24/7 News Cycle")
      (is (= "Choose an agenda in your score area" (:msg (prompt-map :corp))))
      (click-card state :corp "Breaking News")
      (click-prompt state :corp "BOOM!")))

(deftest accelerated-diagnostics-trashes-unplayed-cards
    ;; No additional costs
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 4)]
                        :hand ["Accelerated Diagnostics"]}})
      (play-from-hand state :corp "Accelerated Diagnostics")
      (click-prompt state :corp "OK")
      (click-prompt state :corp "Cancel")
      (is (= 4 (count (:discard (get-corp)))))
      (is (= 3 (count (filter #(not (:seen %)) (:discard (get-corp))))) "3 face down cards in archives")))

(deftest active-policing
  (do-game
    (new-game {:corp {:hand [(qty "Active Policing" 2) "NGO Front"]
                      :discard ["Hostile Takeover"]}})
    (play-from-hand state :corp "Active Policing")
    (is (no-prompt? state :corp) "Active Policing cannot be played")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Active Policing")
    (click-card state :corp "NGO Front")
    (click-prompt state :corp "New remote")
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))) "Runner should have 1 fewer allotted click")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (take-credits state :runner)
    (play-from-hand state :corp "Active Policing")
    (click-prompt state :corp "Done")
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))))))

(deftest ad-blitz
  ;; Launch Campaign
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ad Blitz" "Launch Campaign"]
                      :discard ["Pop-up Window"]}})
    (play-from-hand state :corp "Ad Blitz")
    (click-prompt state :corp "2")
    (click-card state :corp "Launch Campaign")
    (click-prompt state :corp "New remote")
    (click-card state :corp "Pop-up Window")
    (click-prompt state :corp "Server 1")
    (is (zero? (count (:hand (get-corp)))) "Corp should have no cards in HQ")
    (is (= ["Ad Blitz"] (->> (get-corp) :discard (map :title))) "Corp should have only Ad Blitz in Archives")))

(deftest aggressive-negotiation
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Aggressive Negotiation" "Hostile Takeover"]}})
    (play-from-hand state :corp "Aggressive Negotiation")
    (is (no-prompt? state :corp) "Corp should have no prompt")
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Aggressive Negotiation")
    (click-prompt state :corp "Hedge Fund")
    (is (= ["Hedge Fund"] (->> (get-corp) :hand (map :title))) "Hedge Fund is now in HQ")))

(deftest an-offer-you-can-t-refuse-add-card-to-score-area
    ;; add card to score area
    (do-game
      (new-game {:corp {:hand ["An Offer You Can't Refuse"]
                        :discard ["Celebrity Gift"]}})
      (play-from-hand state :corp "An Offer You Can't Refuse")
      (click-prompt state :corp "R&D")
      (is (= 1 (count (:discard (get-corp)))))
      (click-prompt state :runner "No")
      (is (= 1 (:agenda-point (get-corp))) "An Offer the Runner refused")
      (is (= 1 (count (:scored (get-corp)))))
      (is (find-card "An Offer You Can't Refuse" (:scored (get-corp))))
      (is (= 1 (count (:discard (get-corp)))))
      (is (find-card "Celebrity Gift" (:discard (get-corp))))))

(deftest an-offer-you-can-t-refuse-prevent-jack-out-during-run
    ;; prevent jack out during run
    (do-game
      (new-game {:corp {:hand ["An Offer You Can't Refuse" "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "An Offer You Can't Refuse")
      (click-prompt state :corp "R&D")
      (click-prompt state :runner "Yes")
      (is (:run @state) "Run started")
      (is (get-in @state [:run :cannot-jack-out]) "Runner cannot jack out")
      (is (not (find-card "An Offer You Can't Refuse" (:scored (get-corp)))) "Offer isn't in score area")))

(deftest anonymous-tip
  ;; Anonymous Tip
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Anonymous Tip"]}})
    (is (= 1 (count (:hand (get-corp)))) "Corp starts with 1 card in HQ")
    (is (zero? (count (:discard (get-corp)))) "Corp starts with 0 cards in Archives")
    (play-from-hand state :corp "Anonymous Tip")
    (is (= 3 (count (:hand (get-corp)))) "Corp should draw 3 cards")
    (is (= 1 (count (:discard (get-corp)))) "Corp has 1 card in Archives")))

(deftest archived-memories
  ;; Archived Memories
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Archived Memories"]
                      :discard ["Hostile Takeover"]}})
    (play-from-hand state :corp "Archived Memories")
    (click-card state :corp "Hostile Takeover")
    (is (= ["Hostile Takeover"] (->> (get-corp) :hand (map :title))) "Hostile Takeover should be in HQ")))

(deftest argus-crackdown
  ;; Argus Crackdown
  (do-game
      (new-game {:corp {:hand ["Ice Wall" "Argus Crackdown" "Hedge Fund"]
                        :deck ["Hedge Fund"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Argus Crackdown")
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(count (:hand (get-runner))) -2]
            (run-continue state))
          "Runner took 2 meat damage")
      (click-prompt state :runner "No action")
      (is (changed? [(count (:hand (get-runner))) 0]
            (run-empty-server state :rd))
          "Runner took no meat damage on unprotected server")))

(deftest armed-asset-protection-empty-archives
  (do-game
    (new-game {:corp {:hand ["Armed Asset Protection"]}})
    (play-from-hand state :corp "Armed Asset Protection")
    (is (= 6 (:credit (get-corp))) "Corp gained 3 credits")))

(deftest armed-asset-protection-no-agendas-in-archives
  (do-game
    (new-game {:corp {:hand ["Armed Asset Protection" "Ice Wall"]
                      :discard ["Hedge Fund" "NGO Front" "Prisec"]}})
    (take-credits state :corp)
    (run-empty-server state :archives)
    (take-credits state :runner)
    (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :discard)
    (play-from-hand state :corp "Armed Asset Protection")
    (is (= 12 (:credit (get-corp))) "Corp gained 3 credits + 1 credit per card type (not counting facedown cards)")))

(deftest armed-asset-protection-agendas-in-archives
  (do-game
    (new-game {:corp {:hand ["Armed Asset Protection"]
                      :discard ["Hedge Fund" (qty "Bellona" 2)]}})
    (take-credits state :corp)
    (run-empty-server state :archives)
    (click-prompt state :runner "Bellona")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Bellona")
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (play-from-hand state :corp "Armed Asset Protection")
    (is (= 13 (:credit (get-corp))) "Corp gained 3 credits + 1 credit per card type + 2 credits for the agendas")))

(deftest ark-lockdown-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Ark Lockdown"]}
                 :runner {:hand    ["Sure Gamble"]
                          :discard [(qty "Sure Gamble" 2) "Corroder"]}})
      (play-from-hand state :corp "Ark Lockdown")
      (click-prompt state :corp "Sure Gamble")
      (is (= ["Corroder"] (->> (get-runner) :discard (map :title))) "Both copies of Sure Gamble should be rfg")
      (is (= ["Sure Gamble"] (->> (get-runner) :hand (map :title))) "Sure Gambles in hand should be around")
      (is (= ["Sure Gamble" "Sure Gamble"] (->> (get-runner) :rfg (map :title))) "Two copies of Sure Gamble should be rfg'd")))

(deftest ark-lockdown-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Ark Lockdown" "Blacklist"]}
                 :runner {:hand    ["Sure Gamble"]
                          :discard [(qty "Sure Gamble" 2) "Corroder"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (play-from-hand state :corp "Ark Lockdown")
      (is (no-prompt? state :corp) "RFG prompt did not come up")
      (is (empty? (->> (get-runner) :rfg (map :title))) "No cards should be rfg'd")))

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
        (is (= (+ -1 1 discard) (-> (get-corp) :discard count)) "One card from Archives should be shuffled into R&D, AA enters")
        (is (= (+ 2 deck) (-> (get-corp) :deck count)) "Corp should draw two cards and shuffle two cards into R&D"))))

(deftest attitude-adjustment-attitude-adjustment-doesn-t-trigger-hyoubu-s-id-issue-5290
    ;; Attitude Adjustment doesn't trigger Hyoubu's ID. Issue #5290
    (do-game
      (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                        :deck ["Attitude Adjustment"
                              (qty "Hostile Takeover" 2)
                              (qty "Ice Wall" 10)]}})
      (starting-hand state :corp ["Attitude Adjustment" "Hostile Takeover" "Hostile Takeover"])
      (trash-from-hand state :corp "Hostile Takeover")
      (play-from-hand state :corp "Attitude Adjustment")
      (let [credits (:credit (get-corp))]
        (click-card state :corp (find-card "Hostile Takeover" (:hand (get-corp))))
        (click-card state :corp (find-card "Hostile Takeover" (:discard (get-corp))))
        (is (= (+ 5 credits) (:credit (get-corp))) "Corp should gain 4 [Credits] for two revealed agendas and 1 [Credits] from Hyoubu ID"))))

(deftest audacity-requires-3-cards-in-hand-to-play
    ;; requires 3 cards in hand to play
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Audacity" 2) "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Audacity")
      (is (no-prompt? state :corp) "Can't play Audacity with too few cards in HQ")))

(deftest audacity-when-placing-counters-on-1-card
    ;; when placing counters on 1 card
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Audacity" 3) "Ice Wall" "Hostile Takeover"]}})
      (play-from-hand state :corp "Audacity")
      (is (no-prompt? state :corp) "Can't play Audacity without an advanceable card")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Audacity")
      (click-card state :corp "Ice Wall")
      (click-card state :corp "Ice Wall")
      (is (= 2 (get-counters (get-ice state :hq 0) :advancement)) "Ice Wall should have 2 counters")))

(deftest audacity-when-placing-counters-on-two-cards
    ;; when placing counters on two cards
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Audacity" 3) "Ice Wall" "Hostile Takeover"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Audacity")
      (click-card state :corp "Ice Wall")
      (click-card state :corp "Hostile Takeover")
      (is (= 1 (get-counters (get-ice state :hq 0) :advancement)) "Ice Wall should have 1 counter")
      (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "Hostile Takeover should have 1 counter")))

(deftest back-channels-trashing-a-card-with-no-advancements
    ;; trashing a card with no advancements
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Back Channels" "NGO Front"]}})
      (play-from-hand state :corp "NGO Front" "New remote")
      (let [credits (:credit (get-corp))]
        (play-from-hand state :corp "Back Channels")
        (click-card state :corp "NGO Front")
        (is (= credits (:credit (get-corp))) "Corp should gain 0 credits"))))

(deftest back-channels-trashing-a-card-with-some-advancements
    ;; trashing a card with some advancements
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Back Channels" "NGO Front"]}})
      (play-from-hand state :corp "NGO Front" "New remote")
      (core/set-prop state :corp (get-content state :remote1 0) :advance-counter 3)
      (let [credits (:credit (get-corp))]
        (play-from-hand state :corp "Back Channels")
        (click-card state :corp "NGO Front")
        (is (= (+ credits 9) (:credit (get-corp))) "Corp should gain 3 * 3 credits"))))

(deftest backroom-machinations
  (do-game
    (new-game {:corp {:hand ["Backroom Machinations"]}})
    (play-from-hand state :corp "Backroom Machinations")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Backroom Machinations")
    (is (zero? (count-tags state)) "Runner should lose 1 tag")
    (is (= 1 (:agenda-point (get-corp))) "Corp gained 1 points")
    (is (= 1 (count (get-scored state :corp))) "Corp has backroom in score area")))

(deftest bad-times
  ;; Bad Times
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Bad Times"]}})
      (is (= 4 (core/available-mu state)) "Runner should start with 4 MU")
      (play-from-hand state :corp "Bad Times")
      (is (= 4 (core/available-mu state)) "Corp can't play without a tag")
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Bad Times")
      (is (= 2 (core/available-mu state)) "Runner should lose 2 available MU")
      (take-credits state :corp)
      (is (= 4 (core/available-mu state)) "Runner should regain 2 available MU")))

(deftest beanstalk-royalties
  ;; Beanstalk Royalties
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Beanstalk Royalties"]}})
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= (+ credits 3) (:credit (get-corp))) "Corp should gain 3"))))

(deftest best-defense
  ;; Best Defense
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Best Defense" 2)]}
               :runner {:hand ["Dorm Computer" "Mass-Driver"]
                        :credits 10}})
    (play-from-hand state :corp "Best Defense")
    (is (no-prompt? state :corp) "Corp can't play Best Defense without installed runner cards")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (play-from-hand state :runner "Mass-Driver")
    (take-credits state :runner)
    (play-from-hand state :corp "Best Defense")
    (is (= "Choose a Runner card with an install cost of 0 or less to trash" (:msg (prompt-map :corp))))
    (click-card state :corp "Mass-Driver")
    (is (get-program state 0) "Mass-Driver should still be installed")
    (click-card state :corp "Dorm Computer")
    (is (not (get-hardware state 0)) "Dorm Computer should be trashed")
    (gain-tags state :runner 8)
    (play-from-hand state :corp "Best Defense")
    (click-card state :corp "Mass-Driver")
    (is (not (get-program state 0)) "Mass-Driver should still be installed")
    (is (= 2 (count (:discard (get-runner)))) "2 cards should be in heap")))

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
      (is (last-n-log-contains? state 2 "Resource") "Chosen card type should be logged")
      (is (= (inc rc) (:credit (get-runner))) "Runner should gain 1 credit for trashing a Fan Site")
      (is (= (+ (* 4 2) cc) (:credit (get-corp))) "Corp should gain 8 credits for remaining 4 Fan Sites"))))

(deftest big-brother
  ;; Big Brother - Give the Runner 2 tags if already tagged
  (do-game
    (new-game {:corp {:deck ["Big Brother"]}})
    (play-from-hand state :corp "Big Brother")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Big Brother")
    (is (= 3 (count-tags state)) "Runner gained 2 tags")))

(deftest big-deal-happy-path
  ;; Big Deal - terminal, place 4 advancement tokens, may score if able
  (do-game
    (new-game {:corp {:hand ["SDS Drone Deployment" "Big Deal"] :credits 20}})
    (play-from-hand state :corp "SDS Drone Deployment" "New remote")
    (click-advance state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Big Deal")
    (click-card state :corp "SDS Drone Deployment")
    (is (= 5 (get-counters (get-content state :remote1 0) :advancement)))
    (click-prompt state :corp "Yes")
    (is (= 3 (:agenda-point (get-corp))) "Corp scored 3 points")
    (is (zero? (count (:discard (get-corp)))) "Big Deal removed from the game")
    (is (no-prompt? state :corp))))

(deftest big-deal-non-agenda
  ;; Big Deal - can't score non-agendas
  (do-game
    (new-game {:corp {:hand ["NGO Front" "Big Deal"] :credits 20}})
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "Big Deal")
    (click-card state :corp "NGO Front")
    (is (zero? (:click (get-corp))))
    (is (= 4 (get-counters (get-content state :remote1 0) :advancement)))
    (is (no-prompt? state :corp))))

(deftest big-deal-not-enough-advancements
  ;; Big Deal - no prompt if requirements not met
  (do-game
    (new-game {:corp {:hand ["SDS Drone Deployment" "Big Deal"] :credits 20}})
    (play-from-hand state :corp "SDS Drone Deployment" "New remote")
    (play-from-hand state :corp "Big Deal")
    (click-card state :corp "SDS Drone Deployment")
    (is (zero? (:click (get-corp))))
    (is (= 4 (get-counters (get-content state :remote1 0) :advancement)))
    (is (no-prompt? state :corp))))

(deftest bioroid-efficiency-research
  ;; Eli 1.0
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Bioroid Efficiency Research" "Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (play-from-hand state :corp "Bioroid Efficiency Research")
    (let [credits (:credit (get-corp))]
      (click-card state :corp "Eli 1.0")
      (is (rezzed? (get-ice state :hq 0)) "Eli 1.0 should be rezzed")
      (is (= credits (:credit (get-corp))) "Corp should spend no money to rez"))))

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

(deftest boom
  ;; BOOM!
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["BOOM!"]}
               :runner {:hand [(qty "Sure Gamble" 10)]}})
    (play-from-hand state :corp "BOOM!")
    (is (= 1 (count (:hand (get-corp)))) "BOOM! should not be played as runner has no tags")
    (gain-tags state :runner 2)
    (is (zero? (count (:discard (get-runner)))) "Runner should have 0 cards in discard")
    (play-from-hand state :corp "BOOM!")
    (is (= 7 (count (:discard (get-runner)))) "Runner should take 7 damage")))

(deftest bring-them-home-trash-card
  (do-game
    (new-game {:corp {:hand ["Bring Them Home" "Rashida Jaheem"]}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (is (changed? [(count (:hand (get-corp))) 0]
                  (play-from-hand state :corp "Bring Them Home"))
        "Bring Them Home requirements not met")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (take-credits state :runner)
    (is (changed? [(count (:hand (get-runner))) -2]
                  (play-from-hand state :corp "Bring Them Home"))
        "2 Runner cards moved off the grip")
    (is (= "Sure Gamble" (:title (nth (:deck (get-runner)) 0))) "Sure Gamble on top of the deck")
    (is (= "Sure Gamble" (:title (nth (:deck (get-runner)) 1))) "Another Sure Gamble on top of the deck")
    (is (last-log-contains? state "place Sure Gamble and Sure Gamble from the grip to the top of the stack"))
    (is (no-prompt? state :corp) "No additional prompt because threat level is not met")
    (is (zero? (:click (get-corp))) "Terminal ends turns")))

(deftest bring-them-home-threat-steal-card
  (do-game
    (new-game {:corp {:hand ["Bring Them Home" "Reeducation"]}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Reeducation" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Bring Them Home")
    (is (changed? [(count (:hand (get-runner))) -1
                   (:credit (get-corp)) -2]
                  (click-prompt state :corp "Yes"))
        "1 additional Runner cards moved off the grip")
    (is (= 1 (count (core/turn-events state :runner :runner-shuffle-deck))))
    (is (= "Sure Gamble" (:title (nth (:deck (get-runner)) 2))) "Yet another Sure Gamble on top of the deck")))

(deftest building-blocks-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:corp {:deck ["Building Blocks" "Ice Wall"]}})
      (core/gain state :corp :credit 1)
      (is (= 6 (:credit (get-corp))) "Corp starts with 6 credits")
      (play-from-hand state :corp "Building Blocks")
      (is (= 1 (:credit (get-corp))) "Spent 5 credits on Building Blocks")
      (click-card state :corp "Ice Wall")
      (click-prompt state :corp "New remote")
      (let [iw (get-ice state :remote1 0)]
        (is (= 1 (:credit (get-corp))) "Corp spent no credits installing ice")
        (is (rezzed? (refresh iw)) "Ice Wall is installed and rezzed"))))

(deftest building-blocks-choose-invalid-card
    ;; Choose invalid card
    (do-game
      (new-game {:corp {:deck ["Building Blocks" "Hedge Fund" "Cortex Lock"]}})
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Building Blocks")
      (is (no-prompt? state :corp) "Can't play Building Blocks without a Barrier in hand")))

(deftest business-as-usual
  (do-game
    (new-game {:corp {:hand [(qty "Business As Usual" 2) "NGO Front" "Project Atlas"]}
               :runner {:hand ["Cache"]}})
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "Project Atlas" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (take-credits state :runner)
    (is (changed? [(get-counters (get-program state 0) :virus) -3]
                  (play-from-hand state :corp "Business As Usual")
                  (click-prompt state :corp "Remove all virus counters from a card")
                  (click-card state :corp "Cache"))
        "Cache was purged")
    (is (changed? [(get-counters (get-content state :remote1 0) :advancement) 1
                   (get-counters (get-content state :remote2 0) :advancement) 1]
                  (play-from-hand state :corp "Business As Usual")
                  (click-prompt state :corp "Place 1 advancement counter on each of up to 2 cards you can advance")
                  (click-card state :corp "NGO Front")
                  (click-card state :corp "Project Atlas"))
        "Placed 2 advancement counters on advanceable cards")))

(deftest business-as-usual-threat
  (do-game
    (new-game {:corp {:hand ["Business As Usual" "Project Atlas" "Bellona"]}
               :runner {:hand ["Cache"]}})
    (play-from-hand state :corp "Project Atlas" "New remote")
    (play-and-score state "Bellona")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (take-credits state :runner)
    (play-from-hand state :corp "Business As Usual")
    (is (changed? [(get-counters (get-program state 0) :virus) -3
                   (get-counters (get-content state :remote1 0) :advancement) 1]
                  (click-prompt state :corp "Do both")
                  (click-card state :corp "Cache")
                  (click-card state :corp "Project Atlas")
                  (click-prompt state :corp "Done"))
        "Cache was purged and 1 advancement counters was placed")))

(deftest casting-call
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
  (do-game
    (new-game {:corp {:deck [(qty "Casting Call" 2) "Oaktown Renovation"
                             "Improved Tracers" "Hunter"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Hunter" "HQ")
    (let [hunter (get-ice state :hq 0)]
      (rez state :corp hunter)
      (is (= 4 (get-strength (refresh hunter))))
      (play-from-hand state :corp "Casting Call")
      (click-card state :corp (find-card "Improved Tracers" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [imptrac (get-content state :remote1 0)]
        (is (faceup? (refresh imptrac)) "Improved Tracers is faceup")
        (is (= 4 (get-strength (refresh hunter))) "Hunter hasn't gained strength")
        (play-from-hand state :corp "Casting Call")
        (click-card state :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (let [oak (get-content state :remote2 0)]
          (click-advance state :corp (refresh oak))
          (is (= 5 (:credit (get-corp))) "Events on Public agenda work; gained 2 credits from advancing")
          (take-credits state :corp)
          (run-empty-server state "Server 2")
          (click-prompt state :runner "Steal")
          (is (= 2 (count-tags state)) "Runner took 2 tags from accessing agenda with Casting Call hosted on it"))))))

(deftest celebrity-gift
  ;; Ice Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Celebrity Gift" "NGO Front" "IPO" "PAD Campaign" "Hostile Takeover" "Ice Wall"]}})
    (play-from-hand state :corp "Celebrity Gift")
    (let [credits (:credit (get-corp))]
      (click-card state :corp "NGO Front")
      (click-card state :corp "IPO")
      (click-card state :corp "PAD Campaign")
      (click-card state :corp "Hostile Takeover")
      (click-card state :corp "Ice Wall")
      (is (= (+ credits 10) (:credit (get-corp))) "Corp should gain 10 credits from 5 cards"))))

(deftest cerebral-cast-runner-wins
    ;; Runner wins
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

(deftest cerebral-cast-corp-wins
    ;; Corp wins
    (do-game
      (new-game {:corp {:deck [(qty "Cerebral Cast" 2)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "Suffer 1 core damage")
      (is (= 1 (count (:discard (get-runner)))) "Runner took a core damage")
      (is (zero? (count-tags state)) "Runner took no tags from core damage choice")
      (play-from-hand state :corp "Cerebral Cast")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "Take 1 tag")
      (is (= 1 (count (:discard (get-runner)))) "Runner took no additional damage")
      (is (= 1 (count-tags state)) "Runner took a tag from Cerebral Cast choice")))

(deftest cerebral-static-vs-chaos-theory
    ;; vs Chaos Theory
    (do-game
      (new-game {:corp {:deck ["Cerebral Static" "Lag Time"]}
                 :runner {:id "Chaos Theory: Wünderkind"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 5 (core/available-mu state)) "CT starts with 5 memory")
      (play-from-hand state :corp "Cerebral Static")
      (is (= 4 (core/available-mu state)) "Cerebral Static causes CT to have 4 memory")
      (play-from-hand state :corp "Lag Time")
      (is (= 5 (core/available-mu state)) "CT 5 memory restored")))

(deftest clones-are-not-people
  ;; Merger
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["\"Clones are not People\"" "Merger"]}})
    (play-from-hand state :corp "\"Clones are not People\"")
    (play-and-score state "Merger")
    (is (= 2 (count (get-scored state :corp))) "Corp should have 2 cards in score area")))

(deftest closed-accounts
  ;; Closed Accounts - Play if Runner is tagged to make Runner lose all credits
  (do-game
    (new-game {:corp {:deck ["Closed Accounts"]}})
    (play-from-hand state :corp "Closed Accounts")
    (is (and (= 3 (:click (get-corp)))
             (= 5 (:credit (get-runner))))
        "Closed Accounts precondition not met; card not played")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Closed Accounts")
    (is (zero? (:credit (get-runner))) "Runner lost all credits")))

(deftest commercialization-single-advancement-token
    ;; Single advancement token
    (do-game
      (new-game {:corp {:deck ["Commercialization"
                               "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 1)
      (play-from-hand state :corp "Commercialization")
      (click-card state :corp (refresh (get-ice state :hq 0)))
      (is (= 6 (:credit (get-corp))) "Gained 1 for single advanced ice from Commercialization")))

(deftest commercialization-two-advancement-tokens
    ;; Two advancement tokens
    (do-game
      (new-game {:corp {:deck ["Commercialization"
                               "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 2)
      (play-from-hand state :corp "Commercialization")
      (click-card state :corp (refresh (get-ice state :hq 0)))
      (is (= 7 (:credit (get-corp))) "Gained 2 for double advanced ice from Commercialization")))

(deftest complete-image-correctly-guessing
    ;; Correctly guessing
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Priority Requisition"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (-> (get-runner) :discard count zero?) "heap should be empty")
      (click-prompt state :corp "Sure Gamble")
      (is (not (no-prompt? state :corp)) "Corp guessed right so should have another choice")
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :corp "Sure Gamble")
      (is (not (no-prompt? state :corp)) "Even when the runner has no cards in hand, Corp must choose again")
      (click-prompt state :corp "Sure Gamble")
      (is (no-prompt? state :corp) "Runner is flatlined so no more choices")
      (is (= 5 (-> (get-runner) :discard count)) "heap should have 5 cards")))

(deftest complete-image-incorrectly-guessing
    ;; Incorrectly guessing
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Priority Requisition"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (-> (get-runner) :discard count zero?) "heap should be empty")
      (click-prompt state :corp "Easy Mark")
      (is (no-prompt? state :corp) "Corp guessed incorrectly so shouldn't have another choice")
      (is (= 1 (-> (get-runner) :discard count)) "heap should have 1 card")))

(deftest complete-image-not-enough-agenda-points
    ;; Not enough agenda points
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Hostile Takeover"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (no-prompt? state :corp) "Corp shouldn't be able to play Complete Image")))

(deftest complete-image-didn-t-run-last-turn
    ;; Didn't run last turn
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Priority Requisition"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (no-prompt? state :corp) "Corp shouldn't be able to play Complete Image")))

(deftest complete-image-interaction-with-chronos-protocol
    ;; Interaction with Chronos Protocol
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Complete Image" "Priority Requisition"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Complete Image")
      (is (-> (get-runner) :discard count zero?) "heap should be empty")
      (click-prompt state :corp "Sure Gamble") ;; Complete Image
      (is (not (no-prompt? state :corp)) "Corp guessed right so should have another choice")
      (click-prompt state :corp "Yes") ;; Chronos Protocol
      (click-prompt state :corp "Sure Gamble") ;; Chronos Protocol
      (click-prompt state :corp "Sure Gamble") ;; Complete Image
      (click-prompt state :corp "Sure Gamble") ;; Complete Image
      (click-prompt state :corp "Sure Gamble") ;; Complete Image
      (is (not (no-prompt? state :corp)) "Even when the runner has no cards in hand, Corp must choose again")
      (click-prompt state :corp "Sure Gamble") ;; Complete Image
      (click-prompt state :corp "Sure Gamble") ;; Complete Image
      (is (no-prompt? state :corp) "Runner is flatlined so no more choices")
      (is (= 5 (-> (get-runner) :discard count)) "heap should have 5 cards")))

(deftest consulting-visit
  ;; Consulting Visit - Only show single copies of operations corp can afford as choices. Play chosen operation
  (do-game
      (new-game {:corp {:deck ["Consulting Visit"
                               (qty "Beanstalk Royalties" 2)
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"]}})
      (is (= 5 (:credit (get-corp))))
      (starting-hand state :corp ["Consulting Visit"])
      (play-from-hand state :corp "Consulting Visit")
      (is (= ["Beanstalk Royalties" "Green Level Clearance" nil] (prompt-titles :corp)))
      (click-prompt state :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
      (is (= 6 (:credit (get-corp))))))

(deftest consulting-visit-works-properly-when-played-with-mumbad-city-hall
    ;; Works properly when played with Mumbad City Hall
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
      (let [hall (get-content state :remote1 0)]
        (rez state :corp hall)
        (card-ability state :corp (refresh hall) 0)
        (is (= ["Consulting Visit" "Mumba Temple" nil] (prompt-titles :corp)))
        (click-prompt state :corp (find-card "Consulting Visit" (:deck (get-corp))))
        (is (= 2 (:credit (get-corp))))
        (is (= ["Beanstalk Royalties" "Green Level Clearance" nil] (prompt-titles :corp)))
        (click-prompt state :corp (find-card "Green Level Clearance" (:deck (get-corp))))
        (is (= 4 (:credit (get-corp)))))))

(deftest corporate-hospitality
  (do-game
    (new-game {:corp {:hand ["Corporate Hospitality"]
                      :deck [(qty "Hedge Fund" 3)]
                      :discard ["PAD Campaign"]}})
    (is (changed? [(:credit (get-corp)) 2
                   (count (:hand (get-corp))) 2]
                  (play-from-hand state :corp "Corporate Hospitality")
                  (click-card state :corp (find-card "PAD Campaign" (:discard (get-corp)))))
        "Corp gained 2 credits net and drew 2 cards")
    (is (find-card "PAD Campaign" (:hand (get-corp))) "PAD Campaign is now in HQ")))

(deftest corporate-shuffle
  ;; Ice Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Corporate Shuffle" (qty "Ice Wall" 2)]}})
    (is (= 5 (count (:deck (get-corp)))) "Corp should start with 5 cards in deck")
    (is (= 3 (count (:hand (get-corp)))) "Corp should have 3 cards in hand")
    (play-from-hand state :corp "Corporate Shuffle")
    (is (= 2 (count (:deck (get-corp)))) "Corp should have 3 cards in deck after draw")
    (is (= 5 (count (:hand (get-corp)))) "Corp should draw up to 5 cards")
    (is (= 1 (count (:discard (get-corp)))) "Corp should have 1 card in discard from playing")))

(deftest cyberdex-trial
  ;; Cyberdex Trial
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Cyberdex Trial"]}
               :runner {:hand ["Datasucker"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Datasucker")
    (core/add-counter state :runner (get-program state 0) :virus 2)
    (take-credits state :runner)
    (play-from-hand state :corp "Cyberdex Trial")
    (is (zero? (get-counters (get-program state 0) :virus)) "Datasucker should have no virus countes left")))

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

(deftest dedication-ceremony
  ;; Dedication Ceremony
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover" "Underway Renovation" "Dedication Ceremony"]}})
    (play-from-hand state :corp "Underway Renovation" "New remote")
    (play-from-hand state :corp "Dedication Ceremony")
    (click-card state :corp "Underway Renovation")
    (let [under (get-content state :remote1 0)]
      (is (= 3 (get-counters under :advancement)) "Underway Renovation has 3 advancement counters on itself")
      (score state :corp (refresh under))
      (is (refresh under) "Underway Renovation isn't scored because of Dedication Ceremony")
      (play-and-score state "Hostile Takeover")
      (is (= "Hostile Takeover" (:title (get-scored state :corp 0))) "Other agendas can be scored")
      (take-credits state :corp)
      (take-credits state :runner)
      (score state :corp (refresh under))
      (is (nil? (refresh under)) "Underway Renovation is scored")
      (is (= "Underway Renovation" (:title (get-scored state :corp 1)))))))

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
      (rez state :corp vik)
      (run-continue state)
      (card-subroutine state :corp vik 0)
      (is (= 2 (count (:discard (get-runner)))) "2 cards lost to core damage")
      (is (= 2 (:brain-damage (get-runner))) "Brainchips dealt 1 additional brain dmg")
      (card-subroutine state :corp vik 0)
      (is (= 3 (count (:discard (get-runner)))) "2 cards lost to core damage")
      (is (= 3 (:brain-damage (get-runner))) "Brainchips didn't do additional brain dmg"))))

(deftest digital-rights-management-cannot-score-agenda-installed-after-playing
  ;; Cannot score Agenda installed after playing DRM
  (do-game
    (new-game {:corp {:hand [(qty "Digital Rights Management" 2) "Project Vitruvius" (qty "Hedge Fund" 2)]
                      :deck [(qty "Project Beale" 2) (qty "Hedge Fund" 3)]}})
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Digital Rights Management")
    (click-prompt state :corp "Project Beale")
    (click-prompt state :corp "Done")
    (play-from-hand state :corp "Project Vitruvius")
    (click-prompt state :corp "New remote")
    (core/gain state :corp :click 2)
    (let [vit (get-content state :remote1 0)]
      (dotimes [_ 3] (click-advance state :corp (refresh vit)))
      (score state :corp (refresh vit))
      (is (= 0 (count (get-scored state :corp))) "Project Vitruvius was not scored")
      (take-credits state :corp)
      (take-credits state :runner)
      (score state :corp (refresh vit)))))

(deftest digital-rights-management-drm-only-searches-for-agendas-in-r-d
    ;; DRM only searches for Agendas in R&D
    (do-game
      (new-game {:corp {:hand [(qty "Digital Rights Management" 2) (qty "Hedge Fund" 3)]
                        :deck [(qty "Project Beale" 2)
                               (qty "PAD Campaign" 2)
                               (qty "Hedge Fund" 2)
                               (qty "Crisium Grid" 2)
                               (qty "Ice Wall" 2)]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Digital Rights Management")
      (is (= 2 (count (prompt-buttons :corp))) "Only Beale and 'None' option displayed")
      (let [cards-in-hand (count (:hand (get-corp)))]
        (click-prompt state :corp "Project Beale")
        (is (= (inc cards-in-hand) (count (:hand (get-corp)))) "Beale added to hand"))
      (click-card state :corp (find-card "Project Beale" (:hand (get-corp))))
      (is (= 1 (count (prompt-buttons :corp))) "No option to install on centrals")
      (click-prompt state :corp "New remote")
      (core/gain state :corp :click 1)
      (let [beale (get-content state :remote1 0)]
        (dotimes [_ 3] (click-advance state :corp (refresh beale)))
        (score state :corp (refresh beale))
        (is (= 0 (count (get-scored state :corp))) "Beale was not scored")
        (take-credits state :corp)
        (take-credits state :runner)
        (score state :corp (refresh beale))
        (is (= 1 (count (get-scored state :corp))) "Beale was scored"))))

(deftest digital-rights-management-drm-only-installs-on-remotes
    ;; DRM only installs on remotes
    (do-game
      (new-game {:corp {:hand [(qty "Digital Rights Management" 2) (qty "Hedge Fund" 3)]
                        :deck [(qty "Project Beale" 2) (qty "Hedge Fund" 3)]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Digital Rights Management")
      (click-prompt state :corp "Project Beale")
      (click-card state :corp (find-card "Project Beale" (:hand (get-corp))))
      (is (= 1 (count (prompt-buttons :corp))) "No option to install on centrals")
      (click-prompt state :corp "New remote")))

(deftest digital-rights-management-cannot-score-agenda-installed-by-drm
    ;; Cannot score Agenda installed by DRM
    (do-game
      (new-game {:corp {:hand [(qty "Digital Rights Management" 2) (qty "Hedge Fund" 3)]
                        :deck [(qty "Project Beale" 2) (qty "Hedge Fund" 3)]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Digital Rights Management")
      (click-prompt state :corp "Project Beale")
      (click-card state :corp (find-card "Project Beale" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (core/gain state :corp :click 1)
      (let [beale (get-content state :remote1 0)]
        (dotimes [_ 3] (click-advance state :corp (refresh beale)))
        (score state :corp (refresh beale))
        (is (= 0 (count (get-scored state :corp))) "Beale was not scored")
        (take-credits state :corp)
        (take-credits state :runner)
        (score state :corp (refresh beale))
        (is (= 1 (count (get-scored state :corp))) "Beale was scored"))))

(deftest digital-rights-management-cannot-score-agenda-installed-before-playing-drm
    ;; Cannot score Agenda installed before playing DRM
    (do-game
      (new-game {:corp {:hand [(qty "Digital Rights Management" 2) "Project Beale" (qty "Hedge Fund" 2)]
                        :deck [(qty "Project Beale" 2) (qty "Hedge Fund" 3)]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (core/gain state :corp :click 3)
      (play-from-hand state :corp "Project Beale" "New remote")
      (let [beale (get-content state :remote1 0)]
        (dotimes [_ 3] (click-advance state :corp (refresh beale)))
        (play-from-hand state :corp "Digital Rights Management")
        (click-prompt state :corp "None")
        (click-prompt state :corp "Done")
        (score state :corp (refresh beale))
        (is (= 0 (count (get-scored state :corp))) "Beale was not scored"))))

(deftest digital-rights-management-cannot-play-if-runner-made-a-successful-run-on-hq-last-turn
    ;; Cannot play if runner made a successful run on HQ last turn
    (do-game
      (new-game {:corp {:hand [(qty "Digital Rights Management" 2) (qty "Hedge Fund" 2)]
                        :deck [(qty "Project Beale" 2) (qty "Hedge Fund" 3)]}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (play-from-hand state :corp "Digital Rights Management")
      (is (no-prompt? state :corp) "No prompt displayed")))

(deftest distract-the-masses
  (do-game
    (new-game {:corp {:hand ["Distract the Masses" (qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Distract the Masses")
    (click-card state :corp (first (:hand (get-corp))))
    (click-card state :corp (last (:hand (get-corp))))
    (click-card state :corp (first (:discard (get-corp))))
    (click-prompt state :corp "Done")
    (is (= 1 (count (:discard (get-corp)))) "1 card still discarded")
    (is (= 1 (count (:deck (get-corp)))) "1 card shuffled into R&D")
    (is (= 1 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-runner))) "Runner gained 2 credits"))
  (do-game
    (new-game {:corp {:hand ["Distract the Masses" "Hedge Fund"]
                      :discard ["Hedge Fund"]}})
    (play-from-hand state :corp "Distract the Masses")
    (click-card state :corp (first (:hand (get-corp))))
    (click-prompt state :corp "Done")
    (click-card state :corp (first (:discard (get-corp))))
    (click-card state :corp (last (:discard (get-corp))))
    (is (zero? (count (:discard (get-corp)))) "No cards left in archives")
    (is (= 2 (count (:deck (get-corp)))) "2 more cards shuffled into R&D")
    (is (= 1 (count (:rfg (get-corp)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-runner))) "Runner gained 2 credits")))

(deftest distributed-tracing
  ;; Distributed Tracing
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Distributed Tracing" "Hostile Takeover"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Distributed Tracing")
    (is (no-prompt? state :corp) "Corp should have no prompt without agenda stolen")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Distributed Tracing")
    (is (= 1 (count-tags state)) "Runner took 1 tag")))

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
    (is (= 7 (:credit (get-corp))) "Ignored remote with ice but no server contents")))

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
      (rez state :corp pw)
      (rez state :corp ec1)
      (rez state :corp ec2)
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

(deftest eavesdrop-basic-behavior
    ;; Basic Behavior
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Eavesdrop" "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Eavesdrop")
      (click-card state :corp "Ice Wall")
      (is (= "Eavesdrop" (get-title (first (:hosted (get-ice state :hq 0))))) "Eavesdrop is successfully hosted on Ice Wall")
      (take-credits state :corp)
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= :trace (prompt-type :corp)) "Corp should initiate a trace")
      (is (zero? (count-tags state)) "Runner should have no tags")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner should gain 1 tag from Eavesdrop ability")))

(deftest eavesdrop-fire-only-for-hosted-ice
    ;; Fire only for hosted ice
    (do-game
      (new-game {:corp {:hand ["Eavesdrop" "Ice Wall" "Wall of Static"]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Eavesdrop")
      (click-card state :corp "Ice Wall")
      (is (= "Eavesdrop" (get-title (first (:hosted (get-ice state :hq 0))))) "Eavesdrop is successfully hosted on Ice Wall")
      (play-from-hand state :corp "Wall of Static" "R&D")
      (take-credits state :corp)
      (run-on state :rd)
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (is (no-prompt? state :corp) "Corp should have no prompts")
      (is (no-prompt? state :runner) "Runner should have no prompts")
      (is (zero? (count-tags state)) "Runner should have no tags")))

(deftest economic-warfare
  ;; Economic Warfare - If successful run last turn, make the runner lose 4 credits if able
  (do-game
    (new-game {:corp {:deck [(qty "Economic Warfare" 3)]}})
    (play-from-hand state :corp "Economic Warfare")
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
    (is (= 3 (count (:hand (get-corp)))) "Corp still has 3 cards")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "Economic Warfare")
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (play-from-hand state :corp "Economic Warfare")
    (is (zero? (:credit (get-runner))) "Runner has 0 credits")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "Economic Warfare")
    (is (= 3 (:credit (get-runner))) "Runner has 3 credits")))

(deftest economic-warfare-ignoring-net-mercur-credits
  ;; Economic Warfare should ignore Net Mercur credits
  (do-game
    (new-game {:corp {:hand ["Economic Warfare"]}
               :runner {:hand ["Net Mercur" "Mantle" "Marjanah"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Marjanah")
    (play-from-hand state :runner "Mantle")
    (play-from-hand state :runner "Net Mercur")
    (run-on state :archives)
    (card-ability state :runner (get-program state 0) 1)
    (click-card state :runner (get-program state 1))
    (click-prompt state :runner "Place 1 [Credits] on Net Mercur")
    (run-continue state)
    (take-credits state :runner)
    (play-from-hand state :corp "Economic Warfare")
    (is (= 3 (:credit (get-runner))) "Runner has still 3 credits")))

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

(deftest end-of-the-line
  ;; End of the Line
  (do-game
    (new-game {:corp {:deck ["End of the Line"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
    (gain-tags state :runner 1)
    (play-from-hand state :corp "End of the Line")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (is (zero? (count-tags state)) "Runner list a tag")))

(deftest enforced-curfew
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enforced Curfew" "Hostile Takeover"]}})
    (is (= 5 (core/hand-size state :runner)) "Runner should start with 5 max hand size")
    (play-from-hand state :corp "Enforced Curfew")
    (is (= 4 (core/hand-size state :runner)) "Runner should lose 1 hand size")
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (is (= 5 (core/hand-size state :runner)) "Runner should go back to 5 hand size")))

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

(deftest enhanced-login-protocol-first-click-run-each-turn-costs-an-additional-click
    ;; First click run each turn costs an additional click
    (do-game
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Employee Strike"]}})
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (run-empty-server state :archives)
      (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make the first run")
      (run-empty-server state :archives)
      (is (= 1 (:click (get-runner))) "Runner doesn't spend 1 additional click to make the second run")
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
      (run-empty-server state :archives)
      (is (= 2 (:click (get-runner))) "Runner doesn't spend 1 additional click to make a run")))

(deftest enhanced-login-protocol-card-ability-runs-don-t-cost-additional-clicks
    ;; Card ability runs don't cost additional clicks
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
        (run-continue state)
        (run-empty-server state :archives)
        (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
        (run-empty-server state :archives)
        (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make a run"))))

(deftest enhanced-login-protocol-with-new-angeles-sol-enhanced-login-protocol-trashed-and-reinstalled-on-steal-doesn-t-double-remove-penalty
    ;; with New Angeles Sol, Enhanced Login Protocol trashed and reinstalled on steal doesn't double remove penalty
    (do-game
      (new-game {:corp {:id "New Angeles Sol: Your News"
                        :deck ["Enhanced Login Protocol" "Breaking News"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enhanced Login Protocol" (:discard (get-corp))))
      (run-empty-server state :archives)
      (is (= 1 (:click (get-runner))) "Runner has 1 click")))

(deftest enhanced-login-protocol-run-event-don-t-cost-additional-clicks
    ;; Run event don't cost additional clicks
    (do-game
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Out of the Ashes"]}})
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (play-from-hand state :runner "Out of the Ashes")
      (click-prompt state :runner "Archives")
      (is (= 3 (:click (get-runner))) "Runner doesn't spend 1 additional click to run with a run event")
      (run-continue state)
      (run-empty-server state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "No") ; Out of the Ashes prompt
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (run-empty-server state :archives)
      (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make a run")))

(deftest enhanced-login-protocol-works-when-played-on-the-runner-s-turn
    ;; Works when played on the runner's turn
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
      (run-empty-server state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")))

(deftest enhanced-login-protocol-doesn-t-fire-if-already-run-when-played-on-the-runner-s-turn
  ;; Doesn't fire if already run when played on the runner's turn
    (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck ["Enhanced Login Protocol"
                             "Breaking News"]}
               :runner {:deck ["Hades Shard"]}})
    (trash-from-hand state :corp "Breaking News")
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Hades Shard")
    (card-ability state :runner (get-resource state 0) 0)
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (click-card state :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
    (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "Enhanced Login Protocol is in play")
    (is (= 2 (:click (get-runner))) "Runner has 2 clicks")
    (run-empty-server state :archives)
    (is (= 1 (:click (get-runner))) "Runner doesn't spend 1 additional click to make a run")))

(deftest exchange-of-information
  ;; Exchange of Information
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
      (core/steal state :runner (make-eid state) (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (make-eid state) (find-card "Explode-a-palooza" (:hand (get-corp))))
      (take-credits state :runner)
      (is (= 4 (:agenda-point (get-runner))))
      (is (= 3 (:agenda-point (get-corp))))
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 3 (:agenda-point (get-runner))))
      (is (= 4 (:agenda-point (get-corp))))))

(deftest exchange-of-information-swapping-a-just-scored-breaking-news-keeps-the-tags
    ;; Swapping a just scored Breaking News keeps the tags
    (do-game
      (new-game {:corp {:deck ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"]}})
      (take-credits state :corp)
      (core/steal state :runner (make-eid state) (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (make-eid state) (find-card "Explode-a-palooza" (:hand (get-corp))))
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

(deftest exchange-of-information-swapping-a-15-minutes-still-keeps-the-ability-1783
    ;; Swapping a 15 Minutes still keeps the ability. #1783
    (do-game
      (new-game {:corp {:deck [(qty "Exchange of Information" 2) "15 Minutes"
                               "Project Beale"]}})
      (score-agenda state :corp (find-card "15 Minutes" (:hand (get-corp))))
      (take-credits state :corp)
      (gain-tags state :runner 1)
      (core/steal state :runner (make-eid state) (find-card "Project Beale" (:hand (get-corp))))
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
      (core/steal state :runner (make-eid state) (find-card "15 Minutes" (:deck (get-corp))))
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

(deftest exchange-of-information-swapping-a-mandatory-upgrades-gives-the-corp-an-additional-click-per-turn-1687
    ;; Swapping a Mandatory Upgrades gives the Corp an additional click per turn. #1687
    (do-game
      (new-game {:corp {:deck [(qty "Exchange of Information" 2) "Mandatory Upgrades"
                               "Global Food Initiative"]}})
      (score-agenda state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
      (take-credits state :corp)
      (gain-tags state :runner 1)
      (core/steal state :runner (make-eid state) (find-card "Mandatory Upgrades" (:hand (get-corp))))
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
      (is (= 3 (:click-per-turn (get-corp))))))

(deftest extract-trash-to-gain-9
  ;; trash a card to gain 9
  (do-game
   (new-game {:corp {:hand ["Extract" "PAD Campaign"]}})
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (is (changed? [(:credit (get-corp)) +6]
         (play-from-hand state :corp "Extract")
         (click-card state :corp "PAD Campaign"))
       "Gains net 6 credits from Extract")
   (is (= 2 (count (:discard (get-corp)))) "PAD and Extract trashed")))

(deftest extract-skip-trash
  ;; skip trash to gain 6
  (do-game
   (new-game {:corp {:hand ["Extract" "PAD Campaign"]}})
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (is (changed? [(:credit (get-corp)) +3]
         (play-from-hand state :corp "Extract")
         (click-prompt state :corp "Done"))
       "Gains net 3 credits from Extract")
   (is (= 1 (count (:discard (get-corp)))) "Extract trashed, but not PAD")))

(deftest extract-nothing-to-trash
  ;; nothing to trash, gain 6
  (do-game
   (new-game {:corp {:hand ["Extract"]}})
   (is (changed? [(:credit (get-corp)) +3]
         (play-from-hand state :corp "Extract"))
       "Gained net 3c from Extract")
   (is (no-prompt? state :corp) "No prompt because there are no cards to trash!")))

(deftest extract-log-card-str
  (do-game
   (new-game {:corp {:hand ["Extract" "PAD Campaign"]}})
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (play-from-hand state :corp "Extract")
   (click-card state :corp "PAD Campaign")
   (is (last-log-contains? state "trash a card in Server 1"))))

(deftest fast-break
  ;; Fast Break
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Enigma" "Rime" "Hedge Fund"]
                      :hand ["Fast Break" "Hostile Takeover" "Keegan Lane"
                             "Haas Arcology AI" "Research Station"]}
               :runner {:deck [(qty "Fan Site" 3)]}})
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
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= (dec credits) (:credit (get-corp))) "Corp should pay 1 credit to install second Ice Wall"))
    (core/move state :corp (find-card "Fast Break" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Fast Break")
    (let [hand (-> (get-corp) :hand count)
          credits (:credit (get-corp))]
      (click-prompt state :corp "0")
      (is (= hand (-> (get-corp) :hand count)) "Corp should draw no cards as they're allowed to draw no cards")
      (is (some #{"Server 2"} (prompt-buttons :corp)) "Corp should be able to choose existing remotes")
      (click-prompt state :corp "Server 2")
      (click-card state :corp (find-card "Haas Arcology AI" (:hand (get-corp))))
      (click-card state :corp (find-card "Research Station" (:hand (get-corp))))
      (is (= 2 (count (get-content state :remote2))) "Corp can't choose Research Station to install in a remote")
      (click-card state :corp (find-card "Rime" (:hand (get-corp))))
      (click-prompt state :corp "Done")
      (is (= (- credits 2) (:credit (get-corp))) "Corp should pay 2 credits to install third Ice Wall")
      (is (no-prompt? state :corp) "Corp should be able to stop installing early"))))

(deftest fast-track
  ;; Fast Track
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5) "Hostile Takeover"]
                      :hand ["Fast Track"]}})
    (play-from-hand state :corp "Fast Track")
    (click-prompt state :corp "Hostile Takeover")
    (is (= ["Hostile Takeover"] (->> (get-corp) :hand (map :title))) "Hostile Takeover should now be in hand")))

(deftest financial-collapse-runner-has-no-credits
    ;; runner has no credits
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Financial Collapse"]}
                 :runner {:hand ["Kati Jones" "Net Mercur"]}})
      (play-from-hand state :corp "Financial Collapse")
      (is (= ["Financial Collapse"] (->> (get-corp) :hand (map :title)))
          "Financial Collapse shouldn't be playable without credit req")))

(deftest financial-collapse-runner-has-no-installed-resources
    ;; Runner has no installed resources
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Financial Collapse"]}
                 :runner {:credits 6}})
      (let [credits (:credit (get-runner))]
        (play-from-hand state :corp "Financial Collapse")
        (is (= credits (:credit (get-runner))) "Runner should lose no credits from no resources in play"))))

(deftest financial-collapse-runner-has-2-installed-resources-and-doesn-t-trash-a-resource
    ;; Runner has 2 installed resources and doesn't trash a resource
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Financial Collapse"]}
                 :runner {:hand ["Kati Jones" "Net Mercur"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Kati Jones")
      (play-from-hand state :runner "Net Mercur")
      (core/gain state :runner :credit 6)
      (take-credits state :runner)
      (let [credits (:credit (get-runner))]
        (play-from-hand state :corp "Financial Collapse")
        (click-prompt state :runner "No")
        (is (= (+ credits -4) (:credit (get-runner))) "Runner should lose 4 credits from 2 resources in play"))))

(deftest financial-collapse-runner-has-2-installed-resources-and-does-trash-a-resource
    ;; Runner has 2 installed resources and does trash a resource
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Financial Collapse"]}
                 :runner {:hand ["Kati Jones" "Net Mercur"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Kati Jones")
      (play-from-hand state :runner "Net Mercur")
      (core/gain state :runner :credit 6)
      (take-credits state :runner)
      (let [credits (:credit (get-runner))
            kati (get-resource state 0)]
        (play-from-hand state :corp "Financial Collapse")
        (click-prompt state :runner "Yes")
        (click-card state :runner "Kati Jones")
        (is (not (refresh kati)) "Kati Jones should be trashed")
        (is (= credits (:credit (get-runner))) "Runner should lose no credits"))))

(deftest focus-group-regular-scenario-can-afford
    ;; Regular scenario - can afford
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

(deftest focus-group-0-valid-targets-gracefully-end
    ;; 0 valid targets, gracefully end
    (do-game
      (new-game {:corp {:deck ["Focus Group"]}
                 :runner {:deck ["Sure Gamble" "Dirty Laundry" "Corroder" "Datasucker" "Black Orchestra"]}
                 :options {:start-as :runner}})
      (run-empty-server state :rd)
      (take-credits state :runner)
      (play-from-hand state :corp "Focus Group")
      (is (= 5 (:credit (get-corp))))
      (click-prompt state :corp "Hardware")
      (is (no-prompt? state :corp) "No hardware in the grip so just end the interaction")
      (is (= 5 (:credit (get-corp))))))

(deftest focus-group-can-t-afford-to-pay-to-place-advancement-tokens-gracefully-end
    ;; Can't afford to pay to place advancement tokens, gracefully end
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
      (is (no-prompt? state :corp) "Corp can't afford to pay so just end the interaction")
      (is (= 2 (:credit (get-corp))) "Didn't pay to place advancement tokens")))

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

(deftest freelancer
  ;; Freelancer
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Freelancer"]}
               :runner {:hand ["Kati Jones" "Net Mercur"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (play-from-hand state :runner "Net Mercur")
    (take-credits state :runner)
    (play-from-hand state :corp "Freelancer")
    (is (= ["Freelancer" "Hedge Fund"] (->> (get-corp) :hand (map :title))) "Freelancer shouldn't be playable without a tag")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Freelancer")
    (click-card state :corp "Kati Jones")
    (click-card state :corp "Net Mercur")
    (is (zero? (count (get-resource state))) "Runner should have no resources left in play")))

(deftest friends-in-high-places
  ;; Friends in High Places
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Friends in High Places"]
                      :discard ["Ice Wall" "Hostile Takeover"]}})
    (play-from-hand state :corp "Friends in High Places")
    (click-card state :corp "Ice Wall")
    (click-prompt state :corp "New remote")
    (click-card state :corp "Hostile Takeover")
    (click-prompt state :corp "Server 1")
    (is (= "Ice Wall" (:title (get-ice state :remote1 0))) "Ice Wall should be installed")
    (is (= "Hostile Takeover" (:title (get-content state :remote1 0))) "Hostile Takeover should be installed")))

(deftest fully-operational-gain-2-credits
    ;; Gain 2 credits
    (do-game
      (new-game {:corp {:deck ["Fully Operational"]}})
      (play-from-hand state :corp "Fully Operational")
      (is (= 4 (:credit (get-corp))) "Cost 1 credit to play")
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 6 (:credit (get-corp))) "Corp gained 2 credits")
      (is (no-prompt? state :corp) "No lingering prompt after making single choice")))

(deftest fully-operational-draw-2-cards
    ;; Draw 2 cards
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) "Fully Operational"]}})
      (starting-hand state :corp ["Fully Operational"])
      (play-from-hand state :corp "Fully Operational")
      (click-prompt state :corp "Draw 2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp drew 2 cards")
      (is (no-prompt? state :corp) "No lingering prompt after making single choice")))

(deftest fully-operational-extra-choices-from-remote-servers
    ;; Extra choices from remote servers
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
      (is (no-prompt? state :corp) "No lingering prompt after making repeated choices")))

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

(deftest game-over-can-t-play-unless-runner-stole-an-agenda-last-turn
    ;; Can't play unless Runner stole an agenda last turn
    (do-game
      (new-game {:corp {:deck ["Project Beale" "Game Over"]}})
      (play-from-hand state :corp "Game Over")
      (is (not (= "Loot Box" (-> (get-corp) :discard first :title))) "Runner didn't steal an agenda last turn")))

(deftest game-over-trash-all-non-icebreaker-programs-hardware-and-resources-not-trashed
    ;; Trash all non-Icebreaker programs, hardware and resources not trashed
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

(deftest game-over-can-t-afford-to-prevent-any-trashes
    ;; Can't afford to prevent any trashes
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
      (is (no-prompt? state :runner) "No prevention prompt for the Runner")
      (is (= 2 (:credit (get-runner))))
      (is (= 2 (-> (get-runner) :discard count)) "2 programs trashed")
      (is (some? (find-card "Nyashia" (:discard (get-runner)))) "Nyashia trashed")
      (is (some? (find-card "Takobi" (:discard (get-runner)))) "Takobi trashed")
      (is (= 1 (count-bad-pub state)))))

(deftest genotyping
  ;; Genotyping
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Genotyping"]
                      :discard ["Ice Wall" "Fire Wall" "Hostile Takeover" "Prisec"]}})
    (play-from-hand state :corp "Genotyping")
    (is (= 6 (count (:discard (get-corp)))) "Corp should trash top two cards to Genotyping")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Fire Wall")
    (click-card state :corp "Hostile Takeover")
    (click-card state :corp "Prisec")
    (is (= ["Fire Wall" "Hedge Fund" "Hedge Fund" "Hedge Fund" "Hostile Takeover" "Ice Wall" "Prisec"]
           (->> (get-corp) :deck (map :title) sort))
        "All four chosen cards should be shuffled back into R&D")
    (is (= ["Genotyping"] (->> (get-corp) :rfg (map :title))) "Genotyping should be rfg'd")))

(deftest government-subsidy
  ;; Government Subsidy
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Government Subsidy"]
                      :credits 100}})
    (is (changed? [(:credit (get-corp)) 5]
          (play-from-hand state :corp "Government Subsidy"))
        "Corp gains 15 credits")))

(deftest greasing-the-palm
  (do-game
    (new-game {:corp {:hand [(qty "Greasing the Palm" 2) "NGO Front" "PAD Campaign"]}})
    (play-from-hand state :corp "Greasing the Palm")
    (click-card state :corp "PAD Campaign")
    (click-prompt state :corp "New remote")
    (is (= "PAD Campaign" (:title (get-content state :remote1 0))))
    (is (= 7 (:credit (get-corp))) "Corp gained a net of 2 credits")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Greasing the Palm")
    (click-card state :corp "NGO Front")
    (click-prompt state :corp "New remote")
    (is (changed? [(count-tags state) -1]
          (click-prompt state :corp "Yes"))
        "Remvoed 1 tag to pay Greasing the Palm cost")
    (is (= 1 (get-counters (refresh (get-content state :remote2 0)) :advancement)))))

(deftest green-level-clearance
  ;; Green Level Clearance
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Green Level Clearance"]}})
    (play-from-hand state :corp "Green Level Clearance")
    (is (= (+ 5 -1 +3) (:credit (get-corp))) "Corp should gain net 2 credits")
    (is (= 1 (count (:hand (get-corp)))) "Corp should draw 1 card")))

(deftest hangeki
  ;; Hangeki
  (doseq [choice ["Yes" "No"]]
    (testing (str "choosing to " (when (= choice "No") "not ") "access card")
      (do-game
        (new-game {:corp {:deck ["Hostile Takeover" "Dedicated Response Team" "Hangeki"]}})
        (play-from-hand state :corp "Hostile Takeover" "New remote")
        (play-from-hand state :corp "Dedicated Response Team" "New remote")
        (take-credits state :corp)
        (run-empty-server state :remote2)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (take-credits state :runner)
        (play-from-hand state :corp "Hangeki")
        (click-card state :corp (get-content state :remote1 0))
        (is (last-log-contains? state "choose a card in Server 1"))
        (click-prompt state :runner choice)
        (if (= "Yes" choice)
          (do (click-prompt state :runner "Steal")
              (is (= 1 (:agenda-point (get-runner))) "Runner should steal Hostile Takeover")
              (is (= 1 (-> (get-corp) :rfg count)) "Hangeki should be removed from the game"))
          (do (is (no-prompt? state :runner) "Runner should have no more prompts as access ended")
              (is (= -1 (:agenda-point (get-runner))) "Runner should add Hangeki to their score area worth -1 agenda point")
              (is (zero? (-> (get-corp) :rfg count)) "Hangeki shouldn't be removed from the game")))))))

(deftest hansei-review
  ;; Hansei Review
  (do-game
      (new-game {:corp {:hand ["Hansei Review" "IPO"]}})
      (is (= 5 (:credit (get-corp))) "Starting with 5 credits")
      (play-from-hand state :corp "Hansei Review")
      (is (zero? (:credit (get-corp))) "Now at 0 credits")
      (click-card state :corp "IPO")
      (is (= 10 (:credit (get-corp))) "Now at 10 credits")
      (is (= 2 (count (:discard (get-corp)))))))

(deftest hansei-review-no-cards
  ;; Hansei Review - with an empty hand, should not get a trash prompt
  (do-game
      (new-game {:corp {:hand ["Hansei Review"]}})
      (is (= 5 (:credit (get-corp))) "Starting with 5 credits")
      (play-from-hand state :corp "Hansei Review")
      (is (no-prompt? state :corp) "Corp should have no prompt with no cards in hand")
      (is (= 10 (:credit (get-corp))) "Now at 10 credits")
      (is (= 1 (count (:discard (get-corp)))))))

(deftest hard-hitting-news
  ;; Hard-Hitting News
  (do-game
    (new-game {:corp {:deck ["Hard-Hitting News"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should start with 3 clicks")
    (play-from-hand state :corp "Hard-Hitting News")
    (is (zero? (count-tags state)) "Runner should start with 0 tags")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 4 (count-tags state)) "Runner should gain 4 tags from losing Hard-Hitting News trace")
    (is (zero? (:click (get-corp))) "Playing Hard-Hitting News should lose all remaining clicks")))

(deftest hasty-relocation
  ;; Hasty Relocation
  (do-game
    (new-game {:corp {:hand ["Hasty Relocation"
                             "Accelerated Beta Test" "Brainstorm" "Chiyashi"
                             "DNA Tracker" "Excalibur" "Fire Wall"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Excalibur" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
      (is (empty? (:discard (get-corp))) "Archives starts empty")
      (play-from-hand state :corp "Hasty Relocation")
      (is (= "Accelerated Beta Test" (-> (get-corp) :discard first :title)) "ABT is in Archives")
      (is (= ["Brainstorm" "Chiyashi" "DNA Tracker"] (prompt-titles :corp)) "Next 3 cards are in HQ")
      (click-prompt state :corp "Brainstorm")
      (click-prompt state :corp "Chiyashi")
      (click-prompt state :corp "DNA Tracker")
      (click-prompt state :corp "Done")
      (is (= "DNA Tracker" (-> (get-corp) :deck first :title)) "DNA Tracker is now on top of R&D")))

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
    (is (not (no-prompt? state :corp)))
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
    (draw state :runner)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (play-from-hand state :corp "Hellion Alpha Test")
    (is (zero? (count-bad-pub state)) "Corp should start with 0 bad publicity")
    (click-prompt state :corp "0")
    (click-prompt state :runner "2")
    (is (= 1 (count-bad-pub state)) "Corp should gain 1 bad publicity from losing Hellion Alpha Test trace")))

(deftest hellion-beta-test-winning-trace-trashing-2-cards
    ;; Winning Trace - Trashing 2 cards
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
      (is (zero? (-> (get-runner) :discard count)) "heap should be empty")
      (play-from-hand state :corp "Hellion Beta Test")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-resource state 0))
      (click-card state :corp (get-hardware state 0))
      (is (= 2 (-> (get-runner) :discard count)) "Runner should have 2 cards in heap after losing Hellion Beta Test trace")))

(deftest hellion-beta-test-losing-trace-gaining-bad-publicity
    ;; Losing trace - Gaining bad publicity
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
      (is (= 1 (count-bad-pub state)) "Corp should gain 1 bad publicity from losing Hellion Beta Test trace")))

(deftest heritage-committee
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Heritage Committee" "Hostile Takeover"]}})
    (play-from-hand state :corp "Heritage Committee")
    (is (= 4 (count (:hand (get-corp)))) "Corp should draw 3 cards")
    (click-card state :corp "Hostile Takeover")
    (is (= "Hostile Takeover" (-> (get-corp) :deck first :title)) "Hostile Takeover should be moved to the top of R&D")))

(deftest high-profile-target-when-the-runner-has-no-tags
    ;; when the runner has no tags
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))

(deftest high-profile-target-when-the-runner-has-one-tag
    ;; when the runner has one tag
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (gain-tags state :runner 1)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (count (:hand (get-runner)))) "Runner has 3 cards in hand")))

(deftest high-profile-target-when-the-runner-has-two-tags
    ;; when the runner has two tags
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (gain-tags state :runner 2)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))

(deftest high-profile-target-when-the-runner-has-three-tags-gg
    ;; When the runner has three tags, gg
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 10)]}})
      (gain-tags state :runner 3)
      (play-from-hand state :corp "High-Profile Target")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest housekeeping-trash-effect
    ;; Trash effect
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
      (is (no-prompt? state :runner) "Fall Guy prevention didn't trigger")
      (is (= 1 (count (:discard (get-runner)))) "Card trashed")
      (play-from-hand state :runner "Cache")
      (is (no-prompt? state :runner) "Housekeeping didn't trigger on 2nd install")))

(deftest housekeeping-interaction-with-hayley-issue-1869
    ;; Interaction with Hayley (issue #1869)
    (do-game
      (new-game {:corp {:deck ["Housekeeping"]}
                 :runner {:id "Hayley Kaplan: Universal Scholar"
                          :deck [(qty "Cache" 2) "Fall Guy"]}})
      (play-from-hand state :corp "Housekeeping")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Cache" (:hand (get-runner))))
      (click-card state :runner "Fall Guy")
      (is (no-prompt? state :runner) "No second trash prompt from Housekeeping")))

(deftest hunter-seeker
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hunter Seeker" "Hostile Takeover"]}
               :runner {:hand ["Kati Jones"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (take-credits state :runner)
    (play-from-hand state :corp "Hunter Seeker")
    (is (no-prompt? state :corp) "Corp should have no prompt without agenda stolen")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Hunter Seeker")
    (click-card state :corp "Kati Jones")
    (is (not (get-resource state 0)) "Kati should be trashed")))

(deftest hyoubu-precog-manifold
  ;; Hyoubu Precog Manifold
  (do-game
      (new-game {:corp {:hand ["Hyoubu Precog Manifold" "Hedge Fund"]}})
      (play-from-hand state :corp "Hyoubu Precog Manifold")
      (click-prompt state :corp "HQ")
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (not (:run @state)) "Run ended")
      (run-empty-server state :hq)
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (:run @state) "Run still going")))

(deftest hypoxia
  (do-game
    (new-game {:corp {:hand ["Hypoxia" "Public Trail"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (play-from-hand state :corp "Hypoxia")
    (is (= 2 (count (:hand (get-corp)))) "not played, no tag")
    (play-from-hand state :corp "Public Trail")
    (click-prompt state :runner "Take 1 tag")
    (play-from-hand state :corp "Hypoxia")
    (is (= 1 (count (:rfg (get-corp)))) "Hypoxia removed from game")
    (is (= 1 (:brain-damage (get-runner))) "Runner should get 1 core damage from Hypoxia")
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))) "Runner should lose 1 click start of turn")))

(deftest interns
  ;; Fire Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Interns" "Ice Wall" "Fire Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Interns")
    (click-card state :corp "Fire Wall")
    (click-prompt state :corp "HQ")
    (is (= 5 (:credit (get-corp))) "Installing second ice on HQ shouldn't cost anything")))

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
    (is (= ["Fall Guy" "Sure Gamble" nil] (prompt-titles :corp)))
    (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner))))
    (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner))))
    (is (= 3 (count (:hand (get-runner)))))
    ;; able to trash 2 cards but only 1 available target in Runner's hand
    (play-from-hand state :corp "Invasion of Privacy")
    (click-prompt state :corp "0") ; default trace
    (click-prompt state :runner "0") ; Runner won't match
    (is (= 3 (count (:hand (get-runner)))))
    (is (= ["Fall Guy" nil] (prompt-titles :corp)))
    (click-prompt state :corp (find-card "Fall Guy" (:hand (get-runner))))
    (is (no-prompt? state :corp) "No prompt for second card")
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

(deftest kakurenbo
  ;; Kakurenbo
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 8) "Project Junebug" "Kakurenbo"]}})
      (take-credits state :corp)
      (dotimes [n 5]
        (click-card state :corp (nth (:hand (get-corp)) n)))
      (run-empty-server state :archives)
      (is (= 5 (count (:discard (get-corp)))) "5 cards in Archives")
      (is (empty? (remove :seen (:discard (get-corp)))) "Cards in Archives are faceup")
      (take-credits state :runner)
      (play-from-hand state :corp "Kakurenbo")
      (is (= 0 (count (:rfg (get-corp)))) "Kakurenbo was not yet removed from game")
      (click-card state :corp (find-card "Project Junebug" (:hand (get-corp))))
      (click-prompt state :corp "Done")
      (is (empty? (remove #(not (:seen %)) (:discard (get-corp)))) "Cards in Archives are turned facedown")
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (not (no-prompt? state :corp)) "Could not choose operation to install")
      (click-card state :corp (find-card "Project Junebug" (:discard (get-corp))))
      (is (= 0 (count (:rfg (get-corp)))) "Kakurenbo was not yet removed from game")
      (click-prompt state :corp "New remote")
      (is (= "Project Junebug" (:title (get-content state :remote1 0))) "Installed Junebug in remote")
      (is (= 2 (get-counters (get-content state :remote1 0) :advancement)) "Junebug has 2 advancement tokens")
      (is (= 4 (count (:hand (get-corp)))) "4 Hedge Funds left in HQ")
      (is (empty? (remove #(not (:seen %)) (:discard (get-corp)))) "Remaining cards in Archives are still facedown")
      (is (= 5 (count (:discard (get-corp)))) "5 cards in Archives")
      (is (= 1 (count (:rfg (get-corp)))) "Kakurenbo was removed from game")))

(deftest kakurenbo-can-turn-cards-facedown-without-installing
    ;; Can turn cards facedown without installing
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 8) "Project Junebug" "Kakurenbo"]}})
      (take-credits state :corp)
      (dotimes [n 5]
        (click-card state :corp (nth (:hand (get-corp)) n)))
      (run-empty-server state :archives)
      (is (= 5 (count (:discard (get-corp)))) "5 cards in Archives")
      (is (empty? (remove :seen (:discard (get-corp)))) "Cards in Archives are faceup")
      (take-credits state :runner)
      (play-from-hand state :corp "Kakurenbo")
      (click-prompt state :corp "Done")
      (is (every? (complement faceup?) (:discard (get-corp))) "Cards in Archives are facedown")))

(deftest kakurenbo-works-if-0-cards-are-chosen-to-be-discarded-issue-4794
    ;; Works if 0 cards are chosen to be discarded. Issue #4794
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Project Junebug" "Kakurenbo"]
                        :discard ["Launch Campaign"]}})
      (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
      (play-from-hand state :corp "Kakurenbo")
      (is (= 0 (count (:rfg (get-corp)))) "Kakurenbo was not yet removed from game")
      (click-prompt state :corp "Done")
      (click-card state :corp "Launch Campaign")
      (click-prompt state :corp "New remote")
      (is (get-content state :remote1 0) "Launch Campaign is installed")))

(deftest kakurenbo-works-if-0-cards-are-chosen-to-be-discarded-and-0-card-are-chosen-to-be-installed-issue-4794
    ;; Works if 0 cards are chosen to be discarded and 0 card are chosen to be installed. Issue #4794
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Project Junebug" "Kakurenbo"]
                        :discard ["Launch Campaign"]}})
      (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
      (play-from-hand state :corp "Kakurenbo")
      (is (= 0 (count (:rfg (get-corp)))) "Kakurenbo was not yet removed from game")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Done")
      (is (= 1 (count (:rfg (get-corp)))) "Kakurenbo was removed from game")
      (is (no-prompt? state :corp) "No more prompts")))


(deftest kill-switch
  ;; Kill Switch
  (do-game
    (new-game {:corp {:deck ["Kill Switch" (qty "Hostile Takeover" 2)]}})
    (play-from-hand state :corp "Kill Switch")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 core damage")
    (play-and-score state "Hostile Takeover")
    (click-prompt state :corp "Hostile Takeover")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner should get 1 core damage from Kill Switch after Corp scores an agenda")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 2 (:brain-damage (get-runner))) "Runner should get 1 core damage from Kill Switch after accecssing an agenda")))

(deftest lag-time
  (do-game
    (new-game {:corp {:deck ["Lag Time" "Vanilla" "Lotus Field"]}})
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Lotus Field" "R&D")
    (play-from-hand state :corp "Lag Time")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :rd 0))
    (is (= 1 (get-strength (get-ice state :hq 0))) "Vanilla at 1 strength")
    (is (= 5 (get-strength (get-ice state :rd 0))) "Lotus Field at 5 strength")))

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

(deftest lateral-growth-no-installable-cards
  (do-game
    (new-game {:corp {:deck ["Lateral Growth" "Hedge Fund"]}})
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Lateral Growth")
    (is (no-prompt? state :corp) "Corp should have no prompt")
    (is (= 7 (:credit (get-corp))))))

(deftest liquidation
  ;; Marilyn Campaign
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Liquidation" "PAD Campaign" "Launch Campaign" "Marilyn Campaign"]
                      :credits 10}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Marilyn Campaign" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (rez state :corp (get-content state :remote2 0))
    (play-from-hand state :corp "Liquidation")
    (let [credits (:credit (get-corp))]
      (click-card state :corp "Marilyn Campaign")
      (click-card state :corp "PAD Campaign")
      (click-card state :corp "Launch Campaign")
      (is (installed? (get-content state :remote3 0)) "Marilyn Campaign should still be installed")
      (is (= (+ credits 6) (:credit (get-corp))) "Corp should gain 6 for 2 assets trashed"))))

(deftest load-testing
  ;; Load Testing
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Load Testing"]}})
    (play-from-hand state :corp "Load Testing")
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))) "Runner should lose 1 click start of turn")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner should gain 4 clicks per turn again")))

(deftest localized-product-line
  ;; Localized Product Line
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5) (qty "Hostile Takeover" 2)]
                      :hand ["Localized Product Line"]}})
    (play-from-hand state :corp "Localized Product Line")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "5")
    (is (= 5 (count (:hand (get-corp)))) "Corp should have all 5 Hedge Funds in hand")))

(deftest manhunt
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game {:corp {:deck ["Manhunt" (qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Manhunt")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (is (not (no-prompt? state :corp)) "Manhunt trace initiated")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count-tags state)) "Runner took 1 tag")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (run-empty-server state "HQ")
    (is (no-prompt? state :corp) "No Manhunt trace on second run")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest market-forces-full-test
    ;; Full test
    (letfn [(market-forces-credit-test
              [{:keys [tag-count runner-creds expected-credit-diff]}]
              (testing (str "when the runner has " tag-count " tags and " runner-creds " credits")
                (do-game
                  (new-game {:corp {:deck [(qty "Market Forces" 6)]}})
                  (swap! state assoc-in [:corp :credit] 0)
                  (swap! state assoc-in [:runner :credit] runner-creds)
                  (gain-tags state :runner tag-count)
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

(deftest market-forces-when-the-runner-is-not-tagged
    ;; when the runner is not tagged
    (do-game
      (new-game {:corp {:deck [(qty "Market Forces" 6)]}})
      (play-from-hand state :corp "Market Forces")
      (is (= 6 (count (:hand (get-corp))))
          "Market Forces is not played")
      (is (= 3 (:click (get-corp)))
          "the corp does not spend a click")
      (is (= 5 (:credit (get-corp)) (:credit (get-runner)))
          "credits are unaffected")))

(deftest mass-commercialization
  ;; Mass Commercialization
  (do-game
    (new-game {:corp {:deck ["Mass Commercialization"
                             (qty "Ice Wall" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :runner)
    (click-advance state :corp (refresh (get-ice state :hq 0)))
    (click-advance state :corp (refresh (get-ice state :archives 0)))
    (click-advance state :corp (refresh (get-ice state :rd 0)))
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

(deftest media-blitz
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Media Blitz" "Gila Hands Arcology" "Hostile Takeover"]}})
    (play-from-hand state :corp "Gila Hands Arcology" "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Media Blitz")
    (click-card state :corp "Gila Hands Arcology")
    (let [credits (:credit (get-corp))]
      (card-ability state :corp (first (:current (get-corp))) 0)
      (is (= (+ credits 3) (:credit (get-corp))) "Corp should gain 3 from Media Blitz' GT ability"))
    (take-credits state :corp)
    (run-empty-server state :remote2)
    (click-prompt state :runner "Steal")
    (is (last-log-contains? state "Media Blitz is trashed") "Media Blitz should be trashed")))

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
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Stole Breaking News")
      (take-credits state :runner)
      (play-from-hand state :corp "Midseason Replacements")
      (click-prompt state :corp "0") ; default trace
      (click-prompt state :runner "0") ; Runner won't match
    (is (= 6 (count-tags state)) "Runner took 6 tags")))

(deftest mindscaping
  (do-game
    (new-game {:corp {:hand [(qty "Mindscaping" 2)]
                      :deck ["Hedge Fund" "NGO Front"]}
               :runner {:hand [(qty "Sure Gamble" 3)]}})
    (gain-tags state :runner 5)
    (play-from-hand state :corp "Mindscaping")
    (is (changed? [(count (:hand (get-runner))) -3]
          (click-prompt state :corp "Do 1 net damage per tag (up to 3)"))
        "Runner got 3 damage")
    (play-from-hand state :corp "Mindscaping")
    (click-prompt state :corp "Gain 4 [Credits] and draw 2 cards")
    (click-card state :corp "NGO Front")
    (is (= 5 (:credit (get-corp))) "Corp gained a net of 2 credits")
    (is (= 1 (count (:hand (get-corp)))))
    (is (= "NGO Front" (-> (get-corp) :deck first :title)))))

(deftest mitosis
  ;; Mitosis - Install up to 2 cards in new remotes, placing 2 advancements on each
  ;; prevent rez/score of those cards the rest of the turn
  (do-game
    (new-game {:corp {:deck [(qty "Mitosis" 2) "Ronin" "Clone Retirement"]}})
    (play-from-hand state :corp "Mitosis")
    (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
    (click-card state :corp (find-card "Clone Retirement" (:hand (get-corp))))
    (let [ronin (get-content state :remote1 0)
          clone (get-content state :remote2 0)]
      (is (= 2 (get-counters (refresh ronin) :advancement)) "2 advancements placed on Ronin")
      (is (= 2 (get-counters (refresh clone) :advancement)) "2 advancements placed on Ronin")
      (rez state :corp (refresh ronin) {:expect-rez false})
      (score state :corp (refresh clone))
      (is (empty? (:scored (get-corp))) "Clone Retirement not scored")
      (is (zero? (:agenda-point (get-corp))))
      (take-credits state :corp)
      (take-credits state :runner)
      (rez state :corp (refresh ronin))
      (is (rezzed? (refresh ronin)) "Ronin now rezzed")
      (score state :corp (refresh clone))
      (is (= 1 (:agenda-point (get-corp))) "Clone Retirement was able to be scored"))))

(deftest mushin-no-shin
  ;; Mushin No Shin - Add 3 advancements to a card; prevent rez/score of that card the rest of the turn
  (do-game
    (new-game {:corp {:deck [(qty "Mushin No Shin" 2) "Ronin" "Profiteering"]}})
    (play-from-hand state :corp "Mushin No Shin")
    (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
    (let [ronin (get-content state :remote1 0)]
      (is (= 3 (get-counters (refresh ronin) :advancement)) "3 advancements placed on Ronin")
      (rez state :corp (refresh ronin) {:expect-rez false})
      (take-credits state :corp)
      (rez state :corp (refresh ronin) {:expect-rez false})
      (take-credits state :runner)
      (rez state :corp (refresh ronin))
      (is (rezzed? (refresh ronin)) "Ronin now rezzed")
      (play-from-hand state :corp "Mushin No Shin")
      (click-card state :corp (find-card "Profiteering" (:hand (get-corp))))
      (let [prof (get-content state :remote2 0)]
        (score state :corp (refresh prof))
        (is (empty? (:scored (get-corp))) "Profiteering not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (score state :corp (refresh prof))
        (click-prompt state :corp "0")
        (is (= 1 (:agenda-point (get-corp))) "Profiteering was able to be scored")))))

(deftest mutate-basic-operation
    ;; Basic operation
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma is installed")
      (is (rezzed? (get-ice state :hq 0)) "Enigma is rezzed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Installed card name was logged")))

(deftest mutate-no-ice-in-r-d
    ;; No ice in R&D
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :hq 0))
      (is (empty? (get-ice state :hq)) "No ice installed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")))

(deftest mutate-remote-server
    ;; Remote server
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (is (= 1 (count (get-ice state :remote1))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :remote1 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :remote1 0))
      (is (= 1 (count (get-ice state :remote1))) "1 ice installed")
      (is (= "Enigma" (:title (get-ice state :remote1 0))) "Enigma is installed")
      (is (rezzed? (get-ice state :remote1 0)) "Enigma is rezzed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Installed card name was logged")))

(deftest mutually-assured-destruction-test
  ;;"Mutually Assured Destruction"
  ;; trash x installed rezzed cards to give the runner x tags
  ;; triple
  (do-game
   (new-game {:corp {:hand [(qty "PAD Campaign" 3) "Mutually Assured Destruction"]
                     :credits 15}})
   (core/gain state :corp :click 5)
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (rez state :corp (get-content state :remote1 0))
   (rez state :corp (get-content state :remote2 0))
   (rez state :corp (get-content state :remote3 0))
   (is (changed? [(:click (get-corp)) -3]
         (play-from-hand state :corp "Mutually Assured Destruction")
         (click-card state :corp (get-content state :remote1 0))
         (click-card state :corp (get-content state :remote2 0))
         (click-prompt state :corp "Done"))
       "Spent 3 clicks to go MAD")
   (is (= 2 (count-tags state)) "Runner should have two tags from MAD")
   (is (= 3 (count (:discard (get-corp)))) "MAD + 2 cards in discard")))

(deftest napd-cordon
  ;; NAPD Cordon
  (do-game
      (new-game {:corp {:deck ["NAPD Cordon" "Project Atlas"]}
                 :runner {:credits 8}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "NAPD Cordon")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote1 0)]
        (dotimes [_ 2] (click-advance state :corp (refresh atlas)))
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (is (changed? [(:credit (get-runner)) -8]
              (click-prompt state :runner "Pay to steal"))
            "Paid 8c to steal 1adv Atlas"))))

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

(deftest neurospike
  ;; Neurospike
  (do-game
    (new-game {:runner {:hand [(qty "Sure Gamble" 5)]}
               :corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Neurospike" 2) "Obokata Protocol"]
                      :credits 50
                      :click 8}})
    (play-and-score state "Obokata Protocol")
    (is (changed? [(count (:hand (get-runner))) -3]
          (play-from-hand state :corp "Neurospike"))
        "Runner took 3 net damage")
    (play-from-hand state :corp "Neurospike")
    (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
    (is (= :corp (:winner @state)) "Corp wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest next-activation-command-lockdowns-restriction
  ;; Can't play if there's an active lockdown
  (do-game
   (new-game {:corp {:hand ["NEXT Activation Command" "NEXT Activation Command"]}})
   (is (= 0 (count (:play-area (get-corp)))) "Play area is empty")
   (is (= 0 (count (:discard (get-corp)))) "Discard is empty")
   (play-from-hand state :corp "NEXT Activation Command")
   (is (= 1 (count (:play-area (get-corp)))) "NAC in play area")
   (play-from-hand state :corp "NEXT Activation Command")
   (is (= 1 (count (:play-area (get-corp)))) "ONLY one NAC in play area")
   (is (= 0 (count (:discard (get-corp)))) "Discard is empty")))

(deftest next-activation-command-get-trashed-at-start-of-next-corp-turn
    ;; Get trashed at start of next Corp turn
    (do-game
      (new-game {:corp {:hand ["NEXT Activation Command"]}})
      (is (= 0 (count (:play-area (get-corp)))) "Play area is empty")
      (is (= 0 (count (:discard (get-corp)))) "Discard is empty")
      (play-from-hand state :corp "NEXT Activation Command")
      (is (= 1 (count (:play-area (get-corp)))) "NAC in play area")
      (is (= 0 (count (:discard (get-corp)))) "Discard is empty")
      (take-credits state :corp)
      (is (= 1 (count (:play-area (get-corp)))) "NAC in play area")
      (take-credits state :runner)
      (is (= 0 (count (:play-area (get-corp)))) "NAC left play area")
      (is (= 1 (count (:discard (get-corp)))) "NAC in discard")))

(deftest next-activation-command-prevents-break-abilities-on-non-icebreakers
    ;; Prevents break abilities on non-icebreakers
    (do-game
      (new-game {:corp {:hand ["NEXT Activation Command" "Eli 1.0"]}
                 :runner {:hand ["Self-modifying Code" "Corroder" "D4v1d"]
                          :credits 15
                          :deck [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "NEXT Activation Command")
      (play-from-hand state :corp "Eli 1.0" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "D4v1d")
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Corroder")
      (let [eli (get-ice state :hq 0)
            d4 (get-program state 0)
            smc (get-program state 1)
            cor (get-program state 2)]
        (run-on state :hq)
        (rez state :corp (get-ice state :hq 0))
        (run-continue state)
        (card-ability state :runner d4 0)
        (is (no-prompt? state :runner) "Can't use D4v1d")
        (card-side-ability state :runner eli 0)
        (is (no-prompt? state :runner) "Can't use break ability on Eli")
        (card-ability state :runner smc 0) ; Can still use SMC
        (click-prompt state :runner "Done")
        (is (changed? [(:credit (get-runner)) -6]
              (auto-pump-and-break state (refresh cor)))
            "Paid 4+2 to pump and break 6 strength Eli"))))

(deftest next-activation-command-strength-bonus-doesn-t-persist-after-trash-issue-4710
    ;; Strength bonus doesn't persist after trash. Issue #4710
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["NEXT Activation Command" "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [iw (get-ice state :hq 0)]
        (rez state :corp (refresh iw))
        (is (= 1 (core/get-strength (refresh iw))))
        (play-from-hand state :corp "NEXT Activation Command")
        (is (= 3 (core/get-strength (refresh iw))))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 1 (core/get-strength (refresh iw))))
        (is (find-card "NEXT Activation Command" (:discard (get-corp)))))))

(deftest nonequivalent-exchange
    ;; Nonequivalent Exchange
    (do-game
      (new-game {:corp {:hand [(qty "Nonequivalent Exchange" 2)]}})
      (play-from-hand state :corp "Nonequivalent Exchange")
      (is (changed? [(:credit (get-runner)) 2
             (:credit (get-corp)) 7]
            (click-prompt state :corp "Yes"))
          "Runner gained 2 credits")
      (play-from-hand state :corp "Nonequivalent Exchange")
      (is (changed? [(:credit (get-runner)) 0
             (:credit (get-corp)) 5]
            (click-prompt state :corp "No"))
          "Runner gained no credits")))

(deftest o-shortage
  ;; O₂ Shortage
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "O₂ Shortage" 2)]
                      :credits 10}
               :runner {:hand ["Sure Gamble"]}})
    (play-from-hand state :corp "O₂ Shortage")
    (is (changed? [(count (:hand (get-runner))) -1]
          (click-prompt state :runner "Trash 1 random card from the grip"))
        "Runner discarded a single card")
    (play-from-hand state :corp "O₂ Shortage")
    (is (= ["The Corp gains [Click][Click]"] (prompt-buttons :runner)) "Runner has no longer the option to trash from the grip")
    (is (changed? [(:click (get-corp)) 2]
          (click-prompt state :runner "The Corp gains [Click][Click]"))
        "Corp gained 2 clicks")
    (is (no-prompt? state :runner) "Runner should have no more prompt")))

(deftest observe-and-destroy
  ;; Observe and Destroy
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Observe and Destroy"]}
               :runner {:hand ["Kati Jones"]
                        :credits 2
                        :tags 1}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (take-credits state :runner)
    (play-from-hand state :corp "Observe and Destroy")
    (click-card state :corp "Kati Jones")
    (is (zero? (count-tags state)) "Runner should lose 1 tag")
    (is (not (get-resource state 0)) "Kati should be trashed")))

(deftest oppo-research
  (do-game
    (new-game {:corp {:hand [(qty "Oppo Research" 3) "Project Beale" "Rashida Jaheem"]
                      :credits 10}})
    (play-from-hand state :corp "Project Beale" "New remote")
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (is (changed? [(count-tags state) 2]
          (play-from-hand state :corp "Oppo Research"))
        "Runner got 2 tags")
    (take-credits state :corp)
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (take-credits state :runner)
    (play-from-hand state :corp "Oppo Research")
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (take-credits state :runner)
    (is (changed? [(count-tags state) 0]
          (play-from-hand state :corp "Oppo Research"))
        "Runner got no tags")))

(deftest oppo-research-threat-ability
  (do-game
    (new-game {:corp {:hand ["Oppo Research" "Salvo Testing"]
                      :credits 10}})
    (play-from-hand state :corp "Salvo Testing" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (is (changed? [(count-tags state) 4]
          (play-from-hand state :corp "Oppo Research")
          (click-prompt state :corp "Yes"))
        "Runner got 4 tags")
    (is (= 5 (:credit (get-corp))) "Corp spent 5 additional credits")))

(deftest oppo-research-threat-ability-cannot-pay
  (do-game
    (new-game {:corp {:hand ["Oppo Research" "Salvo Testing"]
                      :credits 2}})
    (play-from-hand state :corp "Salvo Testing" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Oppo Research")
    (is (= 1 (count (:choices (prompt-map :corp)))) "Corp cannot pay the additional 5 credits")
    (click-prompt state :corp "No")))

(deftest oppo-research-threat-ability-tag-prevention
  (do-game
    (new-game {:corp {:hand ["Oppo Research" "Salvo Testing"]
                      :credits 10}
               :runner {:hand ["No One Home"]}})
    (play-from-hand state :corp "Salvo Testing" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "No One Home")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (is (changed? [(count-tags state) 2]
          (play-from-hand state :corp "Oppo Research")
          (is (not (no-prompt? state :runner)) "Runner prompted to avoid tag")
          (card-ability state :runner (get-resource state 0) 0)
          (click-prompt state :corp "0")
          (click-prompt state :runner "0")
          (click-prompt state :runner "Done")
          (click-prompt state :corp "Yes"))
        "Runner prevented 2 tag")))

(deftest oversight-ai-rez-at-no-cost
    ;; Rez at no cost
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Oversight AI" "Archer"]}})
      (play-from-hand state :corp "Archer" "R&D")
      (let [archer (get-ice state :rd 0)]
        (play-from-hand state :corp "Oversight AI")
        (click-card state :corp archer)
        (is (rezzed? (refresh archer)))
        (is (= 4 (:credit (get-corp))) "Archer rezzed at no credit cost")
        (is (= "Oversight AI" (get-title (first (:hosted (refresh archer)))))
            "Archer hosting OAI as a condition")
        (is (last-log-contains? state "Corp uses Oversight AI to rez ice protecting R&D at position 0 at no cost.")
                  "The right information is printed to the log"))))

(deftest oversight-ai-trash-rezzed-ice-when-all-subs-are-broken-issue-4752
    ;; Trash rezzed ice when all subs are broken. Issue #4752
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Oversight AI" "Archer"]}
                 :runner {:hand ["Bukhgalter"]
                          :credits 20}})
      (play-from-hand state :corp "Archer" "R&D")
      (let [archer (get-ice state :rd 0)]
        (play-from-hand state :corp "Oversight AI")
        (click-card state :corp archer)
        (is (rezzed? (refresh archer)))
        (take-credits state :corp)
        (play-from-hand state :runner "Bukhgalter")
        (let [bukh (get-program state 0)]
          (run-on state "R&D")
          (run-continue state)
          (card-ability state :runner bukh 1)
          (card-ability state :runner bukh 1)
          (card-ability state :runner bukh 1)
          (card-ability state :runner bukh 1)
          (card-ability state :runner bukh 1)
          (card-ability state :runner bukh 0)
          (click-prompt state :runner "Gain 2 [Credits]")
          (click-prompt state :runner "Trash a program")
          (click-prompt state :runner "Trash a program")
          (click-prompt state :runner "End the run")
          (is (not (refresh archer)) "Archer is trashed")))
      (is (find-card "Archer" (:discard (get-corp))) "Archer is in the discard")
      (is (find-card "Oversight AI" (:discard (get-corp))) "Oversight AI is in the discard")
      (is (last-log-contains? state "Corp uses Oversight AI to trash itself and Archer protecting R&D at position 0.")
          "The right information is printed to the log")))

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game {:corp {:deck ["Patch" "Vanilla"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Patch")
    (click-card state :corp (get-ice state :hq 0))
    (is (= 2 (get-strength (get-ice state :hq 0))) "Vanilla at 2 strength")))

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
  ;; Peak Efficiency - Gain 1 credit for each rezzed piece of ice
  (do-game
    (new-game {:corp {:deck ["Peak Efficiency" (qty "Paper Wall" 3) "Wraparound"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Paper Wall" "New remote")
    (play-from-hand state :corp "Wraparound" "New remote")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :rd 0))
    (rez state :corp (get-ice state :remote1 0))
    (play-from-hand state :corp "Peak Efficiency")
    (is (= 7 (:credit (get-corp))) "Gained 3 credits for 3 rezzed pieces of ice; unrezzed ice ignored")))

(deftest pivot-gets-operations
  (do-game
      (new-game {:corp {:hand ["Pivot"]
                        :deck ["Hedge Fund" "PAD Campaign" "Project Atlas"]}})
      (is (changed? [(:click (get-corp)) -2]
            (play-from-hand state :corp "Pivot")
            (is (= ["Hedge Fund" "Project Atlas"] (prompt-titles :corp)))
            (click-prompt state :corp "Hedge Fund"))
          "Pivot is a double operation")
      (is (= (:title (first (-> @state :corp :hand))) "Hedge Fund"))))

(deftest pivot-gets-agendas
  (do-game
      (new-game {:corp {:hand ["Pivot"]
                        :deck ["Hedge Fund" "PAD Campaign" "Project Atlas"]}})
      (play-from-hand state :corp "Pivot")
      (is (= ["Hedge Fund" "Project Atlas"] (prompt-titles :corp)))
      (click-prompt state :corp "Project Atlas")
      (is (= (:title (first (-> @state :corp :hand))) "Project Atlas"))))

(deftest pivot-threat-ability-to-install
  (do-game
      (new-game {:corp {:hand ["Pivot" "City Works Project" "NGO Front"]
                        :deck ["Subliminal Messaging" "PAD Campaign"]}})
      (play-and-score state "City Works Project")
      (play-from-hand state :corp "Pivot")
      (is (= ["Subliminal Messaging"] (prompt-titles :corp)))
      (click-prompt state :corp "Subliminal Messaging")
      (click-card state :corp "NGO Front")
      (click-prompt state :corp "New remote")))

(deftest pivot-threat-ability-to-play-operations
  (do-game
      (new-game {:corp {:hand ["Pivot" "City Works Project" "NGO Front"]
                        :deck ["Subliminal Messaging" "PAD Campaign"]}})
      (play-and-score state "City Works Project")
      (play-from-hand state :corp "Pivot")
      (is (= ["Subliminal Messaging"] (prompt-titles :corp)))
      (click-prompt state :corp "Subliminal Messaging")
      (click-card state :corp "Subliminal Messaging")))

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

(deftest power-shutdown-default-behavior
    ;; Default behavior
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
      (is (= 2 (count (:discard (get-corp)))) "2 cards trashed from R&D")
      (is (= 1 (count (:deck (get-corp)))) "1 card remaining in R&D")
      (click-card state :runner (get-hardware state 0)) ; try targeting Grimoire
      (is (empty? (:discard (get-runner))) "Grimoire too expensive to be targeted")
      (click-card state :runner (get-program state 0))
      (is (= 1 (count (:discard (get-runner)))) "Cache trashed")))

(deftest power-shutdown-no-installed-runner-cards
    ;; No installed runner cards
    (do-game
      (new-game {:corp {:deck ["Power Shutdown"]}})
      (take-credits state :corp)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (play-from-hand state :corp "Power Shutdown")))

(deftest power-shutdown-can-trash-any-number-of-cards-up-to-deck-size-per-nisei-cr-1-4-errata-removal-issue-5144
    ;; Can trash any number of cards up to deck size per nisei CR 1.4 errata removal, issue #5144
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Ice Wall" 3)]
                        :hand [(qty "Power Shutdown" 1)]}
                 :runner {:deck ["Grimoire"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Grimoire")
      (run-empty-server state :archives)
      (take-credits state :runner)
      (play-from-hand state :corp "Power Shutdown")
      (click-prompt state :corp "5")
      (is (= 5 (count (:discard (get-corp)))) "5 cards trashed from R&D")
      (is (= 0 (count (:deck (get-corp)))) "0 card remaining in R&D") ;; one card was drawn at turn start
      (click-card state :runner (get-hardware state 0)) ; target grimoire
      (is (= 1 (count (:discard (get-runner)))) "Grimoire trashed")))

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

(deftest predictive-algorithm
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Predictive Algorithm" "Hostile Takeover"]}})
    (play-from-hand state :corp "Predictive Algorithm")
    (take-credits state :corp)
    (run-empty-server state :hq)
    (let [credits (:credit (get-runner))]
      (is (= ["Pay to steal" "No action"] (prompt-buttons :runner)) "Runner has option to pay to steal")
      (click-prompt state :runner "Pay to steal")
      (is (= (+ credits -2) (:credit (get-runner))) "Runner should pay 2 to steal"))))

(deftest predictive-planogram
  ;; Predictive Planogram
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 20)]
                        :hand [(qty "Predictive Planogram" 3)]}})
      (play-from-hand state :corp "Predictive Planogram")
      (is (= 5 (:credit (get-corp))))
      (is (= ["Gain 3 [Credits]" "Draw 3 cards"] (prompt-buttons :corp)))
      (click-prompt state :corp "Gain 3 [Credits]")
      (is (= 8 (:credit (get-corp))))
      (play-from-hand state :corp "Predictive Planogram")
      (is (= 1 (count (:hand (get-corp)))) "1 card left in hq")
      (is (= ["Gain 3 [Credits]" "Draw 3 cards"] (prompt-buttons :corp)))
      (click-prompt state :corp "Draw 3 cards")
      (is (= 4 (count (:hand (get-corp)))) "Corp should draw up to 4 cards")
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Predictive Planogram")
      (is (= ["Gain 3 [Credits]" "Draw 3 cards" "Gain 3 [Credits] and draw 3 cards"] (prompt-buttons :corp)))
      (click-prompt state :corp "Gain 3 [Credits] and draw 3 cards")
      (is (= 6 (count (:hand (get-corp)))) "Corp should draw up to 6 cards")
      (is (= 11 (:credit (get-corp))))))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)
                               "Preemptive Action"]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (second (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))

(deftest preemptive-action-forces-you-to-take-3-if-there-are-three-and-removes-itself-from-game
    ;; forces you to take 3 if there are three, and removes itself from game
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)
                               (qty "Preemptive Action" 1)]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Preemptive Action")
      (is (= 3 (count (:discard (get-corp)))))
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (second (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (= 1 (count (:rfg (get-corp)))))))

(deftest preemptive-action-shuffles-all-archives-cards-into-r-d-if-archives-has-less-than-3-cards-and-removes-itself-from-game
    ;; Shuffles all archives cards into R&D if Archives has less than 3 cards, and removes itself from game
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 2)
                               (qty "Preemptive Action" 1)]}})
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))

(deftest priority-construction
  ;; Ice Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Priority Construction" "Fire Wall" "Ice Wall"]}})
    (play-from-hand state :corp "Fire Wall" "New remote")
    (play-from-hand state :corp "Priority Construction")
    (click-card state :corp "Ice Wall")
    (let [credits (:credit (get-corp))]
      (click-prompt state :corp "Server 1")
      (is (= credits (:credit (get-corp))) "Installing another ice in an iced server shouldn't cost credits")
      (is (= 3 (get-counters (get-ice state :remote1 1) :advancement)) "Ice Wall should be installed with 3 counters on itself"))))

(deftest product-recall
  ;; Crisium Grid
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Product Recall" "Crisium Grid"]}})
    (play-from-hand state :corp "Crisium Grid" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Product Recall")
    (let [credits (:credit (get-corp))]
      (click-card state :corp "Crisium Grid")
      (is (= (+ credits 5) (:credit (get-corp))) "Corp should gain 5 credits from trashing Crisium Grid"))))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Runner tags on a card
  (do-game
    (new-game {:corp {:deck ["Psychographics" "Project Junebug"]}})
    (gain-tags state :runner 4)
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
    (click-prompt state :corp "OK")
    (is (not-any? #{"Mwanza City Grid"} (prompt-buttons :corp))
        "Mwanza City Grid is not on the list of installable cards")
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Caprice Nisei" (:title (get-content state :remote1 0)))
        "Caprice Nisei installed by Psychokinesis")
    ;; Test installing an Asset
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp "OK")
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Adonis Campaign" (:title (get-content state :remote2 0)))
        "Adonis Campaign installed by Psychokinesis")
    ;; Test installing an Agenda
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp "OK")
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Global Food Initiative" (:title (get-content state :remote3 0)))
        "Global Food Initiative installed by Psychokinesis")
    ;; Test selecting "Cancel"
    (core/gain state :corp :click 1)
    (core/move state :corp (find-card "Psychokinesis" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Psychokinesis")
    (click-prompt state :corp "OK")
    (click-prompt state :corp "Cancel")
    (is (nil? (:title (get-content state :remote4 0)))
        "Nothing is installed by Psychokinesis")))

(deftest public-trail
  ;; Public Trail
  (do-game
    (new-game {:corp {:hand [(qty "Public Trail" 2)]}})
    (play-from-hand state :corp "Public Trail")
    (is (no-prompt? state :corp))
    (is (not (is-tagged? state)))
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (play-from-hand state :corp "Public Trail")
    (is (= 8 (:credit (get-runner))))
    (is (= ["Take 1 tag" "Pay 8 [Credits]"] (prompt-buttons :runner)))
    (click-prompt state :runner "Pay 8 [Credits]")
    (is (zero? (:credit (get-runner))))
    (is (not (is-tagged? state)))
    (play-from-hand state :corp "Public Trail")
    (click-prompt state :runner "Take 1 tag")
    (is (is-tagged? state))))

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

(deftest red-level-clearance-can-t-choose-same-option-twice-issue-4150
    ;; Can't choose same option twice. Issue #4150
    (do-game
      (new-game {:corp {:deck [(qty "Beanstalk Royalties" 5)]
                        :hand ["Red Level Clearance"]}})
      (play-from-hand state :corp "Red Level Clearance")
      (is (prompt-is-type? state :runner :waiting))
      (is (= 4 (count (:choices (prompt-map :corp)))))
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 3 (count (:choices (prompt-map :corp)))))
      (is (= ["Draw 2 cards" "Gain [Click]" "Install a non-agenda from hand"]
             (prompt-buttons :corp)))
      (click-prompt state :corp "Gain [Click]")
      (is (no-prompt? state :runner) "Runner should have no more prompt")))

(deftest red-planet-couriers-move-all-advancements-on-cards-to-1-advanceable-card
    ;; Move all advancements on cards to 1 advanceable card
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

(deftest red-planet-couriers-interaction-with-masvingo-should-correctly-reset-subs-issue-5090
    ;; interaction with masvingo - should correctly reset subs issue #5090
    (do-game
     (new-game {:corp {:deck ["Red Planet Couriers" (qty "Masvingo" 2)]}})
     (core/gain state :corp :click 2)
     (core/gain state :corp :credit 6)
     (play-from-hand state :corp "Masvingo" "HQ")
     (play-from-hand state :corp "Masvingo" "R&D")
     (let [mas-hq (get-ice state :hq 0)
           mas-rd (get-ice state :rd 0)]
       (rez state :corp mas-hq)
       (rez state :corp mas-rd)
       (core/add-prop state :corp mas-hq :advance-counter 2)
       (play-from-hand state :corp "Red Planet Couriers")
       (click-card state :corp (refresh mas-rd))
       (is (zero? (get-counters (refresh mas-hq) :advancement)) "Advancements removed")
       (is (zero? (count (:subroutines (refresh mas-hq)))) "Subroutines set to 0")
       (is (= 4 (get-counters (refresh mas-rd) :advancement)) "Increased to 4 advancements")
       (is (= 4 (count (:subroutines (refresh mas-rd)))) "Subroutines set to 4"))))

(deftest restore-show-agenda-name-in-log-when-installed
    ;; Show agenda name in log when installed
    (do-game
      (new-game {:corp {:discard ["Project Vitruvius"]
                        :hand ["Restore"]}})
      (play-from-hand state :corp "Restore")
      (click-card state :corp (find-card "Project Vitruvius" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (not(:seen (get-content state :remote1 0))) "Agenda is facedown")
      (is (last-log-contains? state "Corp uses Restore to install Project Vitruvius from Archives.") "Should write correct log")))

(deftest restore-show-removed-count-in-log-when-installed
    ;; Show removed count in log when installed
    (do-game
      (new-game {:corp {:discard [(qty "Marilyn Campaign" 3)]
                        :hand ["Restore"]}})
      (play-from-hand state :corp "Restore")
      (click-card state :corp (find-card "Marilyn Campaign" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (last-log-contains? state "Corp removes 2 copies of Marilyn Campaign from the game.") "Should write correct log")))

(deftest restore-card-is-installed-and-rezzed
    ;; Card is installed and rezzed
    (do-game
      (new-game {:corp {:discard ["Marilyn Campaign"]
                        :hand ["Restore"]}})
      (play-from-hand state :corp "Restore")
      (click-card state :corp (find-card "Marilyn Campaign" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (= "Marilyn Campaign" (:title (get-content state :remote1 0))) "Marilyn Campaign should be installed")
      (is (rezzed? (get-content state :remote1 0)) "Marilyn Campaign was rezzed")
      (is (= 2 (:credit (get-corp))) "Rezzed Marilyn Campaign 2 credit + 1 credit for Restore")))

(deftest retribution
  ;; Retribution
  (do-game
      (new-game {:corp {:hand [(qty "Retribution" 2)]}
                 :runner {:hand ["Corroder" "Zer0" "Paparazzi"]
                          :tags 1}})
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Zer0")
      (take-credits state :runner)
      (play-from-hand state :corp "Retribution")
      (click-card state :corp "Corroder")
      (is (find-card "Corroder" (:discard (get-runner))))
      (play-from-hand state :corp "Retribution")
      (click-card state :corp "Zer0")
      (is (find-card "Zer0" (:discard (get-runner))))))

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

(deftest reverse-infection-gain-2-credits
    ;; Gain 2 credits
    (do-game
      (new-game {:corp {:deck ["Reverse Infection"]}})
      (play-from-hand state :corp "Reverse Infection")
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 7 (:credit (get-corp))) "Corp gained 2 credits")))

(deftest reverse-infection-purge-virus-counters-counts-both-runner-and-corp-side-sandstone
    ;; Purge virus counters, counts both Runner and Corp side (Sandstone)
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

(deftest reverse-infection-correct-log-issue-4861
    ;; correct log issue #4861
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
      (is (last-log-contains? state "Corp uses Reverse Infection to purge 9 virus counters and trash 3 cards from the top of the stack.") "Should write correct log")))

(deftest riot-suppression-take-1-brain-damage
    ;; Take 1 brain damage
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no core damage")
      (click-prompt state :runner "Suffer 1 core damage")
      (is (= 1 (count (:discard (get-runner)))) "1 card lost to core damage")
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppestion removed from game")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has all clicks the following turn")))

(deftest riot-suppression-damage-cannot-be-prevented
    ;; Take 1 brain damage
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Adonis Campaign"]}
                 :runner {:hand ["Caldera"]
                          :credits 20}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Caldera")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no core damage")
      (click-prompt state :runner "Suffer 1 core damage")
      (is (no-prompt? state :runner) "Cannot use Caldera to prevent core damage")))

(deftest riot-suppression-lose-3-clicks
    ;; Lose 3 clicks
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no core damage")
      (click-prompt state :runner "Get 3 fewer [Click] on the next turn")
      (is (empty? (:discard (get-runner))) "Runner discard statys empty")
      (is (zero? (:brain-damage (get-runner))) "Runner takes no core damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppression removed from game")
      (take-credits state :corp)
      (is (= 1 (:click (get-runner))) "Runner has 3 fewer clicks following turn")))

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

(deftest rover-algorithm
  ;; Rover Algorithm
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Rover Algorithm" "Ice Wall"]
                        :credits 10}
                 :runner {:hand ["Corroder"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (fire-subs state (get-ice state :hq 0))
      (is (not (:run @state)) "Run has been ended")
      (play-from-hand state :runner "Corroder")
      (take-credits state :runner)
      (play-from-hand state :corp "Rover Algorithm")
      (click-card state :corp "Ice Wall")
      (take-credits state :corp)
      (let [icew (get-ice state :hq 0)
            corr (get-program state 0)]
        (is (= 1 (core/get-strength (refresh icew))) "Ice Wall starts at 1 str")
        (run-on state :hq)
        (run-continue state)
        (auto-pump-and-break state (refresh corr))
        (core/continue state :corp nil)
        (run-jack-out state)
        (is (= 2 (core/get-strength (refresh icew))) "Ice Wall gained 1 str from Rover Algorithm")
        (run-on state :hq)
        (run-continue state)
        (fire-subs state (get-ice state :hq 0))
        (is (= 2 (core/get-strength (refresh icew))) "Rover Algorithm only triggers on passing ice"))))

(deftest rover-algorithm-works-after-host-ice-is-moved-3808
    ;; Works after host ice is moved #3808
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Vanilla" "Rover Algorithm" "Sunset"]
                        :credits 10}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Vanilla" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Rover Algorithm")
      (click-card state :corp "Ice Wall")
      (play-from-hand state :corp "Sunset")
      (click-prompt state :corp "HQ")
      (click-card state :corp "Ice Wall")
      (click-card state :corp "Vanilla")
      (click-prompt state :corp "Done")
      (is (= ["Vanilla" "Ice Wall"] (map :title (get-ice state :hq))))
      (take-credits state :corp)
      (run-on state "HQ")
      (is (changed? [(get-strength (get-ice state :hq 1)) 1]
            (run-continue state)
            (run-continue state))
          "gains 1 str")))

(deftest sacrifice
  ;; Sacrifice - Remove BP for each agenda point sacrificed and gain a credit
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

(deftest sacrifice-play-restrictions
    ;; Play restrictions
    (do-game
      (new-game {:corp {:deck ["Standoff" "Hostile Takeover" "Sacrifice"]}})
      (play-and-score state "Standoff")
      (click-prompt state :runner "Done")
      (core/gain state :corp :bad-publicity 1)
      (play-from-hand state :corp "Sacrifice")
      (is (= 2 (count (:hand (get-corp)))) "Can not play Sacrifice with no 1+ agenda in score area")
      (play-and-score state "Hostile Takeover")
      ;; Remove BP
      (core/gain state :corp :bad-publicity -2)
      (play-from-hand state :corp "Sacrifice")
      (is (= 1 (count (:hand (get-corp)))) "Can not play Sacrifice with no bad publicity in score area")))

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

(deftest scapenet
  (doseq [[title func] [["Misdirection" get-program]
                          ["Clone Chip" get-hardware]
                          ["The Turning Wheel" get-resource]]]
      (do-game
        (new-game {:corp {:deck ["Scapenet"]}
                   :runner {:deck [title]}})
        (play-from-hand state :corp "Scapenet")
        (is (no-prompt? state :corp) "Couldn't play Scapenet without a successful run.")
        (take-credits state :corp)
        (play-from-hand state :runner title)
        (run-empty-server state :archives)
        (take-credits state :runner)
        (play-from-hand state :corp "Scapenet")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (let [c (func state 0)]
          (click-card state :corp c))
        (if (= "Misdirection" title)
          (is (not (no-prompt? state :corp)) "Scapenet doesn't work on non-virtual non-chip card.")
          (do (is (= 1 (count (:rfg (get-runner)))) "Card removed from game.")
              (is (find-card "Scapenet" (:discard (get-corp)))))))))

(deftest scarcity-of-resources
  ;; Scarcity of Resources
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Scarcity of Resources"]}
               :runner {:hand ["Kati Jones"]}})
    (play-from-hand state :corp "Scarcity of Resources")
    (take-credits state :corp)
    (let [credits (:credit (get-runner))
          cost (:cost (find-card "Kati Jones" (:hand (get-runner))))]
      (play-from-hand state :runner "Kati Jones")
      (is (= (- credits (+ cost 2)) (:credit (get-runner))) "Runner should pay 2 extra for Kati Jones"))))

(deftest scorched-earth
  ;; Scorched Earth
  (do-game
      (new-game {:corp {:deck ["Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))

(deftest scorched-earth-not-tagged
    ;; not tagged
    (do-game
      (new-game {:corp {:deck ["Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (play-from-hand state :corp "Scorched Earth")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))

(deftest scorched-earth-flatline
    ;; flatline
    (do-game
      (new-game {:corp {:deck [(qty "Scorched Earth" 10)]}})
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

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

(deftest seamless-launch
  ;; Seamless Launch
  (do-game
     (new-game {:corp {:hand ["Seamless Launch" "Project Atlas"]}})
     (play-from-hand state :corp "Project Atlas" "New remote")
     (play-from-hand state :corp "Seamless Launch")
     (is (no-prompt? state :corp) "No valid target for Seamless Launch")
     (take-credits state :corp)
     (take-credits state :runner)
     (play-from-hand state :corp "Seamless Launch")
     (click-card state :corp (get-content state :remote1 0))
     (is (= 2 (get-counters (get-content state :remote1 0) :advancement)) "2 counters on Project Atlas")))

(deftest secure-and-protect-with-ice-in-deck
    ;; With ice in deck
    (do-game
      (new-game {:corp {:hand ["Secure and Protect"]
                        :deck [(qty "Hedge Fund" 10) "Ice Wall" "Afshar"]}})
      (play-from-hand state :corp "Secure and Protect")
      (is (= ["Afshar" "Ice Wall"] (vec (sort (prompt-titles :corp)))))
      (click-prompt state :corp "Ice Wall")
      (is (nil? (get-ice state :hq 0)) "No ice installed on HQ")
      (click-prompt state :corp "HQ")
      (is (get-ice state :hq 0) "Ice Wall is installed on HQ")))

(deftest secure-and-protect-with-no-ice-in-deck
    ;; With no ice in deck
    (do-game
      (new-game {:corp {:hand ["Secure and Protect"]
                        :deck [(qty "Hedge Fund" 10)]}})
      (play-from-hand state :corp "Secure and Protect")
      (is (no-prompt? state :corp) "Corp should have no prompts")))

(deftest secure-and-protect-with-varying-install-costs
    ;; With varying install costs
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
      (doall (map sp-test (range 4)))))

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
    (gain-tags state :runner 1)
    (is (zero? (count (:hand (get-runner)))) "Runner hand is empty")
    (let [inti (get-program state 0)
          cc (get-hardware state 0)]
      (play-from-hand state :corp "Self-Growth Program")
      (click-card state :corp inti)
      (click-card state :corp cc))
    (is (= 2 (count (:hand (get-runner)))) "2 cards returned to hand")
    (is (zero? (count (get-program state))) "No programs installed")
    (is (zero? (count (get-hardware state))) "No hardware installed")))

(deftest service-outage-first-click-run-each-turn-costs-a-credit
    ;; First click run each turn costs a credit
    (do-game
     (new-game {:corp {:deck ["Service Outage"]}
                :runner {:deck ["Employee Strike"]}})
     (play-from-hand state :corp "Service Outage")
     (take-credits state :corp)
     (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
     (run-empty-server state :archives)
     (is (= 4 (:credit (get-runner)))
         "Runner spends 1 credit to make the first run")
     (run-empty-server state :archives)
     (is (= 4 (:credit (get-runner)))
         "Runner doesn't spend 1 credit to make the second run")
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

(deftest service-outage-first-card-ability-run-each-turn-costs-an-additional-credit
    ;; First card ability run each turn costs an additional credit
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
       (run-continue state)
       (run-on state :archives)
       (is (= 1 (:credit (get-runner)))
           "Runner doesn't spend 1 credit to make a run"))))

(deftest service-outage-first-run-event-each-turn-costs-an-additional-credit
    ;; First run event each turn costs an additional credit
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
     (run-continue state)
     (run-empty-server state :archives)
     (is (= 3 (:credit (get-runner)))
         "Runner doesn't spend 1 credit to make a run")
     (take-credits state :runner)
     (take-credits state :corp)
     (click-prompt state :runner "No") ; Out of the Ashes prompt
     (core/lose state :runner :credit 4)
     (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
     (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
     (play-from-hand state :runner "Out of the Ashes")
     (is (not (:run @state)) "No run was initiated")))

(deftest service-outage-works-when-played-on-the-runner-s-turn
    ;; Works when played on the runner's turn
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

(deftest service-outage-doesn-t-fire-if-already-run-when-played-on-the-runner-s-turn
    ;; Doesn't fire if already run when played on the runner's turn
    (do-game
     (new-game {:corp {:id "New Angeles Sol: Your News"
                       :deck ["Service Outage"
                              "Breaking News"]}
                :runner {:deck ["Hades Shard"]}})
     (trash-from-hand state :corp "Breaking News")
     (take-credits state :corp)
     (run-empty-server state :hq)
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
         "Runner doesn't spend 1 additional credit to make a run")
     (is (:run @state) "Run was initiated")))

(deftest service-outage-trashed-and-reinstalled-on-steal-doesn-t-double-remove-penalty
    ;; trashed and reinstalled on steal doesn't double remove penalty
    (do-game
     (new-game {:corp {:id "New Angeles Sol: Your News"
                       :deck ["Service Outage"
                              "Breaking News"]}})
     (play-from-hand state :corp "Breaking News" "New remote")
     (play-from-hand state :corp "Service Outage")
     (take-credits state :corp)
     (run-empty-server state :remote1)
     (click-prompt state :runner "Steal")
     (click-prompt state :corp "Yes")
     (click-card state :corp (find-card "Service Outage" (:discard (get-corp))))
     (take-credits state :runner)
     (take-credits state :corp)
     (is (= 7 (:credit (get-runner))) "Runner has 7 credits")
     (run-on state :archives)
     (is (= 6 (:credit (get-runner))) "Runner spends 1 credit to make a run")))

(deftest service-outage-interaction-with-temp-credits
    ;; Interaction with temp credits
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
      (is (= 4 (:credit (get-runner))) "Second run fine")))

(deftest shipment-from-kaguya
  ;; NGO Front
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Shipment from Kaguya" "Ice Wall" "NGO Front"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "Shipment from Kaguya")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "NGO Front")
    (is (= 1 (get-counters (get-ice state :hq 0) :advancement)) "Ice Wall should be advanced")
    (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "NGO should be advanced")))

(deftest shipment-from-mirrormorph
  ;; Prisec
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Shipment from MirrorMorph" "Ice Wall" "PAD Campaign" "Prisec"]}})
    (play-from-hand state :corp "Shipment from MirrorMorph")
    (click-card state :corp "Ice Wall")
    (click-prompt state :corp "New remote")
    (click-card state :corp "PAD Campaign")
    (click-prompt state :corp "Server 1")
    (click-card state :corp "Prisec")
    (click-prompt state :corp "Server 1")
    (is (= "Ice Wall" (:title (get-ice state :remote1 0))) "Ice Wall should be installed")
    (is (= "PAD Campaign" (:title (get-content state :remote1 0))) "PAD Campaign should be installed")
    (is (= "Prisec" (:title (get-content state :remote1 1))) "Prisec should be installed")))

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

(deftest shipment-from-tennin
  ;; Ice Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Shipment from Tennin" "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Shipment from Tennin")
    (click-card state :corp "Ice Wall")
    (is (= 2 (get-counters (get-ice state :hq 0) :advancement)) "Ice Wall should be advanced")))

(deftest shipment-from-vladisibirsk
  ;; Shipment from Vladisibirsk
  (do-game
    (new-game {:corp {:hand [(qty "Shipment from Vladisibirsk" 2) "Ice Wall" "Hostile Takeover" "PAD Campaign" "Bio Vault"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Bio Vault" "New remote")
    (play-from-hand state :corp "Shipment from Vladisibirsk")
    (is (no-prompt? state :corp) "Shouldn't be able to play without runner being tagged")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Shipment from Vladisibirsk")
    (is (no-prompt? state :corp) "Shouldn't be able to play without runner having 2 tags")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Shipment from Vladisibirsk")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Hostile Takeover")
    (click-card state :corp "PAD Campaign")
    (click-card state :corp "PAD Campaign")
    (is (= 1 (get-counters (get-ice state :hq 0) :advancement)))
    (is (= 1 (get-counters (get-content state :remote1 0) :advancement)))
    (is (= 2 (get-counters (get-content state :remote2 0) :advancement)))
    (play-from-hand state :corp "Shipment from Vladisibirsk")
    (dotimes [_ 4]
      (click-card state :corp "Bio Vault"))
    (is (= 4 (get-counters (get-content state :remote3 0) :advancement)))))

(deftest shoot-the-moon
  ;; Ice Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Shoot the Moon" "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Shoot the Moon")
    (is (no-prompt? state :corp) "Shouldn't be able to play without runner being tagged")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Shoot the Moon")
    (let [credits (:credit (get-corp))]
      (click-card state :corp "Ice Wall")
      (is (= credits (:credit (get-corp))) "Corp shouldn't pay anything to rez Ice Wall"))))

(deftest simulation-reset
  ;; Simulation Reset
  (do-game
    (new-game {:corp {:hand ["Simulation Reset" "Ganked!" "Mitosis" "Patch" "Restore" "IPO"]
                      :deck ["Hedge Fund"]
                      :discard ["Fire Wall" "Ice Wall"]}})
    (play-from-hand state :corp "Simulation Reset")
    (click-card state :corp "Ganked!")
    (click-card state :corp "Mitosis")
    (click-card state :corp "Patch")
    (click-card state :corp "Restore")
    (click-prompt state :corp "Done")
    (is (not (= "Choose up to 5 cards in HQ to trash"
                (:msg (prompt-map :runner)))) "Now choosing cards to shuffle back to R&D")
    (click-card state :corp "Fire Wall")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Mitosis")
    (click-card state :corp "Restore")
    (is (no-prompt? state :corp))
    (is (= 1 (count (:rfg (get-corp)))) "Simulation Reset removed from game")
    (is (find-card "Ganked!" (:discard (get-corp))))
    (is (find-card "Patch" (:discard (get-corp))))
    (is (not (find-card "Ice Wall" (:discard (get-corp)))) "Ice Wall has been shuffled into R&D")
    (is (not (find-card "Restore" (:discard (get-corp)))) "Restore has been shuffled into R&D")
    (is (= 5 (count (:hand (get-corp)))))))

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
    (is (= 1 (-> (get-runner) :discard count)) "Scrubber should be in heap after losing Snatch and Grab trace")))

(deftest special-report
  ;; NGO Front
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Special Report" "Ice Wall" "IPO" "NGO Front"]}})
    (play-from-hand state :corp "Special Report")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "IPO")
    (click-card state :corp "NGO Front")
    (is (= 3 (count (:hand (get-corp)))) "corp should draw 3 cards")))

(deftest sprint
  ;; Sprint
  (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Restructure" "NGO Front"]
                        :hand ["Sprint" (qty "IPO" 3) "Ice Wall"]}})
      (play-from-hand state :corp "Sprint")
      (is (zero? (count (:deck (get-corp)))) "Corp should draw 3 cards")
      (is (= 7 (count (:hand (get-corp)))) "Corp should draw 3 cards")
      (is (last-log-contains? state "Corp uses Sprint to draw 3 cards"))
      (click-card state :corp "Ice Wall")
      (click-card state :corp "NGO Front")
      (is (= 2 (count (:deck (get-corp)))) "2 cards shuffled into deck")
      (is (= 5 (count (:hand (get-corp)))) "2 cards shuffled into deck")))

(deftest standard-procedure
  ;; Standard Procedure
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Standard Procedure"]}
               :runner {:hand ["Sure Gamble" "Account Siphon" "Kati Jones" "Corroder"]}})
    (play-from-hand state :corp "Standard Procedure")
    (is (find-card "Standard Procedure" (:hand (get-corp))) "Corp shouldn't play anything as runner can't run")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "Standard Procedure")
    (let [credits (:credit (get-corp))]
      (click-prompt state :corp "Event")
      (is (= (+ credits 4) (:credit (get-corp))) "Corp should gain 4 from 2 events in the grip"))))

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
  ;; Sub Boost - Give ice Barrier
  (do-game
    (new-game {:corp {:deck ["Sub Boost" "Quandary"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (let [qu (get-ice state :hq 0)]
      (rez state :corp qu)
      (is (not (has-subtype? (refresh qu) "Barrier")) "Quandry starts without Barrier")
      (is (= 1 (count (:subroutines (refresh qu)))) "Quandry has 1 subroutine")
      (play-from-hand state :corp "Sub Boost")
      (click-card state :corp (refresh qu))
      (is (has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (has-subtype? (refresh qu) "Barrier") "Quandary ice Barrier")
      (is (= 2 (count (:subroutines (refresh qu)))) "Quandry gains a subroutine"))))

(deftest subcontract-don-t-allow-second-operation-until-damage-prevention-completes
    ;; Don't allow second operation until damage prevention completes
    (do-game
      (new-game {:corp {:deck [(qty "Scorched Earth" 2) "Subcontract"]}
                 :runner {:deck ["Plascrete Carapace"]}})
      (take-credits state :corp)
      (gain-tags state :runner 1)
      (play-from-hand state :runner "Plascrete Carapace")
      (take-credits state :runner)
      (play-from-hand state :corp "Subcontract")
      (click-card state :corp (find-card "Scorched Earth" (:hand (get-corp))))
      (is (prompt-is-type? state :corp :waiting)
          "Corp does not have Subcontract prompt until damage prevention completes")
      (click-prompt state :runner "Done")
      (is (not (no-prompt? state :corp)) "Corp can now play second Subcontract operation")))

(deftest subcontract-interaction-with-terminal-operations
    ;; interaction with Terminal operations
    (do-game
      (new-game {:corp {:deck [(qty "Hard-Hitting News" 2) "Subcontract"]}})
      (gain-tags state :runner 1)
      (take-credits state :corp)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (play-from-hand state :corp "Subcontract")
      (click-card state :corp (find-card "Hard-Hitting News" (:hand (get-corp))))
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 5 (count-tags state)) "Runner has 5 tags")
      (is (no-prompt? state :corp) "Corp does not have a second Subcontract selection prompt")))

(deftest subliminal-messaging
  ;; Subliminal Messaging - Playing/trashing/milling will all prompt returning to hand
  (do-game
      (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                        :deck [(qty "Subliminal Messaging" 3)]}
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
      (is (= 10 (:credit (get-corp))) "Only first subliminal should give Hyoubu reveal credit")
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
      (is (no-prompt? state :corp) "No prompt here because runner made a run last turn")
      (take-credits state :corp)
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 1 (count (:discard (get-corp)))) "1 Subliminal not returned because runner made a run last turn")))

(deftest subliminal-messaging-scenario-involving-subliminal-being-added-to-hq-with-archived-memories
    ;; Scenario involving Subliminal being added to HQ with Archived Memories
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
      (is (no-prompt? state :corp) "Only 1 Subliminal prompt")
      (play-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (is (no-prompt? state :corp)
          "Only 2 Subliminal prompts - there will be a third if flag not cleared")))

(deftest subliminal-messaging-scenario-involving-subliminal-being-reshuffled-into-r-d-with-jackson
    ;; Scenario involving Subliminal being reshuffled into R&D with Jackson
    (do-game
      (new-game {:corp {:deck ["Subliminal Messaging" "Jackson Howard"]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp)
      (let [jhow (get-content state :remote1 0)]
        (rez state :corp jhow)
        (card-ability state :corp jhow 1)
        (click-card state :corp "Subliminal Messaging")
        (is (zero? (count (:discard (get-corp)))))
        (is (= 1 (count (:rfg (get-corp))))))
      (take-credits state :runner)
      (play-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (is (= 1 (count (:hand (get-corp)))) "Subliminal returned to HQ")
      (is (no-prompt? state :corp)
          "Subliminal prompt cleared - there will be a second prompt if flag not cleared")))

(deftest subliminal-messaging-runner-made-run-ensure-game-asks-again-next-turn
    ;; Runner made run, ensure game asks again next turn
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 2)]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (trash-from-hand state :corp "Subliminal Messaging")
      (take-credits state :corp)
      (run-on state "R&D")
      (run-jack-out state)
      (take-credits state :runner)
      (is (no-prompt? state :corp) "No prompt here because runner made a run last turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (is (= 2 (count (:hand (get-corp)))) "Both Subliminals returned to HQ")
      (is (zero? (count (:discard (get-corp)))) "No Subliminals in Archives")))

(deftest subliminal-messaging-user-declines-to-return-to-hand-ensure-game-asks-again-next-turn
    ;; User declines to return to hand, ensure game asks again next turn
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
      (is (zero? (count (:discard (get-corp)))) "No Subliminals in Archives")))

(deftest success-works-with-bad-publicity
    ;; Works with bad publicity
    (do-game
      (new-game {:corp {:deck ["NAPD Contract" "Project Beale" "Success"]}})
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (play-from-hand state :corp "Project Beale" "New remote")
      (core/gain state :corp :bad-publicity 9)
      (core/gain state :corp :credit 8)
      (core/gain state :corp :click 15)
      (let [napd (get-content state :remote1 0)
            beale (get-content state :remote2 0)]
        (dotimes [_ 13] (click-advance state :corp (refresh napd)))
        (is (= 13 (get-counters (refresh napd) :advancement)))
        (score state :corp (refresh napd))
        (is (= 2 (:agenda-point (get-corp))))
        (play-from-hand state :corp "Success")
        (click-card state :corp (get-scored state :corp 0))
        (is (= "NAPD Contract" (:title (first (:rfg (get-corp))))))
        (click-card state :corp (refresh beale))
        (is (= 13 (get-counters (refresh beale) :advancement)))
        (score state :corp (refresh beale))
        (is (= 7 (:agenda-point (get-corp)))))))

(deftest success-works-with-public-agendas
    ;; Works with public agendas
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
        (score state :corp (refresh oaktown))
        (is (= 2 (:agenda-point (get-corp)))))))

(deftest success-interaction-with-jemison-regression-test-for-issue-2704
    ;; interaction with Jemison, regression test for issue #2704
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
        (is (= (+ 4 5) (get-counters (refresh gto) :advancement)) "Advance 5 times from Success"))))

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

(deftest sudden-commandment
  (do-game
    (new-game {:corp {:hand ["Sudden Commandment"]
                      :deck ["IPO" "Hedge Fund"]
                      :credits 10}})
    (is (changed? [(count (:hand (get-corp))) 1]
                  (play-from-hand state :corp "Sudden Commandment"))
        "Corp drew 2 cards (-1 being Sudden Commandment)")
    (is (= 2 (count (:choices (prompt-map :corp)))) "Choices should be Hedge Fund and Done")
    (is (changed? [(:credit (get-corp)) 4]
                  (click-prompt state :corp "Hedge Fund"))
        "Hedge Fund was played")))

(deftest ^:kaocha/pending sudden-commandment-threat
  (do-game
    (new-game {:corp {:hand [(qty "Sudden Commandment" 2) "Bellona"]
                      :deck ["IPO" "Hedge Fund"]
                      :credits 10}})
    (play-and-score state "Bellona")
    (play-from-hand state :corp "Sudden Commandment")
    (click-prompt state :corp "Hedge Fund")
    (is (changed? [(:credit (get-corp)) -3
                   (:click (get-corp)) 1]
                  (click-prompt state :corp "Yes"))
        "Corp spent 3 credits and gained 1 click")
    (play-from-hand state :corp "Sudden Commandment")
    (click-prompt state :corp "Done")
    (is (no-prompt? state :corp) "No additional prompt when playing the second Suddend Commandment of the turn")))

(deftest sunset
  ;; Sunset
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Sunset" "Ice Wall" "Enigma" "Hunter"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "HQ")
    (play-from-hand state :corp "Hunter" "HQ")
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Sunset")
    (click-prompt state :corp "HQ")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Hunter")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Enigma")
    (click-prompt state :corp "Done")
    (is (= "Hunter" (:title (get-ice state :hq 0))) "Hunter should be in position 1")
    (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall should be in position 2")
    (is (= "Enigma" (:title (get-ice state :hq 2))) "Hunter should be in position 3")))

(deftest surveillance-sweep
  ;; Surveillance Sweep
  (do-game
      (new-game {:corp {:deck ["Restructured Datapool" "Surveillance Sweep" "Data Raven"]}
                 :runner {:deck ["Scrubbed"]}})
      (is (zero? (count-tags state)) "Runner should start with no tags")
      (play-from-hand state :corp "Surveillance Sweep")
      (play-and-score state "Restructured Datapool")
      (let [rd-scored (get-scored state :corp 0)]
        (card-ability state :corp rd-scored 0)
        (is (not= :waiting (prompt-type :corp)) "Surveillance Sweep only works during a run")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (count-tags state)) "Runner should gain a tag from Restructured Datapool ability"))
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Data Raven" "HQ")
      (take-credits state :corp)
      (let [dr (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh dr))
        (run-continue state)
        (click-prompt state :runner "Take 1 tag")
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :corp :waiting) "During a run, Corp should wait on Runner first")
        (click-prompt state :runner "0")
        (click-prompt state :corp "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-continue state)
        (run-continue state)
        (play-from-hand state :runner "Scrubbed")
        (run-on state :hq)
        (run-continue state)
        (click-prompt state :runner "Take 1 tag")
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :runner :waiting) "Runner should now be waiting on Corp")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace"))))

(deftest surveillance-sweep-trace-during-run-after-stealing-an-agenda
    ;; trace during run after stealing an agenda
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
            bn (get-content state :remote1 0)]
        (run-on state :remote1)
        (rez state :corp (refresh dr))
        (run-continue state)
        (click-prompt state :runner "Take 1 tag")
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :corp :waiting) "During a run, Corp should wait on Runner first")
        (click-prompt state :runner "0")
        (click-prompt state :corp "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-continue state)
        (run-continue state)
        (click-card state :runner bn)
        (click-prompt state :runner "Steal")
        (is (prompt-is-type? state :runner :waiting) "After steal, Surveillance Sweep leaves play and Runner waits on Corp"))))

(deftest surveillance-sweep-interaction-with-citadel-sanctuary-and-sol
    ;; Interaction with Citadel Sanctuary and Sol
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
      (end-turn state :runner)
      (is (prompt-is-type? state :runner :waiting) "Runner is waiting on Corp")))

(deftest sweeps-week
  ;; Sweeps Week
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Sweeps Week" 2)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Sweeps Week")
    (is (= 5 (:credit (get-corp))) "Corp should spend 1 and gain 1 for 1 card in grip")
    (dotimes [_ 5]
      (core/move state :runner (find-card "Sure Gamble" (:deck (get-runner))) :hand))
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Sweeps Week")
      (is (= (+ credits -1 (count (:hand (get-runner)))) (:credit (get-corp)))
          "Corp should gain 5 for 6 cards in the grip"))))

(deftest sync-rerouting
  ;; SYNC Rerouting
  (do-game
      (new-game {:corp {:hand ["SYNC Rerouting"]}})
      (play-from-hand state :corp "SYNC Rerouting")
      (take-credits state :corp)
      (run-on state :hq)
      (is (changed? [(count-tags state) 1]
            (click-prompt state :runner "Take 1 tag"))
          "Runner took 1 tag")
      (run-jack-out state)
      (run-on state :rd)
      (is (changed? [(:credit (get-runner)) -4]
            (click-prompt state :runner "Pay 4 [Credits]"))
          "Runner paid 4 credits")))

(deftest sync-rerouting-jesminder-avoids-sync-rerouting-tag
    ;; Jesminder avoids SYNC Rerouting tag
    (do-game
      (new-game {:corp {:hand ["SYNC Rerouting"]}
                 :runner {:id "Jesminder Sareen: Girl Behind the Curtain"}})
      (play-from-hand state :corp "SYNC Rerouting")
      (take-credits state :corp)
      (run-on state :hq)
      (is (changed? [(count-tags state) 0]
            (click-prompt state :runner "Take 1 tag"))
          "Jesminder avoided tag")))

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

(deftest the-all-seeing-i-counts-number-of-cards-if-one-card-is-prevented-trashed-with-fall-guy
    ;; Counts number of cards if one card is prevented trashed with fall guy
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
        (gain-tags state :runner 1)
        (play-from-hand state :corp "The All-Seeing I")
        (let [fall-guy (get-resource state 1)]
          (card-ability state :runner fall-guy 0))
        (click-prompt state :runner "Done")
        (is (= 1 (res)) "One installed resource saved by Fall Guy")
        (is (= 2 (count (:discard (get-runner)))) "Two cards in heap"))))

(deftest the-all-seeing-i-checks-that-all-seeing-i-does-not-double-trash-hosted-cards-trashes-hosted-cards
    ;; Checks that All-seeing I does not double-trash hosted cards, trashes hosted cards
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
      (gain-tags state :runner 1)
      (take-credits state :runner)
      (play-from-hand state :corp "The All-Seeing I")
      (is (= "Prevent the trashing of Off-Campus Apartment?"
             (:msg (prompt-map :runner))))
      (let [fall-guy (find-card "Fall Guy" (core/all-active-installed state :runner))]
        (card-ability state :runner fall-guy 0))
      (click-prompt state :runner "Done")
      (is (= "Prevent the trashing of Fall Guy?"
             (:msg (prompt-map :runner))))
      (click-prompt state :runner "Done")
      (is (= 1 (count (core/all-active-installed state :runner))) "One installed card (Off-Campus)")
      (is  (= 2 (count (:discard (get-runner)))) "Two cards in heap")))

(deftest the-all-seeing-i-should-not-trash-jarogniew-mercs-if-there-are-other-installed-resources
    ;; should not trash Jarogniew Mercs if there are other installed resources
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
        (is (zero? (res)) "There are no installed resources"))))

(deftest threat-assessment
  ;; Threat Assessment - play only if runner trashed a card last turn, move a card to the stack or take 2 tags
  (do-game
      (new-game {:corp {:deck [(qty "Threat Assessment" 3) "Adonis Campaign"]}
                 :runner {:deck ["Desperado" "Corroder"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
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
      (click-prompt state :runner "Add Corroder to the top of the Stack")
      (is (= 2 (count-tags state)) "Runner didn't take tags")
      (is (= "Corroder" (:title (first (:deck (get-runner))))) "Moved Corroder to the deck")
      (is (= 2 (count (:rfg (get-corp)))))
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Threat Assessment")
      (is (no-prompt? state :corp) "Threat Assessment triggered with no trash")))

(deftest threat-assessment-interaction-with-hippo-issue-4049
    ;; interaction with Hippo. Issue #4049
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
        (rez state :corp (refresh iw))
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Yes")
        (is (empty? (get-ice state :hq)) "Ice Wall is gone"))
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (play-from-hand state :corp "Threat Assessment")
      (click-card state :corp "Corroder")
      (click-prompt state :runner "Add Corroder to the top of the Stack")
      (is (zero? (count-tags state)) "Runner didn't take tags")
      (is (no-prompt? state :corp))))

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
    (gain-tags state :runner 2)
    (play-from-hand state :corp "Threat Level Alpha")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 6 (count-tags state)) "Runner took 3 tag because they had 3")))

(deftest too-big-to-fail
  ;; Too Big to Fail
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Too Big to Fail" 2)]}})
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Too Big to Fail")
      (is (= 1 (count-bad-pub state)) "Corp should gain 1 bad pub")
      (is (= (+ credits 7) (:credit (get-corp))) "Corp should gain 7 credits"))
    (let [credits (:credit (get-corp))
          bp (count-bad-pub state)]
      (play-from-hand state :corp "Too Big to Fail")
      (is (= bp (count-bad-pub state)) "Corp shouldn't gain any more bad pub")
      (is (= credits (:credit (get-corp))) "Corp shouldn't gain any more as over 10 credits"))))

(deftest traffic-accident
  ;; Traffic Accident
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Traffic Accident"]}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Traffic Accident")
    (is (= 5 (count (:hand (get-runner)))) "Runner shouldn't take damage as they're not tagged")
    (gain-tags state :runner 2)
    (play-from-hand state :corp "Traffic Accident")
    (is (= 3 (count (:hand (get-runner)))) "Runner should take 2 damage")))

(deftest transparency-initiative
  ;; Transparency Initiative - Full test
  (do-game
    (new-game {:corp {:deck ["Transparency Initiative" "Oaktown Renovation"
                             "Project Atlas" "Hostile Takeover" "Casting Call"]}})
    (core/gain state :corp :click 10)
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
      (click-advance state :corp (refresh hostile))
      (is (= 5 (:credit (get-corp))))
      ;; make sure advancing other agendas doesn't gain 1
      (click-advance state :corp (refresh oaktown))
      (is (= 6 (:credit (get-corp))) "Transparency initiative didn't fire")
      (click-advance state :corp (refresh atlas))
      (is (= 5 (:credit (get-corp))) "Transparency initiative didn't fire"))))

(deftest trick-of-light
  ;; Trick of Light
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Trick of Light" "Ice Wall" "NGO Front"]
                        :credits 10}})
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "NGO Front" "New remote")
      (let [ngo (get-content state :remote1 0)
            iw (get-ice state :hq 0)]
        (advance state (refresh ngo) 2)
        (is (= 2 (get-counters (refresh ngo) :advancement)) "NGO Front should have 2 counters")
        (play-from-hand state :corp "Trick of Light")
        (click-card state :corp iw)
        (click-card state :corp ngo)
        (click-prompt state :corp "2")
        (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall is now advanced")
        (is (zero? (get-counters (refresh ngo) :advancement)) "NGO Front should have 0 counters"))))

(deftest trick-of-light-playing-2-trick-of-lights-issue-4565
    ;; Playing 2 Trick of Lights. Issue #4565
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Trick of Light" 2) "Ice Wall" "NGO Front"]
                        :credits 10}})
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "NGO Front" "New remote")
      (let [ngo (get-content state :remote1 0)
            iw (get-ice state :hq 0)]
        (advance state (refresh ngo) 2)
        (is (= 2 (get-counters (refresh ngo) :advancement)) "NGO Front should have 2 counters")
        (play-from-hand state :corp "Trick of Light")
        (click-card state :corp (refresh iw))
        (click-card state :corp (refresh ngo))
        (click-prompt state :corp "2")
        (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall is now advanced")
        (is (zero? (get-counters (refresh ngo) :advancement)) "NGO Front should have 0 counters")
        (play-from-hand state :corp "Trick of Light")
        (click-card state :corp (refresh ngo))
        (click-card state :corp (refresh iw))
        (click-prompt state :corp "2")
        (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should have 0 counters")
        (is (= 2 (get-counters (refresh ngo) :advancement)) "NGO Front should have 2 counters"))))

(deftest trojan-horse
  ;; Trojan Horse
  (do-game
    (new-game {:corp {:deck ["Trojan Horse" "Dedicated Response Team"]}
               :runner {:deck ["Wyrm"]}})
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Wyrm")
    (run-empty-server state :remote1)
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
    (play-from-hand state :corp "Trojan Horse")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-program state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Wyrm should be in heap after Runner loses Trojan Horse trace")))

(deftest trust-operation-happy
  (do-game
    (new-game {:corp {:hand ["Trust Operation" "Ice Wall"]
                      :discard ["Archer"]}
               :runner {:hand ["Daily Casts"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    ;;can't play unless runner is tagged
    (play-from-hand state :corp "Trust Operation")
    (is (no-prompt? state :corp))
    (is (= 1 (count (:hand (get-corp)))) "Trust Operation not played")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Trust Operation")
    (click-card state :corp "Daily Casts")
    (is (= 1 (count (:discard (get-runner)))) "Daily casts was trashed")
    (is (changed? [(:credit (get-corp)) 0]
          (click-card state :corp "Archer")
          (click-prompt state :corp "HQ")
          (let [archer (get-ice state :hq 1)]
        (is (rezzed? (refresh archer)) "Archer is rezzed")))
        "Spends nothing to install and rez Archer")))

(deftest trust-operation-decline-to-trash
  ;; decline to trash a card
  (do-game
    (new-game {:corp {:hand ["Trust Operation"]
                      :discard ["Archer"]}
               :runner {:hand ["Daily Casts"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (take-credits state :runner)
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Trust Operation")
    (click-prompt state :corp "Done")
    (is (= 0 (count (:discard (get-runner)))) "Daily casts was not trashed")
    (click-card state :corp "Archer")
    (click-prompt state :corp "HQ")
    (let [archer (get-ice state :hq 0)]
      (is (rezzed? (refresh archer)) "Archer is rezzed"))))

(deftest trust-operation-no-card-to-trash
  (do-game
    (new-game {:corp {:hand ["Trust Operation"]
                      :discard ["Archer"]}})
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Trust Operation")
    (click-prompt state :corp "Done")
    (click-card state :corp "Archer")
    (click-prompt state :corp "HQ")
    (let [archer (get-ice state :hq 0)]
      (is (rezzed? (refresh archer)) "Archer is rezzed"))))

(deftest ultraviolet-clearance
  ;; Ultraviolet Clearance - Only allow agenda to be installed in remote servers
  (do-game
    (new-game {:corp {:deck [(qty "Ultraviolet Clearance" 2) "Improved Tracers" "Remote Enforcement"]}})
    (core/gain state :corp :click 3 :credit 7)
    (play-from-hand state :corp "Ultraviolet Clearance")
    (click-card state :corp (find-card "Improved Tracers" (:hand (get-corp))))
    (is (= 1 (count (prompt-buttons :corp))) "Wrong number of options in install prompt")
    (click-prompt state :corp "New remote")
    (play-from-hand state :corp "Ultraviolet Clearance")
    (click-card state :corp (find-card "Remote Enforcement" (:hand (get-corp))))
    (is (= 2 (count (prompt-buttons :corp))) "Wrong number of options in install prompt")))

(deftest under-the-bus
  ;; Under the Bus
  (do-game
    (new-game {:corp {:deck ["Under the Bus"]}
               :runner {:deck ["Film Critic"]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
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

(deftest under-the-bus-under-the-bus-can-be-played-with-no-connections
  ;; Under the Bus
  (do-game
    (new-game {:corp {:deck ["Under the Bus"]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (is (zero? (count-bad-pub state)) "Corp has no bad pub")
    (play-from-hand state :corp "Under the Bus")
    (click-prompt state :corp "Done")
    (is (= 1 (count-bad-pub state)) "Corp takes 1 bad pub")))

(deftest under-the-bus-with-replacement-effects
    ;; With replacement effects
    (do-game
    (new-game {:corp {:deck ["Ice Wall"]
                      :hand ["Under the Bus"]}
               :runner {:deck ["Film Critic" "Khusyuk"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Film Critic")
    (play-from-hand state :runner "Khusyuk")
    (run-continue state)
    (click-prompt state :runner "1 [Credit]: 1 card")
    (click-prompt state :runner "Ice Wall")
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (is (= 1 (count (get-resource state))) "Runner has 1 resource installed")
    (is (zero? (count-bad-pub state)) "Corp has no bad pub")
    (play-from-hand state :corp "Under the Bus")
    (click-card state :corp (get-resource state 0))
    (is (empty? (get-resource state)) "Runner has no resource installed")
    (is (= 2 (count (:discard (get-runner)))) "Runner has 2 trashed cards")
    (is (= 1 (count-bad-pub state)) "Corp takes 1 bad pub")))

(deftest violet-level-clearance
  ;; Violet Level Clearance
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Violet Level Clearance"]}})
    (play-from-hand state :corp "Violet Level Clearance")
    (is (= 8 (:credit (get-corp))) "Corp should gain 8 credits")
    (is (= 4 (count (:hand (get-corp)))) "Corp should draw 4 cards")
    (is (= 1 (count (:deck (get-corp)))) "Corp should draw 4 cards")))

(deftest voter-intimidation
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Voter Intimidation" "Hostile Takeover"]}
               :runner {:hand ["Kati Jones"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Voter Intimidation")
    (click-prompt state :corp "1 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (click-card state :corp "Kati Jones")
    (is (not (get-resource state 0)) "Kati Jones is trashed")))

(deftest wake-up-call-should-fire-after-using-en-passant-to-trash-ice
    ;; should fire after using En Passant to trash ice
    (do-game
      (new-game {:corp {:hand ["Enigma" "Wake Up Call"]}
                 :runner {:deck ["En Passant" "Maya"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Maya")
      (run-on state :hq)
      (run-continue state)
      (run-continue state)
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

(deftest wake-up-call-should-fire-after-embezzle-issue-4188
    ;; Should fire after Embezzle. Issue #4188
    (do-game
      (new-game {:corp {:deck ["Wake Up Call"]
                        :hand [(qty "Hedge Fund" 4)]
                        :credit 10}
                 :runner {:hand ["Embezzle" "Desperado"]
                          :credit 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Desperado")
      (play-from-hand state :runner "Embezzle")
      (run-continue state)
      (click-prompt state :runner "Operation")
      (take-credits state :runner)
      (play-from-hand state :corp "Wake Up Call")
      (click-card state :corp "Desperado")
      (click-prompt state :runner "Trash Desperado")
      (is (= 2 (count (:discard (get-runner)))))))

(deftest wake-up-call-should-fire-after-stargate-issue-4188
    ;; Should fire after Stargate. Issue #4188
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
      (run-continue state)
      (click-prompt state :runner "Hedge Fund")
      (take-credits state :runner)
      (play-from-hand state :corp "Wake Up Call")
      (click-card state :corp "Acacia")
      (click-prompt state :runner "Trash Acacia")
      (is (= 1 (count (:discard (get-runner)))))))

(deftest wetwork-refit
  ;; Wetwork Refit - Only works on Bioroid ice and adds a subroutine
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
      (is (not-any? #{"Eli 1.0"} (prompt-buttons :corp))
          "Unrezzed Eli 1.0 is not a choice to host Wetwork Refit")
      (click-prompt state :corp "Done")
      (take-credits state :corp)
      (take-credits state :runner)
      (rez state :corp (refresh eli))
      (rez state :corp (refresh vanilla))
      (play-from-hand state :corp "Wetwork Refit")
      (click-card state :corp (refresh eli))
      (is (= "Wetwork Refit" (get-title (first (:hosted (refresh eli)))))
          "Wetwork Refit is hosted on Eli 1.0")
      (is (= 3 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 2 different subroutines")
      (is (= "[Wetwork Refit] Do 1 core damage" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has a core damage subroutine as his first subroutine")
      (core/move state :corp (first (:hosted (refresh eli))) :hand)
      (is (empty? (:hosted (refresh eli))) "No cards are hosted on Eli 1.0")
      (is (= 2 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 1 different subroutine")
      (is (= "End the run" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has an end the run subroutine as his first subroutine")
      (play-from-hand state :corp "Wetwork Refit")
      (click-card state :corp (refresh vanilla))
      (is (not= "Wetwork Refit" (get-title (first (:hosted (refresh vanilla)))))
          "Wetwork Refit is not hosted on Vanilla"))))

(deftest witness-tampering
  ;; Witness Tampering
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Witness Tampering"]
                      :bad-pub 3}})
    (play-from-hand state :corp "Witness Tampering")
    (is (= 1 (count-bad-pub state)) "Corp should lose 2 bad pub")))

(deftest your-digital-life
  (do-game
    (new-game {:corp {:hand [(qty "Your Digital Life" 5)]}})
    (play-from-hand state :corp "Your Digital Life")
    (is (= 7 (:credit (get-corp))) "Corp gained 4 credits")))
