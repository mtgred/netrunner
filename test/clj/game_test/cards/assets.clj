(ns game-test.cards.assets
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards)

(deftest adonis-campaign
  ;; Adonis Campaign
  (do-game
    (new-game (default-corp [(qty "Adonis Campaign" 1)])
              (default-runner))
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (let [ac (get-content state :remote1 0)]
      (core/rez state :corp ac)
      (is (= 1 (:credit (get-corp))))
      (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))
            counters (get-counters (refresh ac) :credit)]
        (take-credits state :runner)
        (is (= (:credit (get-corp)) (+ credits 3)) "Gain 3 from Adonis")
        (is (= (get-counters (refresh ac) :credit) (- counters 3)) "9 counter remaining on Adonis")))))

(deftest advanced-assembly-lines
  ;; Advanced Assembly Lines
  (do-game
    (new-game (default-corp [(qty "Advanced Assembly Lines" 1)
                             (qty "PAD Campaign" 1)])
              (default-runner))
    (play-from-hand state :corp "Advanced Assembly Lines" "New remote")
    (let [aal (get-content state :remote1 0)
          credits (:credit (get-corp))
          hq (count (:hand (get-corp)))]
      (core/rez state :corp aal)
      (is (= (+ credits 2) (:credit (get-corp))) "Spend 1 gain 3")
      (card-ability state :corp aal 0)
      (prompt-select :corp (find-card "PAD Campaign" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (is (= (- hq 1) (count (:hand (get-corp)))) "Installed 1 card, hq is empty"))))

(deftest aggressive-secretary
  ;; Aggressive Secretary
  (do-game
    (new-game
      (default-corp [(qty "Aggressive Secretary" 1)])
      (default-runner [(qty "Cache" 3)]))
    (play-from-hand state :corp "Aggressive Secretary" "New remote")
    (let [as (get-content state :remote1 0)]
      ;; Single advance AggSec
      (core/advance state :corp {:card (refresh as)})
      (take-credits state :corp)
      ;; Run on AggSec with 3 programs
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (run-empty-server state "Server 1")
      (prompt-choice :corp "Yes")
      (is (= 3 (:credit (get-corp))))
      ;; Corp can trash one program
      (prompt-select :corp (get-in @state [:runner :rig :program 1]))
      ;; There should be two Caches left
      (is (= 3 (:credit (get-corp))))
      (is (= 2 (count (get-in @state [:runner :rig :program])))))))

(deftest alexa-belsky
  ;; Alexa Belsky
  (do-game
    (new-game
      (default-corp [(qty "Alexa Belsky" 1) (qty "Hedge Fund" 1) (qty "Breaking News" 1)
                     (qty "Gutenberg" 1) (qty "Product Placement" 1) (qty "Jackson Howard" 1)])
      (default-runner))
    (play-from-hand state :corp "Alexa Belsky" "New remote")
    (let [alexa (get-content state :remote1 0)]
      (core/rez state :corp alexa)
      (card-ability state :corp alexa 0)
      (is (= 1 (count (:discard (get-corp)))) "Alexa Belsky trashed")
      (is (= 5 (count (:hand (get-corp)))))
      (is (= 0 (count (:deck (get-corp)))))
      (prompt-choice :runner 5) ;Runner chooses to pay 5 credits so 2 cards are prevented from being shuffled
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 3 (count (:deck (get-corp)))))
      (is (= 0 (:credit (get-runner)))))))

(deftest alix-t4lb07
  ;; Alix T4LB07
  (do-game
    (new-game
      (default-corp [(qty "Alix T4LB07" 1) (qty "PAD Campaign" 3)])
      (default-runner))
    (play-from-hand state :corp "Alix T4LB07" "New remote")
    (let [alix (get-content state :remote1 0)]
      (core/rez state :corp alix)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (get-counters (refresh alix) :power)) "Two counters on Alix")
      (is (= 4 (:credit (get-corp))))
      (card-ability state :corp alix 0)
      (is (= 8 (:credit (get-corp))) "Gain 4 credits from Alix"))))

(deftest allele-repression
  ;; Allele Repression
  (do-game
    (new-game (default-corp [(qty "Allele Repression" 1)])
              (default-runner))
    (play-from-hand state :corp "Allele Repression" "New remote")
    (let [ar (get-content state :remote1 0)]
      (core/advance state :corp (refresh ar))
      (core/advance state :corp (refresh ar))
      (card-ability state :corp ar 0)
      (is (= 1 (count (:discard (get-corp)))) "Allele Repression is trashed"))))

(deftest amani-senai
  ;; Amani Senai - trace on score/steal to bounce, with base strength = advancement req of the agenda
  (do-game
    (new-game (default-corp [(qty "Amani Senai" 1)
                             (qty "Medical Breakthrough" 2)])
              (default-runner [(qty "Analog Dreamers" 1)]))
    (play-from-hand state :corp "Amani Senai" "New remote")
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (take-credits state :corp)
    (let [senai (get-content state :remote1 0)
          breakthrough (get-content state :remote3 0)]
      (core/rez state :corp senai)
      (play-from-hand state :runner "Analog Dreamers")
      (run-empty-server state "Server 2")
      (prompt-choice :runner "Steal")
      (is (= 0 (count (get-in @state [:corp :servers :remote2 :content]))) "Agenda was stolen")
      (prompt-choice :corp "Medical Breakthrough") ;simult. effect resolution
      (prompt-choice :corp "Yes")
      (prompt-choice :corp 0)  ;; Corp doesn't pump trace
      (is (= 3 (-> (get-runner) :prompt first :strength)) "Trace base strength is 3 after stealing first Breakthrough")
      (prompt-choice :runner 0)
      (let [n (count (get-in @state [:runner :hand]))]
        (is (= 1 (count (get-in @state [:runner :rig :program]))) "There is an Analog Dreamers installed")
        (prompt-select :corp (first (get-in @state [:runner :rig :program])))
        (is (= 0 (count (get-in @state [:runner :rig :program]))) "Analog Dreamers was uninstalled")
        (is (= (+ n 1) (count (get-in @state [:runner :hand]))) "Analog Dreamers was added to hand"))
      (take-credits state :runner)
      (score-agenda state :corp breakthrough)
      ;; (prompt-choice :corp "Medical Breakthrough") ; there is no simult. effect resolution on score for some reason
      (prompt-choice :corp "Yes")       ;corp should get to trigger trace even when no runner cards are installed
      (prompt-choice :corp 0)
      (is (= 2 (-> (get-runner) :prompt first :strength)) "Trace base strength is 2 after scoring second Breakthrough"))))

(deftest anson-rose
  ;; Anson Rose
  (do-game
    (new-game
      (default-corp [(qty "Anson Rose" 1) (qty "Ice Wall" 1)])
      (default-runner))
    (play-from-hand state :corp "Anson Rose" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ar (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp (refresh ar))
      (is (nil? (:advance-counter (refresh ar))) "Anson Rose should start with 0 advancement counters")
      (is (nil? (:advance-counter (refresh iw))) "Ice Wall should start with 0 advancement counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 1 (:advance-counter (refresh ar))) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (nil? (:advance-counter (refresh iw))) "Ice Wall should still have 0 counters so far")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 2 (:advance-counter (refresh ar))) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (nil? (:advance-counter (refresh iw))) "Ice Wall should still have 0 counters so far")
      (core/rez state :corp (refresh iw))
      (prompt-choice :corp "Yes")
      (prompt-choice :corp 2)
      (is (= 0 (:advance-counter (refresh ar))) "Anson Rose should lose all advancement counters")
      (is (= 2 (:advance-counter (refresh iw))) "Ice Wall should gain 2 advancement counter"))))

(deftest aryabhata-tech
  ;; Aryabhata Tech
  (do-game
    (new-game
      (default-corp [(qty "Aryabhata Tech" 1)
                     (qty "Hunter" 1)])
      (default-runner))
    (play-from-hand state :corp "Aryabhata Tech" "New remote")
    (play-from-hand state :corp "Hunter" "HQ")
    (let [at (get-content state :remote1 0)
          h (get-ice state :hq 0)]
      (core/rez state :corp (refresh at))
      (core/rez state :corp (refresh h))
      (take-credits state :corp)
      (run-on state :hq)
      (let [c-credits (:credit (get-corp))
            r-credits (:credit (get-runner))]
        (card-subroutine state :corp h 0)
        (prompt-choice :corp 0)
        (prompt-choice :runner 0)
        (is (= 1 (- (:credit (get-corp)) c-credits)))
        (is (= -1 (- (:credit (get-runner)) r-credits)))))))

(deftest bio-ethics-association
  ;; Bio-Ethics Association
  (testing "Basic test"
    (do-game
      (new-game
        (default-corp [(qty "Bio-Ethics Association" 1)])
        (default-runner))
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))))))
  (testing "should be able to prevent damage from multiple copies"
    (do-game
      (new-game
        (default-corp [(qty "Bio-Ethics Association" 2)])
        (default-runner [(qty "Feedback Filter" 1) (qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (take-credits state :runner)
      (let [filter (get-hardware state 0)]
        (is (= 1 (count (:prompt (get-runner)))) "Runner has a single damage prevention prompt")
        (card-ability state :runner filter 0)
        (prompt-choice :runner "Done")
        (is (= 0 (count (:discard (get-runner)))) "Runner prevented damage")
        (is (= 1 (count (:prompt (get-runner)))) "Runner has a next damage prevention prompt")
        (prompt-choice :runner "Done")
        (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")))))

(deftest bioroid-work-crew
  ;; Bioroid Work Crew
  (letfn [(bwc-test [card]
            (do-game
              (new-game
                (default-corp [(qty "Bioroid Work Crew" 1)
                               (qty card 1)])
                (default-runner))
              (play-from-hand state :corp "Bioroid Work Crew" "New remote")
              (let [bwc (get-content state :remote1 0)]
                (core/rez state :corp bwc)
                (card-ability state :corp bwc 0)
                (prompt-select :corp (find-card card (:hand (get-corp))))
                (prompt-choice :corp "New remote")
                (is (= 0 (count (:hand (get-corp)))))
                (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now"))))]
    (doall (map bwc-test
                ["Hostile Takeover"
                 "Dedicated Response Team"
                 "Builder"
                 "Research Station"]))))

(deftest blacklist
  ;; Blacklist
  (testing "#2426.  Need to allow steal."
    (do-game
      (new-game (default-corp [(qty "Fetal AI" 3) (qty "Blacklist" 1)])
                (default-runner))
      (trash-from-hand state :corp "Fetal AI")
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 1 (count (:discard (get-corp)))))
      (take-credits state :corp)
      (run-empty-server state :archives)
      (prompt-choice-partial :runner "Pay")
      (is (= 2 (:agenda-point (get-runner))) "Runner has 2 agenda points")
      (is (= 1 (count (:scored (get-runner))))))))

(deftest breached-dome
  ;; Breached Dome
  (do-game
    (new-game (default-corp [(qty "Breached Dome" 10)])
              (default-runner [(qty "Sure Gamble" 10)]))
    (trash-from-hand state :corp "Breached Dome")
    (play-from-hand state :corp "Breached Dome" "New remote")
    (take-credits state :corp)
    (run-empty-server state "R&D")
    (prompt-choice :runner "No action")
    (is (= 4 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 4 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 2 (count (:discard (get-runner)))) "Runner's discard grew by 2")
    (run-empty-server state "Server 1")
    (prompt-choice :runner "No action")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 3 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 4 (count (:discard (get-runner)))) "Runner's discard grew by 2")
    (run-empty-server state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 2 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 6 (count (:discard (get-runner)))) "Runner's discard grew by 2")))

(deftest brain-taping-warehouse
  ;; Brain-Taping Warehouse - Lower rez cost of Bioroid ICE by 1 for each unspent Runner click
  (do-game
    (new-game (default-corp [(qty "Brain-Taping Warehouse" 1) (qty "Ichi 1.0" 1)
                             (qty "Eli 1.0" 1)])
              (default-runner))
    (play-from-hand state :corp "Brain-Taping Warehouse" "New remote")
    (play-from-hand state :corp "Ichi 1.0" "Server 1")
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (let [ichi (get-ice state :remote1 0)
          eli (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state :remote1)
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 3 (:click (get-runner))))
      (core/rez state :corp ichi)
      (is (= 2 (:credit (get-corp))) "Paid only 2c to rez Ichi; reduction of 3c")
      (run-jack-out state)
      (run-on state :hq)
      (is (= 2 (:click (get-runner))))
      (core/rez state :corp eli)
      (is (= 1 (:credit (get-corp))) "Paid only 1c to rez Eli; reduction of 2c"))))

(deftest broadcast-square
  ;; Broadcast Square - Trace 3: Prevent all bad publicity
  (do-game
    (new-game (default-corp [(qty "Profiteering" 1) (qty "Hostile Takeover" 1) (qty "Broadcast Square" 1)])
              (default-runner))
    (play-from-hand state :corp "Broadcast Square" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 3 (:credit (get-corp))) "Corp should have spent 2 credits")
    (play-from-hand state :corp "Profiteering" "New remote")
    (score-agenda state :corp (get-content state :remote2 0))
    (prompt-choice :corp "3")  ;; Take 3 bad publicity from Profiteering, gain 15 (if bad publicity actually taken)
    (prompt-choice :corp 0)  ;; Corp doesn't pump trace, base 3
    (prompt-choice :runner 0)  ;; Runner doesn't pump trace; loses trace
    (is (= 1 (:agenda-point (get-corp))) "Corp should score a 1-point agenda")
    (is (= 0 (:bad-publicity (get-corp))) "Corp should gain 0 bad publicity")
    (is (= 3 (:credit (get-corp))) "Corp should gain 0 credits")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (score-agenda state :corp (get-content state :remote3 0))
    (prompt-choice :corp 0)  ;; Corp doesn't pump trace, base 3
    (prompt-choice :runner 3)  ;; Runner pumps trace; wins trace
    (is (= 2 (:agenda-point (get-corp))) "Corp should score a 1-point agenda")
    (is (= 1 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity from failed trace")
    (is (= 10 (:credit (get-corp))) "Corp should gain 7 credits")))

(deftest capital-investors
  ;; Capital Investors - Click for 2 credits
  (do-game
    (new-game (default-corp [(qty "Capital Investors" 1)])
              (default-runner))
    (play-from-hand state :corp "Capital Investors" "New remote")
    (let [cap (get-content state :remote1 0)]
      (core/rez state :corp cap)
      (card-ability state :corp cap 0)
      (card-ability state :corp cap 0)
      (is (= 0 (:click (get-corp))) "Used twice, spent 2 clicks")
      (is (= 7 (:credit (get-corp))) "Used twice, gained 4 credits"))))

(deftest cerebral-overwriter
  ;; Cerebral Overwriter
  (do-game
    (new-game (default-corp ["Cerebral Overwriter"])
              (default-runner))
    (play-from-hand state :corp "Cerebral Overwriter" "New remote")
    (let [co (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh co)})
      (core/advance state :corp {:card (refresh co)})
      (is (= 2 (get-in (refresh co) [:advance-counter])))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice :corp "Yes") ; choose to do the optional ability
      (is (= 2 (:brain-damage (get-runner))) "Runner takes 2 brain damage"))))

(deftest chairman-hiro
  ;; Chairman Hiro - Reduce Runner max hand size; add as 2 agenda points if Runner trashes him
  (do-game
    (new-game (default-corp [(qty "Chairman Hiro" 2)])
              (default-runner))
    (play-from-hand state :corp "Chairman Hiro" "New remote")
    (play-from-hand state :corp "Chairman Hiro" "Server 1")
    (prompt-choice :corp "OK")
    (is (= 1 (count (:discard (get-corp)))) "First Hiro trashed")
    (is (= 0 (:agenda-point (get-runner))) "No points for Runner if trashed by Corp")
    (let [hiro (get-content state :remote1 0)]
      (core/rez state :corp hiro)
      (is (= 3 (core/hand-size state :runner)) "Runner max hand size reduced by 2")
      (take-credits state :corp)
      (take-credits state :runner 3)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay") ; trash Hiro
      (is (= 2 (:credit (get-runner))) "Runner paid 6 credits to trash")
      (is (= 5 (core/hand-size state :runner)) "Runner max hand size restored to 5")
      (is (= 1 (count (get-in @state [:runner :scored])))
          "Chairman Hiro added to Runner score area")
      (is (= 2 (:agenda-point (get-runner))) "Runner gained 2 agenda points"))))

(deftest chief-slee
  ;; Chief Slee
  (do-game
    (new-game (default-corp ["Chief Slee" "Hive" "Hedge Fund"])
              (default-runner [(qty "Sure Gamble" 5)]))
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hive" "HQ")
    (play-from-hand state :corp "Chief Slee" "New remote")
    (run-on state :hq)
    (let [slee (get-content state :remote1 0)
          hive (get-ice state :hq 0)]
      (core/rez state :corp hive)
      (card-subroutine state :corp hive 0)
      (dotimes [_ 5]
        (card-ability state :corp slee 0))
      (take-credits state :runner)
      (card-ability state :corp slee 1)
      (is (= 5 (count (:discard (get-runner)))) "Chief Slee should do 5 meat damage"))))

(deftest ci-fund
  ;; C.I. Fund
  (do-game
    (new-game (default-corp ["C.I. Fund" "Hedge Fund"])
              (default-runner))
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "C.I. Fund" "New remote")
    (take-credits state :corp)
    (let [ci (get-content state :remote1 0)]
      (core/rez state :corp ci)
      (take-credits state :runner)
      (card-ability state :corp ci 0)
      (prompt-choice :corp 3)
      (is (= 3 (-> (refresh ci) :counter :credit)))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp ci 0)
      (prompt-choice :corp 3)
      (is (= 6 (-> (refresh ci) :counter :credit)))
      (core/end-phase-12 state :corp nil)
      (is (= 8 (-> (refresh ci) :counter :credit)))
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 10 (-> (refresh ci) :counter :credit)))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp ci 1)
        (is (= 8 (- (:credit (get-corp)) credits)))
        (is (nil? (-> (refresh ci) :counter :credit)))))))

(deftest city-surveillance
  ;; City Surveillance - Runner chooses to pay 1 credit or take 1 tag at start of their turn
  (do-game
    (new-game (default-corp [(qty "City Surveillance" 1)])
              (default-runner))
    (play-from-hand state :corp "City Surveillance" "New remote")
    (let [surv (get-content state :remote1 0)]
      (core/rez state :corp surv)
      (take-credits state :corp)
      (is (some #{"Pay 1[Credits]" "Take 1 tag"} (-> (get-runner) :prompt first :choices)))
      (prompt-choice :runner "Pay 1[Credits]")
      (is (= 4 (:credit (get-runner))) "Runner paid 1 credit")
      (is (= 0 (:tag (get-runner))) "Runner didn't take a tag")
      (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner))) ;; Set Runner's credits to 0 so they can't choose to pay
      (take-credits state :corp)
      (is (some #{"Take 1 tag"} (-> (get-runner) :prompt first :choices)))
      (prompt-choice :runner "Take 1 tag")
      (is (= 0 (:credit (get-runner))) "Runner paid no credits")
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag"))
      (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")))

(deftest clone-suffrage-movement
  ;; Clone Suffrage Movement
  (do-game
    (new-game (default-corp ["Clone Suffrage Movement" (qty "Hedge Fund" 2) "Ice Wall"])
              (default-runner))
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Clone Suffrage Movement" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (let [csm (get-content state :remote1 0)]
      (core/rez state :corp (refresh csm))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (-> (get-corp) :discard count)) "Clone Suffrage Movement should activate")
      (is (:corp-phase-12 @state) "Corp should get option to fire Clone Suffrage Movement")
      ;; Runner has 1+ credit and chooses to pay 1 credit
      (card-ability state :corp csm 0)
      (prompt-select :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (core/end-phase-12 state :corp nil)
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Clone Suffrage Movement didn't activate cuz of the ice"))))

(deftest clyde-van-rite
  ;; Clyde Van Rite - Multiple scenarios involving Runner not having credits/cards to trash
  (do-game
    (new-game (default-corp ["Clyde Van Rite" "Ice Wall"])
              (default-runner [(qty "Sure Gamble" 7)]))
    (play-from-hand state :corp "Clyde Van Rite" "New remote")
    (let [clyde (get-content state :remote1 0)]
      (core/rez state :corp clyde)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp in Step 1.2")
      ;; Runner has 1+ credit and chooses to pay 1 credit
      (card-ability state :corp clyde 0)
      (is (= 9 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (is (some #{"Pay 1[Credits]" "Trash top card"} (-> (get-runner) :prompt first :choices)))
      (prompt-choice-partial :runner "Pay")
      (is (= 8 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner can't pay 1 credit so must trash top card
      (core/lose state :runner :credit (:credit (get-runner)))
      (card-ability state :corp clyde 0)
      (is (= 0 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (is (some #{"Trash top card"} (-> (get-runner) :prompt first :choices)))
      (prompt-choice-partial :runner "Trash")
      (is (= 0 (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner has 1+ card in Stack and chooses to trash 1 card
      (card-ability state :corp clyde 0)
      (is (= 4 (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (is (some #{"Pay 1[Credits]" "Trash top card"} (-> (get-runner) :prompt first :choices)))
      (prompt-choice :runner "Trash top card")
      (is (= 4 (:credit (get-runner))))
      (is (= 0 (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner has no cards in Stack so must pay 1 credit
      (card-ability state :corp clyde 0)
      (is (= 8 (:credit (get-runner))))
      (is (= 0 (count (:deck (get-runner)))))
      (is (some #{"Pay 1[Credits]"} (-> (get-runner) :prompt first :choices)))
      (prompt-choice-partial :runner "Pay")
      (is (= 7 (:credit (get-runner))))
      (is (= 0 (count (:deck (get-runner)))))
      (take-credits state :corp)
      (dotimes [_ 4]
        (core/click-credit state :runner nil))
      (core/lose state :runner :credit (:credit (get-runner)))
      (core/end-turn state :runner nil)
      ;; Runner has no credits and no cards so nothing happens
      (card-ability state :corp clyde 0)
      (is (= 0 (:credit (get-runner))))
      (is (= 0 (count (:deck (get-runner)))))
      (is (empty? (get-in @state [:corp :prompt]))))))

(deftest commercial-bankers-group
  ;; Commercial Bankers Group - Gain 3 credits at turn start if unprotected by ice
  (do-game
    (new-game (default-corp [(qty "Commercial Bankers Group" 1) (qty "Ice Wall" 1)])
              (default-runner))
    (play-from-hand state :corp "Commercial Bankers Group" "New remote")
    (let [cbg (get-content state :remote1 0)]
      (core/rez state :corp cbg)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Bankers Group paid 3 credits")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (is (= 11 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 11 (:credit (get-corp))) "Bankers Group didn't pay credits"))))

(deftest constellation-protocol
  ;; Constellation Protocol
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Constellation Protocol" "Ice Wall" "Fire Wall"])
                (default-runner))
      (core/gain state :corp :credit 100 :click 10)
      (play-from-hand state :corp "Constellation Protocol" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Fire Wall" "New remote")
      (let [cp (get-content state :remote1 0)
            iw (get-ice state :remote2 0)
            fw (get-ice state :remote3 0)]
        (core/rez state :corp cp)
        (core/rez state :corp iw)
        (core/rez state :corp fw)
        (advance state iw 1)
        (advance state fw 1)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Should be waiting for Constellation Protocol to be fired")
        (card-ability state :corp cp 0)
        (is (= 1 (:advance-counter (refresh iw))))
        (is (= 1 (:advance-counter (refresh fw))))
        (prompt-select :corp (refresh iw))
        (prompt-select :corp (refresh fw))
        (is (= 0 (:advance-counter (refresh iw))))
        (is (= 2 (:advance-counter (refresh fw))))
        (core/end-phase-12 state :corp nil))))
  (testing "Variable number of advanceable cards"
    (do-game
      (new-game (default-corp ["Constellation Protocol" "Ice Wall" "Hive"])
                (default-runner))
      (core/gain state :corp :credit 100 :click 10)
      (play-from-hand state :corp "Constellation Protocol" "New remote")
      (let [cp (get-content state :remote1 0)]
        (core/rez state :corp cp))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire with no advanceable ice")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote2 0)]
        (core/rez state :corp iw)
        (advance state iw 1)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire with only a single ice"))
      (play-from-hand state :corp "Hive" "New remote")
      (let [hive (get-ice state :remote3 0)]
        (core/rez state :corp hive)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire when the target ice can't be advanced"))))
  (testing "Can't advance assets"
    (do-game
      (new-game (default-corp ["Constellation Protocol" "Ice Wall" "Contract Killer"])
                (default-runner))
      (core/gain state :corp :credit 100 :click 10)
      (play-from-hand state :corp "Constellation Protocol" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Contract Killer" "New remote")
      (let [cp (get-content state :remote1 0)
            iw (get-ice state :remote2 0)
            ck (get-content state :remote3 0)]
        (core/rez state :corp cp)
        (core/rez state :corp iw)
        (core/rez state :corp ck)
        (advance state iw 1))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire when only target is asset"))))

(deftest contract-killer
  ;; Contract Killer
  (do-game
    (new-game (default-corp ["Contract Killer"])
              (default-runner [(qty "Sure Gamble" 2) "Data Dealer"]))
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Contract Killer" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Data Dealer")
    (take-credits state :runner)
    (let [ck (get-content state :remote1 0)]
      (advance state ck 2)
      (card-ability state :corp ck 0)
      (prompt-select :corp (get-resource state 0))
      (is (= 1 (-> (get-corp) :discard count)) "Contract Killer should be trashed as an ability cost")
      (is (= 1 (-> (get-runner) :discard count)) "Contract Killer should trash Data Dealer"))
    (take-credits state :corp)
    (take-credits state :runner)
    (core/gain state :corp :click 1)
    (core/move state :corp (find-card "Contract Killer" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Contract Killer" "New remote")
    (let [ck (get-content state :remote2 0)]
      (advance state ck 2)
      (card-ability state :corp ck 1)
      (is (= 1 (-> (get-corp) :discard count)) "Contract Killer should be trashed as an ability cost")
      (is (= 3 (-> (get-runner) :discard count)) "Contract Killer should do 2 meat damage"))))

(deftest corporate-town
  ;; Corporate Town
  (do-game
    (new-game (default-corp ["Corporate Town" "Hostile Takeover"])
              (default-runner ["Data Dealer"]))
    (core/gain state :corp :click 1)
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Corporate Town" "New remote")
    (let [ct (get-content state :remote2 0)
          ht (get-scored state :corp)]
      (core/rez state :corp ct)
      (prompt-select :corp ht)
      (take-credits state :corp)
      (play-from-hand state :runner "Data Dealer")
      (take-credits state :runner)
      (card-ability state :corp ct 0)
      (prompt-select :corp (get-resource state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Corporate Town should trash Data Dealer")
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Corporate Town shouldn't activate if there are no resources"))))

(deftest cpc-generator
  ;; CPC Generator
  (do-game
    (new-game (default-corp ["CPC Generator"])
              (default-runner))
    (play-from-hand state :corp "CPC Generator" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (let [credits (:credit (get-corp))]
      (core/click-credit state :runner nil)
      (is (= 1 (- (:credit (get-corp)) credits)) "Should gain one from CPC Generator"))
    (let [credits (:credit (get-corp))]
      (core/click-credit state :runner nil)
      (is (= 0 (- (:credit (get-corp)) credits)) "Shouldn't gain another credit from CPC Generator"))))

(deftest daily-business-show
  ;; Daily Business Show
  (testing "Full test"
    (do-game
      (new-game (default-corp [(qty "Daily Business Show" 3) (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                               (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
                (default-runner))
      (starting-hand state :corp ["Daily Business Show" "Daily Business Show" "Daily Business Show" "Hedge Fund"])
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (take-credits state :runner)
      (is (= 5 (count (:hand (get-corp)))) "Drew an additional 3 cards with 3 DBS")
      (is (not-empty (:prompt (get-runner))) "Runner is waiting for Corp to use DBS")
      (prompt-select :corp (find-card "Hedge Fund" (:hand (get-corp)))) ;invalid target
      (prompt-select :corp (find-card "Resistor" (:hand (get-corp))))
      (prompt-select :corp (find-card "Product Placement" (:hand (get-corp))))
      (prompt-select :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (count (:hand (get-corp)))))
      (is (= "Hedge Fund" (:title (first (:hand (get-corp))))))
      (is (= "Jackson Howard" (:title (second (:hand (get-corp))))))
      (is (= "Resistor" (:title (last (:deck (get-corp))))) "Resistor last card in deck")
      (is (= "Product Placement" (:title (last (butlast (:deck (get-corp))))))
          "Product Placement second last card in deck")
      (is (= "Breaking News" (:title (last (butlast (butlast (:deck (get-corp)))))))
          "Breaking News third last card in deck")))
  (testing "Sensie Actors Union interaction"
    (do-game
      (new-game (default-corp [(qty "Daily Business Show" 1) (qty "Sensie Actors Union" 2)
                               (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                               (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
                (default-runner))
      (starting-hand state :corp ["Daily Business Show" "Sensie Actors Union" "Sensie Actors Union" "Hedge Fund"])
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Sensie Actors Union" "New remote")
      (play-from-hand state :corp "Sensie Actors Union" "New remote")
      (let [sensie1 (get-content state :remote2 0)
            sensie2 (get-content state :remote3 0)]
        (core/rez state :corp (get-content state :remote1 0))
        (core/rez state :corp sensie1)
        (core/rez state :corp sensie2)
        (take-credits state :corp)
        (take-credits state :runner)
        ;; Use first Sensie
        (is (= 1 (count (:hand (get-corp)))))
        (card-ability state :corp sensie1 0)
        (is (= 5 (count (:hand (get-corp)))) "Drew 3 cards with Sensie, +1 with DBS")
        (prompt-select :corp (find-card "Resistor" (:hand (get-corp)))) ; DBS target
        (prompt-select :corp (find-card "Hedge Fund" (:hand (get-corp)))) ; Sensie target
        (is (= 3 (count (:hand (get-corp)))))
        (is (= "Hedge Fund" (:title (last (:deck (get-corp))))) "Hedge Fund last card in deck")
        (is (= "Resistor" (:title (last (butlast (:deck (get-corp))))))
            "Resistor second last card in deck")
        ;; Try to use first Sensie again
        (card-ability state :corp sensie1 0)
        (is (empty? (get-in @state [:corp :prompt])) "Sensie didn't activate")
        (is (= 3 (count (:hand (get-corp)))))
        ;; Use second Sensie
        (starting-hand state :corp ["Hedge Fund" "Jackson Howard"])
        (is (= 2 (count (:hand (get-corp)))))
        (card-ability state :corp sensie2 0)
        (is (= 5 (count (:hand (get-corp)))) "Drew 3 cards with Sensie, DBS didn't activate")
        (prompt-select :corp (find-card "Breaking News" (:hand (get-corp)))) ; Sensie target
        (is (= "Breaking News" (:title (last (:deck (get-corp))))) "Breaking News last card in deck"))))
  (testing "Should not trigger if rezzed after mandatory draw"
    (do-game
      (new-game (default-corp [(qty "Daily Business Show" 3) (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                               (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
                (default-runner))
      (starting-hand state :corp ["Daily Business Show"])
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/draw state :corp)
      (is (= 1 (count (:hand (get-corp)))) "DBS did not fire on manual draw")
      (is (empty? (:prompt (get-corp))) "Corp is not being asked to bury a card with DBS"))))

(deftest dedicated-response-team
  ;; Dedicated Response Team - Do 2 meat damage when successful run ends if Runner is tagged
  (do-game
    (new-game (default-corp [(qty "Dedicated Response Team" 1)])
              (default-runner))
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (let [drt (get-content state :remote1 0)]
      (core/rez state :corp drt)
      (take-credits state :corp)
      (run-empty-server state :rd)
      (is (empty? (:discard (get-runner))) "Not tagged, no damage done")
      (core/gain state :runner :tag 1)
      (run-on state :rd)
      (run-jack-out state)
      (is (empty? (:discard (get-runner))) "Tagged but run unsuccessful, no damage done")
      (run-empty-server state :rd)
      (is (= 2 (count (:discard (get-runner)))) "Suffered 2 damage for successful run w/ tag"))))

(deftest director-haas
  ;; Director Haas
  (do-game
    (new-game (default-corp [(qty "Director Haas" 2)])
              (default-runner))
    (play-from-hand state :corp "Director Haas" "New remote")
    (play-from-hand state :corp "Director Haas" "Server 1")
    (prompt-choice :corp "OK")
    (is (= 1 (count (:discard (get-corp)))) "First Haas trashed")
    (is (= 0 (:agenda-point (get-runner))) "No points for Runner if trashed by Corp")
    (let [dh (get-content state :remote1 0)]
      (core/rez state :corp dh))
    (is (= 2 (:click (get-corp))) "Corp should immediately gain a click")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 4 (:click (get-corp))) "Corp should have an extra click each turn")
    (take-credits state :corp)
    (take-credits state :runner 3)
    (run-empty-server state "Server 1")
    (prompt-choice-partial :runner "Pay") ; trash Haas
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should be back to 3 clicks")
    (is (= 1 (count (-> @state :runner :scored)))
        "Director Haas added to Runner score area")
    (is (= 2 (:agenda-point (get-runner))) "Runner gained 2 agenda points")))

(deftest docklands-crackdown
  ;; Docklands Crackdown
  (letfn [(dlcd-test [number]
            (do-game
              (new-game (default-corp ["Docklands Crackdown"])
                        (default-runner ["Cache"]))
              (play-from-hand state :corp "Docklands Crackdown" "New remote")
              (let [dlcd (get-content state :remote1 0)]
                (core/rez state :corp dlcd)
                (core/add-counter state :corp dlcd :power number)
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (is (= (- 4 number) (:credit (get-runner)))))))]
    (doall (map dlcd-test [0 1 2 3 4]))))

(deftest early-premiere
  ;; Early Premiere - Pay 1c at start of turn to place an advancement on a card in a server
  (do-game
    (new-game (default-corp [(qty "Early Premiere" 1) (qty "Ice Wall" 1)
                             (qty "Ghost Branch" 1) (qty "Blacklist" 1)])
              (default-runner))
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Early Premiere" "New remote")
    (play-from-hand state :corp "Blacklist" "New remote")
    (play-from-hand state :corp "Ghost Branch" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ep (get-content state :remote1 0)
          bl (get-content state :remote2 0)
          gb (get-content state :remote3 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp ep)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp ep 0)
      (prompt-select :corp iw)
      (is (nil? (:advance-counter (refresh iw))) "Ice Wall can't targeted, not in server")
      (prompt-select :corp bl)
      (is (nil? (:advance-counter (refresh bl))) "Blacklist can't targeted, can't be advanced")
      (prompt-select :corp gb)
      (is (= 1 (:advance-counter (refresh gb))) "1 advancement on Ghost Branch")
      (is (= 4 (:credit (get-corp)))))))

(deftest echo-chamber
  ;; Echo Chamber - 3 clicks to become 1 point agenda
  (do-game
    (new-game (default-corp [(qty "Echo Chamber" 1)])
              (default-runner))
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Echo Chamber" "New remote")
    (let [ec (get-content state :remote1 0)]
      (core/rez state :corp ec)
      (card-ability state :corp ec 0))
    (is (= 1 (:agendapoints (get-in @state [:corp :scored 0]))) "Echo Chamber added to Corp score area")))

(deftest edge-of-world
  ;; Edge of World
  (do-game
    (new-game (default-corp [(qty "Edge of World" 3) (qty "Ice Wall" 3)])
              (default-runner))
    (core/gain state :corp :credit 6 :click 1)
    (play-from-hand state :corp "Edge of World" "New remote")
    (play-from-hand state :corp "Edge of World" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :runner :prompt first :prompt-type))
        "Runner waiting for Corp to act")
    (prompt-choice :corp "Yes")
    (prompt-choice :runner "Yes")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
    (run-empty-server state "Server 2")
    (prompt-choice :corp "Yes")
    (prompt-choice :runner "Yes")
    (is (= 2 (:brain-damage (get-runner))) "Runner did not take brain damage when no ICE protected Edge of World")))

(deftest elizas-toybox
  ;; Eliza's Toybox - Rez a card ignoring all costs
  (do-game
    (new-game (default-corp ["Eliza's Toybox" "Wotan" "Archer"])
              (default-runner))
    (play-from-hand state :corp "Wotan" "R&D")
    (play-from-hand state :corp "Archer" "HQ")
    (play-from-hand state :corp "Eliza's Toybox" "New remote")
    (let [wotan (get-ice state :rd 0)
          archer (get-ice state :hq 0)
          eliza (get-content state :remote1 0)]
      (core/rez state :corp eliza)
      (is (= 1 (:credit (get-corp))))
      (is (= 0 (:click (get-corp))) "3 clicks spent")
      (core/gain state :corp :click 6)
      (card-ability state :corp eliza 0)
      (prompt-select :corp wotan)
      (is (get-in (refresh wotan) [:rezzed]))
      (is (= 3 (:click (get-corp))) "3 clicks spent")
      (is (= 1 (:credit (get-corp))) "No credits spent")
      (card-ability state :corp eliza 0)
      (prompt-select :corp archer)
      (is (get-in (refresh archer) [:rezzed]))
      (is (= 0 (:click (get-corp))) "3 clicks spent")
      (is (= 1 (:credit (get-corp))) "No credits or agendas spent"))))

(deftest elizabeth-mills
  ;; Elizabeth Mills - Remove 1 bad publicity when rezzed; click-trash to trash a location
  (do-game
    (new-game (default-corp [(qty "Elizabeth Mills" 1)])
              (default-runner [(qty "Earthrise Hotel" 1)]))
    (core/gain state :corp :bad-publicity 1)
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (take-credits state :runner)
    (let [liz (get-content state :remote1 0)
          hotel (get-in @state [:runner :rig :resource 0])]
      (core/rez state :corp liz)
      (is (= 0 (:bad-publicity (get-corp))) "1 bad publicity removed")
      (card-ability state :corp liz 0)
      (prompt-select :corp hotel)
      (is (= 1 (count (:discard (get-runner)))) "Earthrise trashed")
      (is (= 1 (count (:discard (get-corp)))) "Elizabeth Mills trashed")
      (is (= 1 (:bad-publicity (get-corp))) "1 bad publicity taken from trashing a location"))))

(deftest encryption-protocol
  ;; Encryption Protocol - Trash cost of installed cards increased by 1
  (do-game
    (new-game (default-corp [(qty "Encryption Protocol" 2)])
              (default-runner))
    (play-from-hand state :corp "Encryption Protocol" "New remote")
    (play-from-hand state :corp "Encryption Protocol" "New remote")
    (let [ep1 (get-content state :remote1 0)
          ep2 (get-content state :remote2 0)]
      (core/rez state :corp ep1)
      (core/rez state :corp ep2)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 4 (core/trash-cost state :runner (refresh ep1)))
          "Trash cost increased to 4 by two active Encryption Protocols")
      (prompt-choice-partial :runner "Pay") ; trash first EP
      (run-empty-server state "Server 2")
      (is (= 3 (core/trash-cost state :runner (refresh ep2)))
          "Trash cost increased to 3 by one active Encryption Protocol"))))

(deftest estelle-moon
  ;; Estelle Moon
  (letfn [(estelle-test [number]
            (do-game
              (new-game (default-corp ["Estelle Moon" (qty "Encryption Protocol" 20)])
                        (default-runner))
              (starting-hand state :corp (repeat 9 "Encryption Protocol"))
              (core/move state :corp (find-card "Estelle Moon" (:deck (get-corp))) :hand)
              (play-from-hand state :corp "Estelle Moon" "New remote")
              (let [em (get-content state :remote1 0)]
                (core/rez state :corp (refresh em))
                (core/gain state :corp :click 10)
                (dotimes [_ number]
                  (play-from-hand state :corp "Encryption Protocol" "New remote"))
                (let [credits (:credit (get-corp))
                      hand (count (:hand (get-corp)))]
                  (card-ability state :corp (refresh em) 0)
                  (is (= (* 2 number) (- (:credit (get-corp)) credits)) (str "Should gain " (* 2 number) " credits"))
                  (is (= number (- (count (:hand (get-corp))) hand)) (str "Should draw " number " cards"))
                  (is (= 1 (-> (get-corp) :discard count)) "Estelle Moon should be trashed")))))]
    (doall (map estelle-test (range 10)))))

(deftest eve-campaign
  ;; Eve Campaign
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 1)])
              (default-runner))
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (let [eve (get-content state :remote1 0)]
      (core/rez state :corp eve)
      (is (= 0 (:credit (get-corp))))
      (is (= 16 (get-counters (refresh eve) :credit)))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 4 (:credit (get-corp))))
      (is (= 14 (get-counters (refresh eve) :credit))))))

(deftest executive-boot-camp
  ;; Executive Boot Camp
  (testing "suppress the start-of-turn event on a rezzed card. Issue #1346"
    (do-game
      (new-game (default-corp [(qty "Eve Campaign" 1) (qty "Executive Boot Camp" 1)])
                (default-runner))
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Executive Boot Camp" "New remote")
      (take-credits state :corp)
      (is (= 6 (:credit (get-corp))) "Corp ends turn with 6 credits")
      (let [eve (get-content state :remote1 0)
            ebc (get-content state :remote2 0)]
        (core/rez state :corp ebc)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp in Step 1.2")
        (card-ability state :corp ebc 0)
        (prompt-select :corp eve)
        (is (= 2 (:credit (get-corp))) "EBC saved 1 credit on the rez of Eve")
        (is (= 16 (get-counters (refresh eve) :credit)))
        (core/end-phase-12 state :corp nil)
        (is (= 2 (:credit (get-corp))) "Corp did not gain credits from Eve")
        (is (= 16 (get-counters (refresh eve) :credit)) "Did not take counters from Eve")
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (:corp-phase-12 @state)) "With nothing to rez, EBC does not trigger Step 1.2")
        (is (= 14 (get-counters (refresh eve) :credit)) "Took counters from Eve"))))
  (testing "works with Ice that has alternate rez costs"
    (do-game
      (new-game (default-corp [(qty "15 Minutes" 1) (qty "Executive Boot Camp" 1)
                               (qty "Tithonium" 1)])
                (default-runner))
      (core/gain state :corp :credit 3)
      (score-agenda state :corp (find-card "15 Minutes" (:hand (get-corp))))
      (play-from-hand state :corp "Tithonium" "HQ")
      (play-from-hand state :corp "Executive Boot Camp" "New remote")
      (let [ebc (get-content state :remote1 0)
            tith (get-ice state :hq 0)]
        (core/rez state :corp ebc)
        (take-credits state :corp)
        (is (= 9 (:credit (get-corp))) "Corp ends turn with 9 credits")
        (take-credits state :runner)
        (is (not (:rezzed (refresh tith))) "Tithonium not rezzed")
        (is (:corp-phase-12 @state) "Corp in Step 1.2")
        (card-ability state :corp ebc 0)
        (prompt-select :corp tith)
        (prompt-choice :corp "No")
        (is (and (:installed (refresh tith)) (:rezzed (refresh tith))) "Rezzed Tithonium")
        (is (= 1 (:credit (get-corp))) "EBC saved 1 credit on the rez of Tithonium")))))

(deftest executive-search-firm
  ;; Executive Search Firm
  (do-game
    (new-game (default-corp ["Executive Search Firm" "Elizabeth Mills"
                             "Midori" "Shannon Claire"])
              (default-runner))
    (starting-hand state :corp ["Executive Search Firm"])
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Executive Search Firm" "New remote")
    (doseq [card ["Elizabeth Mills" "Midori" "Shannon Claire"]]
      (let [esf (get-content state :remote1 0)
            number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (card-ability state :corp esf 0)
        (prompt-choice :corp (find-card card (:deck (get-corp))))
        (is (= card (->> (get-corp) :hand (map :title) first)) (str card " should be in hand"))
        (core/move state :corp (find-card card (:hand (get-corp))) :deck)
        (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Should be shuffled")))))

(deftest expose
  ;; Exposé
  (do-game
    (new-game (default-corp ["Exposé"])
              (default-runner))
    (core/gain state :corp :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :corp "Exposé" "New remote")
      (let [expose (get-content state (keyword (str "remote" (inc i))) 0)]
        (core/rez state :corp (refresh expose))
        (is (zero? (:bad-publicity (get-corp))) "Corp should have 0 bad publicity to start with")
        (when (pos? i)
          (core/gain-bad-publicity state :corp i)
          (is (= i (:bad-publicity (get-corp))) (str "Corp should gain " i " bad publicity"))
          (advance state (refresh expose) i))
        (card-ability state :corp (refresh expose) 0)
        (is (zero? (:bad-publicity (get-corp))) "Corp should have 0 bad publicity after using Exposé's ability")
        (is (= 1 (-> (get-corp) :discard count)) "Archives should have 1 card in it")
        (is (= "Exposé" (-> (get-corp) :discard first :title)) "Only card in Archives should be Exposé")
        (core/move state :corp (find-card "Exposé" (:discard (get-corp))) :hand)))))

(deftest false-flag
  ;; False Flag
  (testing "when the corp attempts to score False Flag"
    (testing "and False Flag has 7 advancements"
      (do-game
       (new-game (default-corp [(qty "False Flag" 1)])
                 (default-runner))
       (play-from-hand state :corp "False Flag" "New remote")
       (let [ff (get-content state :remote1 0)]
         (core/add-counter state :corp ff :advancement 7)
         (core/rez state :corp (refresh ff))
         (card-ability state :corp (refresh ff) 0)
         (is (nil? (get-content state :remote1 0))
             "False Flag is no longer in remote")
         (is (= 3 (:agendapoints (get-in @state [:corp :scored 0])))
             "the corp can score False Flag")
         (is (= 1 (:click (get-corp)))
             "scoring False Flag costs one click"))))
    (testing "and False Flag has less than 7 advancements"
      (do-game
       (new-game (default-corp [(qty "False Flag" 1)])
                 (default-runner))
       (play-from-hand state :corp "False Flag" "New remote")
       (let [ff (get-content state :remote1 0)]
         (core/add-counter state :corp ff :advancement 6)
         (core/rez state :corp (refresh ff))
         (card-ability state :corp (refresh ff) 0)
         (is (not (nil? (get-content state :remote1 0)))
             "False Flag remains in the remote")
         (is (nil? (:agendapoints (get-in @state [:corp :scored 0])))
             "the corp cannot score false flag")
         (is (= 2 (:click (get-corp)))
             "the corp does not lose a click")))))
  (testing "when the runner accesses False Flag"
    (letfn [(false-flag-tags-test
              [[advancements expected-tags]]
              (testing (str "and False Flag has " advancements " advancements")
                (do-game
                 (new-game (default-corp [(qty "False Flag" 1)])
                           (default-runner))
                 (play-from-hand state :corp "False Flag" "New remote")
                 (core/add-prop state
                                :corp
                                (get-content state :remote1 0)
                                :advance-counter
                                advancements)
                 (take-credits state :corp)
                 (run-empty-server state "Server 1")
                 (prompt-choice :runner "No")
                 (let [tags (:tag (get-runner))]
                   (is (= expected-tags tags)
                       (str "the runner recieves " tags " tags"))))))]
      (doall (map false-flag-tags-test
                  [[0 0]
                   [2 1]
                   [5 2]
                   [10 5]])))))

(deftest franchise-city
  ;; Franchise City
  (do-game
    (new-game (default-corp [(qty "Franchise City" 1) (qty "Accelerated Beta Test" 1)])
              (default-runner))
    (play-from-hand state :corp "Franchise City" "New remote")
    (play-from-hand state :corp "Accelerated Beta Test" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp 1)
    (run-empty-server state "Server 2")
    (prompt-choice :runner "Steal")
    (is (= 0 (count (get-in @state [:corp :servers :server2 :content]))) "Agenda was stolen")
    (is (= 2 (:agenda-point (get-runner))) "Runner stole 2 points")
    (is (= 0 (count (get-in @state [:corp :servers :server1 :content])))
        "Franchise City no longer installed")
    (is (find-card "Franchise City" (:scored (get-corp))) "Franchise City in corp scored area")
    (is (= 1 (:agenda-point (get-corp))) "Corp has 1 point")))

(deftest full-immersion-recstudio
  ;; Full Immmersion RecStudio - install directly, and via Interns
  (testing "Full test"
    (do-game
      (new-game
        (default-corp [(qty "Full Immersion RecStudio" 1)
                       (qty "Interns" 2)
                       (qty "Launch Campaign" 3)])
        (default-runner))
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (core/rez state :corp fir)
        (card-ability state :corp fir 0)
        (prompt-select :corp (find-card "Launch Campaign" (:hand (get-corp))))
        (let [lc (first (:hosted (refresh fir)))]
          (is lc "Launch Campaign hosted on Full Immersion RecStudio")
          (core/rez state :corp lc)
          (is (and (:installed (refresh lc)) (:rezzed (refresh lc))) "Rezzed Launch Campaign")
          (take-credits state :corp)
          (take-credits state :runner)
          (is (= 5 (:credit (get-corp))) "Gained 2cr from Launch Campaign")
          (is (= 4 (get-counters (refresh lc) :credit)) "4cr left on Launch Campaign")
          (play-from-hand state :corp "Interns")
          (prompt-select :corp (find-card "Launch Campaign" (:hand (get-corp))))
          (prompt-choice :corp (refresh fir))
          (is (= 2 (count (:hosted (refresh fir)))) "Interns installed onto FIR")))))
  (testing "hosting an asset with events does not double-register events. Issue #1827"
    (do-game
      (new-game
        (default-corp [(qty "Full Immersion RecStudio" 1) (qty "Sandburg" 1) (qty "Vanilla" 1)
                       (qty "Oaktown Renovation" 1)])
        (default-runner))
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (play-from-hand state :corp "Vanilla" "HQ")
      (let [fir (get-content state :remote1 0)
            van (get-ice state :hq 0)]
        (core/rez state :corp fir)
        (core/rez state :corp van)
        (card-ability state :corp fir 0)
        (prompt-select :corp (find-card "Sandburg" (:hand (get-corp))))
        (core/gain state :corp :credit 7 :click 3)
        (core/rez state :corp (first (:hosted (refresh fir))))
        (is (= 2 (:current-strength (refresh van))) "Vanilla at 2 strength")
        (card-ability state :corp fir 0)
        (prompt-select :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (core/advance state :corp {:card (last (:hosted (refresh fir)))})
        (is (= 11 (:credit (get-corp))) "Gained 1cr from advancing Oaktown")))))

(deftest fumiko-yamamori
  ;; Fumiko Yamamori
  (do-game
    (new-game
      (default-corp ["Fumiko Yamamori"])
      (default-runner))
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Fumiko Yamamori" "New remote")
    (let [fumiko (get-content state :remote1 0)]
      (core/rez state :corp (refresh fumiko))
      (core/psi-game state :corp (refresh fumiko)
                     {:equal  {:msg "resolve equal bets effect"}
                      :not-equal {:msg "resolve unequal bets effect"}})
      (prompt-choice :corp "2 [Credits]")
      (prompt-choice :runner "0 [Credits]")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should discard a card to meat damage"))))

(deftest gene-splicer
  ;; Gene Splicer
  (testing "Runner accesses an unadvanced Gene Splicer and doesn't trash
           ;; No net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game
        (default-corp [(qty "Gene Splicer" 1)])
        (default-runner [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice :runner "No action")
      (is (= 0 (count (:discard (get-runner)))) "Runner took no net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses an unadvanced Gene Splicer and trashes it.
           No net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game
        (default-corp [(qty "Gene Splicer" 1)])
        (default-runner [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (is (= 0 (count (:discard (get-runner)))) "Runner took no net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Runner accesses a single-advanced Gene Splicer and doesn't trash.
           1 net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game
        (default-corp [(qty "Gene Splicer" 1)])
        (default-runner [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 1)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice :runner "No action")
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses a single-advanced Gene Splicer and trashes it.
           1 net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game
        (default-corp [(qty "Gene Splicer" 1)])
        (default-runner [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 1)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Runner accesses a double-advanced Gene Splicer and doesn't trash
           2 net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game
        (default-corp [(qty "Gene Splicer" 1)])
        (default-runner [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 2)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice :runner "No")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses a double-advanced Gene Splicer and trashes it.
           2 net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game
        (default-corp [(qty "Gene Splicer" 1)])
        (default-runner [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 2)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Corp triple-advances a Gene Splicer and uses its ability to add to their score area as a 1 point agenda"
    (do-game
      (new-game
        (default-corp [(qty "Gene Splicer" 2) (qty "Ice Wall" 3) (qty "Vanilla" 2)])
        (default-runner [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (let [gs (get-content state :remote1 0)]
        (core/add-counter state :corp gs :advancement 2)
        (take-credits state :runner)
        (core/add-counter state :corp (refresh gs) :advancement 1)
        (core/rez state :corp (refresh gs))
        (card-ability state :corp (refresh gs) 0)
        (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
        (is (= 1 (:agendapoints (get-in @state [:corp :scored 0]))) "Gene Splicer added to Corp score area")))))

(deftest genetics-pavilion
  ;; Genetics Pavilion - Limit Runner to 2 draws per turn, but only during Runner's turn
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Genetics Pavilion" 1)])
                (default-runner [(qty "Diesel" 1) (qty "Sure Gamble" 3) (qty "Sports Hopper" 1)]))
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :corp)
        (core/rez state :corp gp)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (play-from-hand state :runner "Sports Hopper")
        (play-from-hand state :runner "Diesel")
        (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
        (take-credits state :runner)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (let [hopper (get-in @state [:runner :rig :hardware 0])]
          (card-ability state :runner hopper 0)
          (is (= 3 (count (:hand (get-runner)))) "Able to draw 3 cards during Corp's turn")
          (core/derez state :corp (refresh gp))
          (take-credits state :corp)
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/move state :runner (find-card "Diesel" (:discard (get-runner))) :hand)
          (is (= 1 (count (:hand (get-runner)))))
          (play-from-hand state :runner "Diesel")
          (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards with Diesel")
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/rez state :corp (refresh gp))
          (core/draw state :runner)
          (is (= 2 (count (:hand (get-runner)))) "No card drawn; GP counts cards drawn prior to rez")))))
  (testing "vs Fisk Investment Seminar"
    (do-game
      (new-game (default-corp [(qty "Genetics Pavilion" 1) (qty "Hedge Fund" 3)])
                (default-runner [(qty "Fisk Investment Seminar" 1) (qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :corp)
        (core/rez state :corp gp)
        (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (is (= 1 (count (:hand (get-runner)))))
        (is (= 0 (count (:hand (get-corp)))))
        (play-from-hand state :runner "Fisk Investment Seminar")
        (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
        (is (= 3 (count (:hand (get-corp)))) "Drew all 3 cards"))))
  (testing "Mr. Li interaction. #1594"
    (do-game
      (new-game (default-corp [(qty "Genetics Pavilion" 1)])
                (default-runner [(qty "Mr. Li" 1) (qty "Account Siphon" 1) (qty "Faerie" 1)
                                 (qty "Sure Gamble" 1) (qty "John Masanori" 1) (qty "Desperado" 1)]))
      (starting-hand state :runner ["Mr. Li"])
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Mr. Li")
      (let [mrli (get-in @state [:runner :rig :resource 0])]
        (is (= 0 (count (:hand (get-runner)))))
        ; use Mr. Li with 2 draws allowed
        (card-ability state :runner mrli 0)
        (is (= 2 (count (:hand (get-runner)))))
        (prompt-select :runner (first (:hand (get-runner))))
        (is (= 1 (count (:hand (get-runner)))))
        ; use Mr. Li with 0 draws allowed
        (card-ability state :runner mrli 0)
        (is (= 1 (count (:hand (get-runner)))))
        (prompt-select :runner (first (:hand (get-runner)))) ; will fail because not a valid target
        (prompt-choice :runner "Done") ; cancel out
        (take-credits state :runner)
        (take-credits state :corp)
        (core/draw state :runner)
        (is (= 2 (count (:hand (get-runner)))))
        ; use Mr. Li with 1 draw allowed
        (card-ability state :runner mrli 0)
        (is (= 3 (count (:hand (get-runner)))))
        (prompt-select :runner (first (:hand (get-runner)))) ; will fail
        (prompt-select :runner (second (:hand (get-runner)))) ; will fail
        (prompt-select :runner (second (rest (:hand (get-runner)))))
        (is (= 2 (count (:hand (get-runner)))))))))

(deftest ghost-branch
  ;; Ghost Branch - Advanceable; give the Runner tags equal to advancements when accessed
  (do-game
    (new-game (default-corp [(qty "Ghost Branch" 1)])
              (default-runner))
    (play-from-hand state :corp "Ghost Branch" "New remote")
    (let [gb (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh gb)})
      (core/advance state :corp {:card (refresh gb)})
      (is (= 2 (get-in (refresh gb) [:advance-counter])))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice :corp "Yes") ; choose to do the optional ability
      (is (= 2 (:tag (get-runner))) "Runner given 2 tags"))))

(deftest grndl-refinery
  ;; GRNDL Refinery
  (do-game
    (new-game (default-corp ["GRNDL Refinery"])
              (default-runner))
    (core/gain state :corp :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :corp "GRNDL Refinery" "New remote")
      (let [grndl (get-content state (keyword (str "remote" (inc i))) 0)
            credits (- (:credit (get-corp)) i)]
        (when (pos? i)
          (advance state (refresh grndl) i)
          (is (= i (:advance-counter (refresh grndl))) (str "GRNDL Refinery should have " i " advancement counters on it")))
        (card-ability state :corp (refresh grndl) 0)
        (is (= (+ credits (* i 4)) (:credit (get-corp))) (str "Corp should gain " (* i 4) " credits"))
        (is (= 1 (-> (get-corp) :discard count)) "Archives should have 1 card in it")
        (is (= "GRNDL Refinery" (-> (get-corp) :discard first :title)) "Only card in Archives should be GRNDL Refinery")
        (core/move state :corp (find-card "GRNDL Refinery" (:discard (get-corp))) :hand)))))

(deftest haas-arcology-ai
  ;; Haas Arcology AI - Click and advancement to gain 2 clicks, once per turn
  (do-game
    (new-game (default-corp [(qty "Haas Arcology AI" 1)])
              (default-runner))
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Haas Arcology AI" "New remote")
    (let [haa (get-content state :remote1 0)]
      (advance state haa 2)
      (core/rez state :corp (refresh haa))
      (is (= 1 (:click (get-corp))))
      (is (= 2 (:advance-counter (refresh haa))))
      (card-ability state :corp (refresh haa) 0)
      (is (= 1 (:advance-counter (refresh haa))) "Spent 1 advancement")
      (is (= 2 (:click (get-corp))) "Spent last click to gain 2 clicks")
      (card-ability state :corp (refresh haa) 0)
      (is (= 1 (:advance-counter (refresh haa))) "Can't use twice in a turn")
      (is (= 2 (:click (get-corp))) "Didn't spend a click"))))

(deftest honeyfarm
  ;; Honeyfarm - lose one credit on access
  (do-game
    (new-game (default-corp [(qty "Honeyfarm" 3)])
              (default-runner))
    (trash-from-hand state :corp "Honeyfarm")
    (play-from-hand state :corp "Honeyfarm" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= 4 (:credit (get-runner))))
    (run-empty-server state "Archives")
    (is (= 3 (:credit (get-runner))))
    (run-empty-server state "HQ")
    (is (= 2 (:credit (get-runner))))))

(deftest hostile-infrastructure
  ;; Hostile Infrastructure - do 1 net damage when runner trashes a corp card
  (do-game
    (new-game (default-corp [(qty "Hostile Infrastructure" 3)])
              (default-runner))
    (core/gain state :runner :credit 50)
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (run-empty-server state :hq)
    (prompt-choice-partial :runner "Pay")
    (is (= 1 (count (:discard (get-runner)))) "Took 1 net damage")
    (run-empty-server state :remote1)
    (prompt-choice-partial :runner "Pay")
    (is (= 2 (count (:discard (get-runner)))) "Took 1 net damage")))

(deftest hyoubu-research-facility
  ;; Hyoubu Research Facility
  (do-game
    (new-game (default-corp [(qty "Hyoubu Research Facility" 1) (qty "Snowflake" 1)])
              (default-runner))
    (play-from-hand state :corp "Hyoubu Research Facility" "New remote")
    (play-from-hand state :corp "Snowflake" "HQ")
    (let [hrf (get-content state :remote1 0)
          sf (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (core/rez state :corp hrf)
      (core/rez state :corp sf)
      (card-subroutine state :corp sf 0)
      (prompt-choice :corp "2 [Credits]")
      (prompt-choice :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 2c from Hyoubu")
      (run-on state "HQ")
      (card-subroutine state :corp sf 0)
      (prompt-choice :corp "2 [Credits]")
      (prompt-choice :runner "0 [Credits]")
      (is (= 3 (:credit (get-corp))) "No credits gained from Hyoubu"))))

(deftest ibrahim-salem
  ;; Ibrahim Salem
  (do-game
    (new-game (default-corp ["Hostile Takeover" "Ibrahim Salem"])
              (default-runner ["Sure Gamble" "Astrolabe" "Paperclip" "Daily Casts"]))
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Ibrahim Salem" "New remote")
    (let [is (get-content state :remote2 0)]
      (core/rez state :corp (refresh is))
      (prompt-select :corp (-> (get-corp) :scored first))
      (doseq [[i [card-type card-name]]
              (map-indexed vector ['("Event" "Sure Gamble")
                                   '("Hardware" "Astrolabe")
                                   '("Program" "Paperclip")
                                   '("Resource" "Daily Casts")])]
        (take-credits state :corp)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp is in Step 1.2")
        (card-ability state :corp is 0)
        (prompt-choice :corp card-type)
        (prompt-choice :corp (find-card card-name (:hand (get-runner))))
        (core/end-phase-12 state :corp nil)
        (is (= (inc i) (-> (get-runner) :discard count)))))))

(deftest illegal-arms-factory
  ;; Illegal Arms Factory; draw a card, gain a credit, bad pub when trashed while rezzed
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 1)
                             (qty "Beanstalk Royalties" 1)
                             (qty "IPO" 1)
                             (qty "Illegal Arms Factory" 3)])
              (default-runner))
    (core/gain state :runner :credit 20)
    (core/move state :corp (find-card "IPO" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Beanstalk Royalties" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Illegal Arms Factory" "New remote")
    (play-from-hand state :corp "Illegal Arms Factory" "New remote")
    (let [iaf (get-content state :remote2 0)]
      (core/rez state :corp iaf)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (prompt-choice-partial :runner "Pay")
      (is (= 0 (:bad-publicity (get-corp))) "Took no bad pub on unrezzed trash")
      (take-credits state :runner)
      (is (= 3 (count (:hand (get-corp)))) "Drew a card from IAF + mandatory")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit from IAF")
      (take-credits state :corp)
      (run-empty-server state :remote2)
      (prompt-choice-partial :runner "Pay")
      (is (= 1 (:bad-publicity (get-corp))) "Took a bad pub on rezzed trash"))))

(deftest indian-union-stock-exchange
  ;; Indian Union Stock Exchange
  (do-game
    (new-game (make-deck "Argus Security: Protection Guaranteed"
                         ["Indian Union Stock Exchange" "Beanstalk Royalties"
                          "Kill Switch" "Net Police"])
              (default-runner))
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Indian Union Stock Exchange" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= (+ 3 credits) (:credit (get-corp))) "Corp should only gain 3 credits"))
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Kill Switch")
      (is (= credits (:credit (get-corp))) "Corp should neither gain nor lose any credits"))
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Net Police" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (is (= credits (:credit (get-corp))) "Corp should neither gain nor lose any credits"))))

(deftest it-department
  ;; IT Department - Add strength to rezzed ICE until end of turn
  (do-game
    (new-game (default-corp [(qty "IT Department" 1) (qty "Wall of Static" 1)])
              (default-runner))
    (play-from-hand state :corp "IT Department" "New remote")
    (play-from-hand state :corp "Wall of Static" "Server 1")
    (let [itd (get-content state :remote1 0)
          wos (get-ice state :remote1 0)]
      (core/rez state :corp itd)
      (core/rez state :corp wos)
      (card-ability state :corp itd 1)
      (is (= 0 (:click (get-corp))) "Spent 1 click")
      (is (= 1 (get-counters (refresh itd) :power)) "IT Dept has 1 counter")
      (core/add-counter state :corp (refresh itd) :power 4)
      (is (= 5 (get-counters (refresh itd) :power)) "IT Dept has 5 counters")
      (card-ability state :corp itd 0)
      (prompt-select :corp wos)
      ;; refer to online guides for summary of how this ludicrous formula is calculated
      (is (= 8 (:current-strength (refresh wos))) "Gained 5 strength")
      (is (= 4 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 0)
      (prompt-select :corp wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 3 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 0)
      (prompt-select :corp wos)
      (is (= 12 (:current-strength (refresh wos))) "Gained total of 9 strength")
      (is (= 2 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 0)
      (prompt-select :corp wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 1 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (take-credits state :corp)
      (is (= 3 (:current-strength (refresh wos))) "Back to default strength"))))

(deftest jackson-howard
  ;; Jackson Howard - Draw 2 cards
  (do-game
    (new-game (default-corp [(qty "Jackson Howard" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Restructure" 2)])
              (default-runner))
    ;; guaranteed to be at least 1 jhow in hand after draw, and 2 cards in R&D
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (let [jhow (get-content state :remote1 0)]
      (core/rez state :corp jhow)
      (is (= 5 (count (:hand (get-corp)))))
      (is (= 2 (:click (get-corp))))
      (card-ability state :corp jhow 0)
      (is (= 7 (count (:hand (get-corp)))) "Drew 2 cards")
      (is (= 1 (:click (get-corp)))))))

(deftest jeeves-model-bioroids
  (do-game
    (new-game (default-corp [(qty "Jeeves Model Bioroids" 1) (qty "TGTBT" 1)
                             (qty "Melange Mining Corp." 2)])
              (default-runner [(qty "Ghost Runner" 3)]))
    (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Ghost Runner")
    (play-from-hand state :runner "Ghost Runner")
    (play-from-hand state :runner "Ghost Runner")
    (take-credits state :runner)
    ; install 3 things
    (play-from-hand state :corp "TGTBT" "New remote")
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;;click for credits
    (take-credits state :corp 3)
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;;click to purge
    (core/do-purge state :corp 3)
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;;click to advance
    (core/advance state :corp (get-content state :remote2 0))
    (core/advance state :corp (get-content state :remote2 0))
    (core/advance state :corp (get-content state :remote2 0))
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;; use 3 clicks on card ability - Melange
    (core/rez state :corp (get-content state :remote3 0))
    (card-ability state :corp (get-content state :remote3 0) 0)
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;; trash 3 resources
    (core/gain state :runner :tag 1)
    (core/trash-resource state :corp nil)
    (prompt-select :corp (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))))
    (core/trash-resource state :corp nil)
    (prompt-select :corp (get-resource state 0))
    (is (= 2 (count (:discard (get-runner)))))
    (core/trash-resource state :corp nil)
    (prompt-select :corp (get-resource state 0))
    (is (= 3 (count (:discard (get-runner)))))
    (is (= 1 (:click (get-corp))))))

(deftest kala-ghoda
  ; Kala Ghoda Real TV
  (do-game
    (new-game (default-corp [(qty "Kala Ghoda Real TV" 1)])
              (default-runner) [(qty "Sure Gamble" 3)])
    (starting-hand state :runner ["Sure Gamble"])
    (play-from-hand state :corp "Kala Ghoda Real TV" "New remote")
    (let [tv (get-content state :remote1 0)]
      (core/rez state :corp tv)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp tv 0)
      (prompt-choice :corp "Done")
      (card-ability state :corp tv 1)
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 1 (count (:discard (get-runner)))))
      (is (last-log-contains? state "Sure Gamble")
          "Kala Ghoda did log trashed card names"))))

(deftest lakshmi-smartfabrics
  ;; Lakshmi Smartfabrics - Gain power counter when rezzing a card; use counters to protect agenda in HQ
  (do-game
    (new-game (default-corp [(qty "Lakshmi Smartfabrics" 1) (qty "Vanilla" 1)
                             (qty "Marked Accounts" 1) (qty "Elective Upgrade" 1)])
              (default-runner))
    (play-from-hand state :corp "Lakshmi Smartfabrics" "New remote")
    (let [lak (get-content state :remote1 0)]
      (core/rez state :corp lak)
      (is (= 1 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter for itself")
      (play-from-hand state :corp "Vanilla" "R&D")
      (play-from-hand state :corp "Marked Accounts" "New remote")
      (core/rez state :corp (get-ice state :rd 0))
      (is (= 2 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter")
      (core/rez state :corp (get-content state :remote2 0))
      (is (= 3 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter")
      (take-credits state :corp)
      (card-ability state :corp (refresh lak) 0)
      (prompt-select :corp (find-card "Elective Upgrade" (:hand (get-corp))))
      (is (last-log-contains? state "Elective Upgrade") "Revealed agenda")
      (is (= 0 (get-counters (refresh lak) :power)) "Spent 3 power counters")
      (run-empty-server state "HQ")
      (prompt-choice :runner "No action")
      (is (empty? (:scored (get-runner))) "Steal prevented by Smartfabrics")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (prompt-choice :runner "Steal")
      (is (= 3 (:agenda-point (get-runner))) "Runner could steal on later turn"))))

(deftest launch-campaign
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 1)])
              (default-runner))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [launch (get-content state :remote1 0)]
      (core/rez state :corp launch)
      (is (= 4 (:credit (get-corp))))
      (is (= 6 (get-counters (refresh launch) :credit)))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))))
      (is (= 4 (get-counters (refresh launch) :credit))))))

(deftest malia-z0l0ka
  ;; Malia Z0L0K4 - blank an installed non-virtual runner resource
  (do-game
   (new-game (default-corp [(qty "Malia Z0L0K4" 2)
                            (qty "Mausolus" 1)])
             (default-runner [(qty "Rachel Beckman" 1)
                              (qty "Daily Casts" 1)
                              (qty "Rumor Mill" 1)]))
   (play-from-hand state :corp "Malia Z0L0K4" "New remote")
   (play-from-hand state :corp "Malia Z0L0K4" "New remote")
   (play-from-hand state :corp "Mausolus" "HQ")
   (take-credits state :corp)
   (let [malia1 (get-content state :remote1 0)
         malia2 (get-content state :remote2 0)
         mausolus (get-ice state :hq 0)]
     (play-from-hand state :runner "Daily Casts")
     (take-credits state :runner)
     (let [N (:credit (get-runner))]
       (core/rez state :corp malia1)
       (prompt-select :corp (get-resource state 0))
       (take-credits state :corp)
       (is (= N (:credit (get-runner))) "Daily casts did not trigger when blanked"))
     (take-credits state :runner)
     (core/derez state :corp malia1)
     (let [N (:credit (get-runner))]
       (take-credits state :corp)
       (is (= (+ N 2) (:credit (get-runner))) "Daily casts triggers again when unblanked"))
     (play-from-hand state :runner "Rachel Beckman")
     (is (= 4 (:click (get-runner))) "Runner has 4 clicks after playing Beckman")
     (core/rez state :corp malia1)
     (prompt-select :corp (get-resource state 1))
     (is (= 3 (:click (get-runner))) "Runner has 3 clicks after Beckman is blank")
     (core/derez state :corp malia1)
     (is (= 4 (:click (get-runner))) "Runner has 4 clicks after Beckman is unblanked")
     (core/rez state :corp malia1)
     (prompt-select :corp (get-resource state 1))
     (core/rez state :corp mausolus)
     (card-subroutine state :corp mausolus 2)
     (is (and (= 1 (:tag (get-runner)))
              (= 0 (count (:discard (get-runner))))) "Runner has 1 tag, but Rachel Beckman not trashed")
     (take-credits state :runner)
     (is (= 0 (count (:hand (get-corp)))) "Malia is not in hand")
     (core/move-card state :corp {:card malia1 :server "HQ"})
     (is (= 1 (count (:hand (get-corp)))) "Malia is in hand")
     (is (= 1 (count (:discard (get-runner)))) "Rachel Beckman got trashed on unblanking")
     (core/rez state :corp malia2)
     (prompt-select :corp (get-resource state 0))
     (let [N (:credit (get-runner))]
       (take-credits state :corp)
       (is (= N (:credit (get-runner))) "Daily casts is blank, so no drip")))
   (play-from-hand state :runner "Rumor Mill")
   (take-credits state :runner)
   (let [N (:credit (get-runner))]
     (take-credits state :corp)
     (is (= (+ N 2) (:credit (get-runner)))))))
(deftest mark-yale
  ;; Mark Yale - Spend agenda counters or trash himself to gain credits
  (do-game
    (new-game (default-corp [(qty "Firmware Updates" 1) (qty "Mark Yale" 1)])
              (default-runner))
    (play-from-hand state :corp "Firmware Updates" "New remote")
    (play-from-hand state :corp "Mark Yale" "New remote")
    (let [firm (get-content state :remote1 0)
          yale (get-content state :remote2 0)]
      (score-agenda state :corp firm)
      (core/rez state :corp yale)
      (let [firmscored (get-in @state [:corp :scored 0])]
        (is (= 3 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :corp yale 1)
        (prompt-select :corp firmscored)
        (is (= 7 (:credit (get-corp))) "Gained 3 credits")
        (is (= 2 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :corp yale 1)
        (prompt-select :corp firmscored)
        (is (= 10 (:credit (get-corp))) "Gained 3 credits")
        (is (= 1 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :corp yale 1)
        (prompt-select :corp firmscored)
        (is (= 13 (:credit (get-corp))) "Gained 3 credits")
        (is (= 0 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :corp yale 1)
        (prompt-select :corp firmscored)
        (is (= 13 (:credit (get-corp))) "Gained 0 credits because agenda needs a counter")
        (is (= 0 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :corp yale 0)
        (is (= 15 (:credit (get-corp))) "Gained 2 credits")
        (is (= 1 (count (:discard (get-corp)))) "Mark Yale trashed")))))

(deftest mca-austerity-policy
  (do-game
    (new-game
      (default-corp [(qty "MCA Austerity Policy" 1)])
      (default-runner))
    (play-from-hand state :corp "MCA Austerity Policy" "New remote")
    (let [mca (get-content state :remote1 0)]
      (core/rez state :corp mca)
      (card-ability state :corp mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      ; once per turn only
      (card-ability state :corp mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      (take-credits state :corp)
      ; runner loses a click
      (is (= 3 (:click (get-runner))))
      (take-credits state :runner)
      (card-ability state :corp mca 0)
      (is (= 2 (get-counters (refresh mca) :power)))
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp mca 0)
      (is (= 3 (get-counters (refresh mca) :power)))
      ; Fire MCA
      (is (= 2 (:click (get-corp))))
      (card-ability state :corp (refresh mca) 1)
      (is (= 5 (:click (get-corp)))))))

(deftest mental-health-clinic
  ;; Mental Health Clinic - Gain 1 credit when turn begins; Runner max hand size increased by 1
  (do-game
    (new-game (default-corp [(qty "Mental Health Clinic" 1)])
              (default-runner))
    (play-from-hand state :corp "Mental Health Clinic" "New remote")
    (let [mhc (get-content state :remote1 0)]
      (core/rez state :corp mhc)
      (is (= 6 (core/hand-size state :runner)) "Runner max hand size increased by 1")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Gained 1 credit at start of turn"))))

(deftest net-analytics
  ;; Draw a card when runner avoids or removes 1 or more tags
  (do-game
    (new-game (default-corp [(qty "Ghost Branch" 3) (qty "Net Analytics" 3)])
              (default-runner [(qty "New Angeles City Hall" 3)]))
    (starting-hand state :corp ["Net Analytics" "Ghost Branch"])
    (play-from-hand state :corp "Ghost Branch" "New remote")
    (play-from-hand state :corp "Net Analytics" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "New Angeles City Hall")
    (take-credits state :runner)
    (let [gb (get-content state :remote1 0)
          net (get-content state :remote2 0)
          nach (get-in @state [:runner :rig :resource 0])]
      (core/rez state :corp (refresh net))
      (core/advance state :corp {:card (refresh gb)})
      (is (= 1 (get-in (refresh gb) [:advance-counter])))
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))) "Corp hand size is 1 before run")
      (run-empty-server state "Server 1")
      (prompt-choice :corp "Yes") ; choose to do the optional ability
      (card-ability state :runner nach 0)
      (prompt-choice :runner "Done")
      (prompt-choice :corp "Yes") ; Draw from Net Analytics
      (prompt-choice :runner "No")
      (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
      (is (= 0 (:tag (get-runner))) "Avoided 1 Ghost Branch tag")
      (is (= 2 (count (:hand (get-corp)))) "Corp draw from NA")
      ; tag removal
      (core/tag-runner state :runner 1)
      (prompt-choice :runner "No") ; Don't prevent the tag
      (core/remove-tag state :runner 1)
      (prompt-choice :corp "Yes") ; Draw from Net Analytics
      (is (= 3 (count (:hand (get-corp)))) "Corp draw from NA"))))

(deftest net-police
  ;; Net Police - Recurring credits equal to Runner's link
  (do-game
    (new-game
      (default-corp [(qty "Net Police" 1)])
      (make-deck "Sunny Lebeau: Security Specialist" [(qty "Dyson Mem Chip" 1)
                                                      (qty "Access to Globalsec" 1)]))
    (play-from-hand state :corp "Net Police" "New remote")
    (is (= 2 (:link (get-runner))))
    (let [netpol (get-content state :remote1 0)]
      (core/rez state :corp netpol)
      (is (= 2 (:rec-counter (refresh netpol))) "2 recurring for Runner's 2 link")
      (take-credits state :corp)
      (play-from-hand state :runner "Dyson Mem Chip")
      (take-credits state :runner)
      (is (= 3 (:rec-counter (refresh netpol))) "3 recurring for Runner's 3 link")
      (take-credits state :corp)
      (play-from-hand state :runner "Access to Globalsec")
      (take-credits state :runner)
      (is (= 4 (:rec-counter (refresh netpol))) "4 recurring for Runner's 4 link"))))

(deftest news-team
  ;; News Team - on access take 2 tags or take as agenda worth -1
  (do-game
    (new-game (default-corp [(qty "News Team" 3) (qty "Blacklist" 1)])
              (default-runner))
    (trash-from-hand state :corp "News Team")
    (play-from-hand state :corp "Blacklist" "New remote")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (prompt-choice :runner "Take 2 tags")
    (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
    (run-empty-server state :archives)
    (prompt-choice :runner "Add News Team to score area")
    (is (= 1 (count (:scored (get-runner)))) "News Team added to Runner score area")
    (trash-from-hand state :corp "News Team")
    (core/rez state :corp (get-content state :remote1 0))
    (run-empty-server state :archives)
    (prompt-choice :runner "Add News Team to score area")
    (is (= 2 (count (:scored (get-runner)))) "News Team added to Runner score area with Blacklist rez")))

(deftest ngo-front
  ;; NGO Front - full test
  (do-game
    (new-game (default-corp [(qty "NGO Front" 3)])
              (default-runner))
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (let [ngo1 (get-content state :remote1 0)
          ngo2 (get-content state :remote2 0)
          ngo3 (get-content state :remote3 0)]
      (core/advance state :corp {:card ngo2})
      (core/advance state :corp {:card (refresh ngo3)})
      (core/advance state :corp {:card (refresh ngo3)})
      (core/rez state :corp (refresh ngo1))
      (core/rez state :corp (refresh ngo2))
      (core/rez state :corp (refresh ngo3))
      (is (= 2 (:credit (get-corp))) "Corp at 2 credits")
      (card-ability state :corp ngo1 1)
      (card-ability state :corp ngo1 0)
      (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
      (is (= 0 (count (:discard (get-corp)))) "Nothing trashed")
      (card-ability state :corp ngo2 1)
      (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
      (is (= 0 (count (:discard (get-corp)))) "Nothing trashed")
      (card-ability state :corp ngo2 0)
      (is (= 7 (:credit (get-corp))) "Corp gained 5 credits")
      (is (= 1 (count (:discard (get-corp)))) "1 NGO Front Trashed")
      (card-ability state :corp ngo3 1)
      (is (= 15 (:credit (get-corp))) "Corp gained 8 credits")
      (is (= 2 (count (:discard (get-corp)))) "2 NGO Front Trashed"))))

(deftest pad-factory
  ;; PAD Factory - Click to place an advancement, cannot score target until next turn
  (do-game
    (new-game (default-corp [(qty "PAD Factory" 1) (qty "15 Minutes" 1)])
              (default-runner))
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "PAD Factory" "New remote")
    (play-from-hand state :corp "15 Minutes" "New remote")
    (let [pf (get-content state :remote1 0)
          fif (get-content state :remote2 0)]
      (core/rez state :corp pf)
      (card-ability state :corp (refresh pf) 0)
      (prompt-select :corp fif)
      (card-ability state :corp (refresh pf) 0)
      (prompt-select :corp (refresh fif))
      (is (= 0 (:click (get-corp))) "Spent 2 clicks using PAD Factory twice")
      (is (= 2 (:advance-counter (refresh fif))) "Agenda has 2 advancements")
      (core/score state :corp {:card (refresh fif)})
      (is (empty? (:scored (get-corp))) "Prevented from scoring this turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/score state :corp {:card (refresh fif)})
      (is (= 1 (count (:scored (get-corp)))) "Scored agenda"))))

(deftest palana-agroplex
  ;; Pālanā Agroplex - Both players draw 1 at start of Corp turn
  (do-game
    (new-game (default-corp [(qty "Pālanā Agroplex" 1) (qty "Hedge Fund" 3)])
              (default-runner))
    (starting-hand state :corp ["Pālanā Agroplex"])
    (starting-hand state :runner ["Sure Gamble"])
    (play-from-hand state :corp "Pālanā Agroplex" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (take-credits state :runner)
    (core/end-phase-12 state :corp nil)
    (is (= 2 (count (:hand (get-corp)))) "Corp drew 1 from Agroplex")
    (is (= 2 (count (:hand (get-runner)))) "Runner drew 1 from Agroplex")))

(deftest personalized-portal
  ;; Personalized Portal - on corp turn start, force the runner to draw 1 card
  ;; and then gain 1 credit for every 2 cards in the runners hand
  (do-game
    (new-game (default-corp [(qty "Personalized Portal" 1)])
              (default-runner [(qty "Daily Casts" 3) (qty "Dyson Mem Chip" 3)]))
    (play-from-hand state :corp "Personalized Portal" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (starting-hand state :runner [])
    (is (empty? (:hand (get-runner))) "Runner's grip is empty to start")
    (is (= 4 (:credit (get-corp))) "Corp starts with 4 credits")
    (take-credits state :runner)
    (is (= 1 (count (:hand (get-runner)))) "Runner drew 1 card")
    (is (= 4 (:credit (get-corp))) "Corp gained 0 credits")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-runner)))) "Runner drew 1 card")
    (is (= 8 (:credit (get-corp))) "Corp gained 1 credit")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Runner drew 1 card")
    (is (= 12 (:credit (get-corp))) "Corp gained 1 credit")))

(deftest plan-b
  ;; Plan B - score agenda with adv cost <= # of adv counters
  (do-game
    (new-game (default-corp [(qty "Plan B" 1)
                             (qty "Braintrust" 1)
                             (qty "The Future Perfect" 1)
                             (qty "Mushin No Shin" 1)])
              (default-runner))
    (play-from-hand state :corp "Mushin No Shin")
    (prompt-select :corp (find-card "Plan B" (:hand (get-corp))))
    (take-credits state :corp)
    (run-empty-server state :remote1)
    ;; prompt for corp to use Plan B
    (prompt-choice :corp "Yes")
    ;; Pick TFP, does not score
    (prompt-select :corp (find-card "The Future Perfect" (:hand (get-corp))))
    (is (find-card "The Future Perfect" (:hand (get-corp))) "TFP is not scored")
    ;; Pick Brain Trust, scores
    (prompt-select :corp (find-card "Braintrust" (:hand (get-corp))))
    (is (find-card "Braintrust" (:scored (get-corp))) "Braintrust is scored")))

(deftest political-dealings
  ;; Political Dealings
  (testing "Full test"
    (do-game
      (new-game (default-corp [(qty "Political Dealings" 1) (qty "Medical Breakthrough" 1) (qty "Oaktown Renovation" 1)])
                (default-runner))
      (core/move state :corp (find-card "Medical Breakthrough" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Oaktown Renovation" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Political Dealings" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      ;; Install Medical Breakthrough
      (core/draw state :corp)
      (prompt-choice :corp "Yes")
      (prompt-choice :corp "New remote")
      (is (= "Medical Breakthrough" (:title (get-content state :remote2 0)))
          "Medical Breakthrough installed by Political Dealings")
      ;; Install Oaktown Renovation
      (core/draw state :corp)
      (prompt-choice :corp "Yes")
      (prompt-choice :corp "New remote")
      (is (= "Oaktown Renovation" (:title (get-content state :remote3 0)))
          "Oaktown Renovation installed by Political Dealings")
      (is (= true (:rezzed (get-content state :remote3 0)))
          "Oaktown Renovation installed face up")))
  (testing "Daily Business Show interaction - Draw 2 agendas, install both of them but return 1 to bottom of R&D"
    (do-game
      (new-game (default-corp [(qty "Political Dealings" 1) (qty "Daily Business Show" 1) (qty "Turtlebacks" 1)
                               (qty "Breaking News" 1) (qty "Project Beale" 1)])
                (default-runner))
      (starting-hand state :corp ["Political Dealings" "Daily Business Show" "Turtlebacks"])
      (core/gain state :corp :credit 3)
      (play-from-hand state :corp "Political Dealings" "New remote")
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Turtlebacks" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (take-credits state :corp)
      (is (= 0 (count (:hand (get-corp)))))
      (let [agenda1 (first (:deck (get-corp)))
            agenda2 (second (:deck (get-corp)))]
        (take-credits state :runner)
        ;; Install first agenda
        (is (= 2 (count (:hand (get-corp)))))
        (is (= 0 (:credit (get-corp))))
        (prompt-choice :corp "Yes")
        (prompt-choice :corp "New remote")
        (is (= (:cid agenda1) (:cid (get-content state :remote4 0))))
        (is (= 1 (:credit (get-corp))) "Turtlebacks triggered")
        ;; Install second agenda
        (prompt-choice :corp "Yes")
        (prompt-choice :corp "New remote")
        (is (= (:cid agenda2) (:cid (get-content state :remote5 0))))
        (is (= 2 (:credit (get-corp))) "Turtlebacks triggered")
        ;; DBS - put first agenda at bottom of R&D
        (prompt-select :corp (get-content state :remote4 0))
        (is (= 0 (count (:hand (get-corp)))))
        (is (= (:cid agenda1) (:cid (last (:deck (get-corp))))))))))

(deftest psychic-field
  (testing "Basic test"
    ;; Psychic Field - Do 1 net damage for every card in Runner's hand when accessed/exposed
    (do-game
      (new-game (default-corp [(qty "Psychic Field" 2)])
                (default-runner [(qty "Infiltration" 3) (qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Psychic Field" "New remote")
      (play-from-hand state :corp "Psychic Field" "New remote")
      (let [psyf1 (get-content state :remote1 0)
            psyf2 (get-content state :remote2 0)]
        (take-credits state :corp)
        (starting-hand state :runner ["Infiltration" "Sure Gamble" "Sure Gamble"])
        (play-from-hand state :runner "Infiltration")
        (prompt-choice :runner "Expose a card")
        (prompt-select :runner psyf1)
        (is (= 2 (count (:hand (get-runner)))))
        (prompt-choice :corp "2 [Credits]")
        (prompt-choice :runner "0 [Credits]")
        (is (= 3 (count (:discard (get-runner)))) "Suffered 2 net damage on expose and psi loss")
        (core/gain state :runner :click 3)
        (core/draw state :runner 3)
        (is (= 3 (count (:hand (get-runner)))))
        (run-empty-server state :remote2)
        (prompt-choice :corp "1 [Credits]")
        (prompt-choice :runner "0 [Credits]")
        (is (= 6 (count (:discard (get-runner)))) "Suffered 3 net damage on access and psi loss"))))
  (testing "when in Archives. #1965"
    (do-game
      (new-game (default-corp [(qty "Psychic Field" 2) (qty "Shock!" 2) (qty "Clone Retirement" 2)])
                (default-runner))
      (trash-from-hand state :corp "Psychic Field")
      (trash-from-hand state :corp "Shock!")
      (trash-from-hand state :corp "Clone Retirement")
      (take-credits state :corp)
      ;; Runner run on archives to trigger access choice
      (run-empty-server state :archives)
      (is (not-any? #{"Psychic Field"} (get-in @state [:runner :prompt :choices]))
          "Psychic Field is not a choice to access in Archives")))
  (testing "Interaction with Neutralize All Threats and Hostile Infrastructure, #1208"
    (do-game
      (new-game (default-corp [(qty "Psychic Field" 3) (qty "Hostile Infrastructure" 3)])
                (default-runner [(qty "Neutralize All Threats" 1) (qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Psychic Field" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Neutralize All Threats")
      (run-empty-server state :remote1)
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "1 [Credits]")
      (prompt-choice-partial :runner "Pay")
      (is (not (get-content state :remote1)) "Psychic Field trashed by Neutralize All Threats")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest public-support
  ;; Public support scoring and trashing
  ;; TODO could also test for NOT triggering "when scored" events
  (do-game
    (new-game (default-corp [(qty "Public Support" 2)])
              (default-runner))
    ;; Corp turn 1, install and rez public supports
    (play-from-hand state :corp "Public Support" "New remote")
    (play-from-hand state :corp "Public Support" "New remote")
    (let [publics1 (get-content state :remote1 0)
          publics2 (get-content state :remote2 0)]
      (core/rez state :corp (refresh publics1))
      (core/rez state :corp (refresh publics2))
      (take-credits state :corp)
      ;; Runner turn 1, creds
      (is (= 2 (:credit (get-corp))))
      (is (= 3 (get-counters (refresh publics1) :power)))
      (take-credits state :runner)
      ;; Corp turn 2, creds, check if supports are ticking
      (is (= 2 (get-counters (refresh publics1) :power)))
      (is (= 0 (:agenda-point (get-corp))))
      (is (nil? (:agendapoints (refresh publics1))))
      (take-credits state :corp)
      ;; Runner turn 2, run and trash publics2
      (run-empty-server state "Server 2")
      (prompt-choice-partial :runner "Pay") ; pay to trash
      (is (= 5 (:credit (get-runner))))
      (take-credits state :runner)
      ;; Corp turn 3, check how publics1 is doing
      (is (= 1 (get-counters (refresh publics1) :power)))
      (is (= 0 (:agenda-point (get-corp))))
      (take-credits state :corp)
      ;; Runner turn 3, boring
      (take-credits state :runner)
      ;; Corp turn 4, check the delicious agenda points
      (let [scored-pub (get-in @state [:corp :scored 0])]
        (is (= 1 (:agenda-point (get-corp))) "Gained 1 agenda point")
        (is (= "Public Support" (:title scored-pub)))
        (is (= 1 (:agendapoints scored-pub)))))))

(deftest quarantine-system
  ;; Forfeit agenda to rez up to 3 ICE with 2 credit discount per agenda point
  (do-game
    (new-game
      (default-corp [(qty "Chiyashi" 3) (qty "Quarantine System" 1) (qty "Project Beale" 1)])
      (default-runner))
    (core/gain state :corp :credit 100)
    (core/gain state :corp :click 100)
    (play-from-hand state :corp "Chiyashi" "HQ")
    (play-from-hand state :corp "Chiyashi" "HQ")
    (play-from-hand state :corp "Chiyashi" "HQ")
    (play-from-hand state :corp "Quarantine System" "New remote")
    (play-from-hand state :corp "Project Beale" "New remote")
    (is (= 102 (:credit (get-corp))) "Corp has 102 creds")
    (let [ch1 (get-ice state :hq 0)
          ch2 (get-ice state :hq 1)
          ch3 (get-ice state :hq 2)
          qs (get-content state :remote1 0)
          beale (get-content state :remote2 0)]
      (core/rez state :corp qs)
      (card-ability state :corp qs 0)
      (is (empty? (:prompt (get-corp))) "No prompt to rez ICE")
      (score-agenda state :corp beale)
      ; 1 on rez
      (is (= 101 (:credit (get-corp))) "Corp has 101 creds")
      (card-ability state :corp qs 0)
      (prompt-select :corp (get-in (get-corp) [:scored 0]))
      (prompt-select :corp ch1)
      (prompt-select :corp ch2)
      (prompt-select :corp ch3)
      ; pay 8 per Chiyashi - 24 total
      (is (= 77 (:credit (get-corp))) "Corp has 77 creds")
      (is (empty? (:prompt (get-corp))) "No prompt to rez ICE"))))

(deftest rashida-jaheem
  ;; Rashida Jaheem
  (testing "when there are enough cards in R&D"
    (do-game
      (new-game (default-corp [(qty "Rashida Jaheem" 1) (qty "Hedge Fund" 3)])
                (default-runner))
      (starting-hand state :corp ["Rashida Jaheem"])
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (let [credits (:credit (get-corp))
            cards (count (:hand (get-corp)))
            rj (get-content state :remote1 0)]
        (card-ability state :corp rj 0)
        (prompt-choice :corp "Yes")
        (is (= (+ 3 credits) (:credit (get-corp))))
        (is (= (+ 3 cards) (count (:hand (get-corp))))))))
  (testing "when there aren't enough cards in R&D"
    (do-game
      (new-game (default-corp [(qty "Rashida Jaheem" 1) (qty "Hedge Fund" 4)])
                (default-runner))
      (starting-hand state :corp ["Rashida Jaheem"])
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/draw state :corp)
      (core/draw state :corp)
      (take-credits state :corp)
      (take-credits state :runner)
      (let [credits (:credit (get-corp))
            cards (count (:hand (get-corp)))
            rj (get-content state :remote1 0)]
        (card-ability state :corp rj 0)
        (prompt-choice :corp "Yes")
        (is (= (+ 3 credits) (:credit (get-corp))))
        (is (= (+ 2 cards) (count (:hand (get-corp)))))
        (is (= :runner (:winner @state)) "Runner wins")))))

(deftest reality-threedee
  ;; Reality Threedee - Take 1 bad pub on rez; gain 1c at turn start (2c if Runner tagged)
  (do-game
    (new-game (default-corp [(qty "Reality Threedee" 1)])
              (default-runner))
    (play-from-hand state :corp "Reality Threedee" "New remote")
    (let [r3d (get-content state :remote1 0)]
      (core/rez state :corp r3d)
      (is (= 1 (:bad-publicity (get-corp))) "Took 1 bad pub on rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Gained 1 credit")
      (take-credits state :corp)
      (core/gain state :runner :tag 1)
      (take-credits state :runner)
      (is (= 13 (:credit (get-corp))) "Gained 2 credits because Runner is tagged"))))

(deftest reconstruction-contract
  ;; Reconstruction Contract - place advancement token when runner takes meat damage
  (do-game
    (new-game (default-corp [(qty "Reconstruction Contract" 1) (qty "Scorched Earth" 1) (qty "Pup" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Imp" 3)]))
    (core/gain state :runner :tag 1)
    (core/gain state :corp :credit 5)
    (starting-hand state :runner ["Sure Gamble" "Sure Gamble" "Sure Gamble" "Imp" "Imp"])
    (play-from-hand state :corp "Reconstruction Contract" "New remote")
    (let [rc (get-content state :remote1 0)]
      (core/rez state :corp (refresh rc))
      (play-from-hand state :corp "Scorched Earth")
      (is (= 4 (count (:discard (get-runner)))))
      (is (= 1 (:advance-counter (refresh rc))) "Reconstruction Contract has 1 advancement token")
      (starting-hand state :runner ["Imp" "Imp"])
      (play-from-hand state :corp "Pup" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (card-subroutine state :corp (get-ice state :hq 0) 0)
      (is (= 5 (count (:discard (get-runner)))))
      (is (= 1 (:advance-counter (refresh rc))) "Reconstruction Contract doesn't get advancement token for net damage"))))

(deftest reversed-accounts
  ;; Reversed Accounts - Trash to make Runner lose 4 credits per advancement
  (do-game
    (new-game (default-corp [(qty "Reversed Accounts" 1)])
              (default-runner))
    (play-from-hand state :corp "Reversed Accounts" "New remote")
    (let [rev (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh rev)})
      (core/advance state :corp {:card (refresh rev)})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Sure Gamble")
      (take-credits state :runner)
      (is (= 18 (:credit (get-runner))))
      (core/advance state :corp {:card (refresh rev)})
      (core/advance state :corp {:card (refresh rev)})
      (is (= 4 (:advance-counter (refresh rev))))
      (core/rez state :corp (refresh rev))
      (card-ability state :corp rev 0)
      (is (= 1 (count (:discard (get-corp)))) "Reversed Accounts trashed")
      (is (= 2 (:credit (get-runner))) "Runner lost 16 credits"))))

(deftest ronald-five
  ;; Ronald Five - Runner loses a click every time they trash a Corp card
  (do-game
    (new-game (default-corp [(qty "Ronald Five" 1) (qty "Melange Mining Corp." 1)])
              (default-runner))
    (play-from-hand state :corp "Ronald Five" "New remote")
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :remote1 0))
    (run-empty-server state :remote2)
    (prompt-choice-partial :runner "Pay") ; trash MMC
    (is (= 2 (:click (get-runner))) "Lost 1 click")
    (run-empty-server state :remote1)
    (prompt-choice-partial :runner "Pay") ; trash Ronald Five
    (is (= 0 (:click (get-runner))) "Lost 1 click")))

(deftest ronin
  ;; Ronin - Click-trash to do 3 net damage when it has 4 or more advancements
  (do-game
    (new-game (default-corp [(qty "Ronin" 1) (qty "Mushin No Shin" 1)])
              (default-runner))
    (play-from-hand state :corp "Mushin No Shin")
    (prompt-select :corp (find-card "Ronin" (:hand (get-corp))))
    (let [ron (get-content state :remote1 0)]
      (is (= 3 (:advance-counter (refresh ron))))
      (core/rez state :corp (refresh ron))
      (card-ability state :corp ron 0)
      (is (= 3 (count (:hand (get-runner))))
          "Ronin ability didn't fire with only 3 advancements")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/advance state :corp {:card (refresh ron)})
      (is (= 4 (:advance-counter (refresh ron))))
      (card-ability state :corp ron 0)
      (is (= 3 (count (:discard (get-runner)))) "Ronin did 3 net damage")
      (is (= 2 (count (:discard (get-corp)))) "Ronin trashed"))))

(deftest ronin
  ;; Ronin - doesn't fire (or crash) if no advance counters
  (do-game
    (new-game (default-corp [(qty "Ronin" 1)])
              (default-runner))
    (play-from-hand state :corp "Ronin" "New remote")
    (let [ron (get-content state :remote1 0)]
      (is (nil? (:advance-counter (refresh ron))) "Ronin starts with no counters")
      (core/rez state :corp (refresh ron))
      (card-ability state :corp (refresh ron) 0)
      (is (nil? (:advance-counter (refresh ron))) "Ronin didn't gain counters")
      (is (= 3 (count (:hand (get-runner))))
          "Ronin ability didn't fire with 0 advancements"))))

(deftest sandburg
  ;; Sandburg - +1 strength to all ICE for every 5c when Corp has over 10c
  (do-game
    (new-game (default-corp [(qty "Sandburg" 1) (qty "Ice Wall" 2) (qty "Hedge Fund" 3)])
              (default-runner))
    (core/gain state :corp :click 3 :credit 3)
    (play-from-hand state :corp "Sandburg" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [sb (get-content state :remote1 0)
          iwall1 (get-ice state :hq 0)
          iwall2 (get-ice state :rd 0)]
      (core/rez state :corp iwall1)
      (core/rez state :corp iwall2)
      (core/rez state :corp sb)
      (is (= 6 (:credit (get-corp))))
      (play-from-hand state :corp "Hedge Fund")
      (is (= 10 (:credit (get-corp))))
      (is (= 3 (:current-strength (refresh iwall1))) "Strength boosted by 2")
      (is (= 3 (:current-strength (refresh iwall2))) "Strength boosted by 2")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (is (= 18 (:credit (get-corp))))
      (is (= 4 (:current-strength (refresh iwall1))) "Strength boosted by 3")
      (is (= 4 (:current-strength (refresh iwall2))) "Strength boosted by 3")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (is (= 1 (:current-strength (refresh iwall1))) "Strength back to default")
      (is (= 1 (:current-strength (refresh iwall2))) "Strength back to default"))))

(deftest sealed-vault
  ;; Sealed Vault - Store credits for 1c, retrieve credits by trashing or spending click
  (do-game
    (new-game (default-corp [(qty "Sealed Vault" 1) (qty "Hedge Fund" 1)])
              (default-runner))
    (play-from-hand state :corp "Sealed Vault" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (let [sv (get-content state :remote1 0)]
      (core/rez state :corp sv)
      (card-ability state :corp sv 0)
      (prompt-choice :corp 8)
      (is (= 8 (get-counters (refresh sv) :credit)) "8 credits stored on Sealed Vault")
      (is (= 0 (:credit (get-corp))))
      (card-ability state :corp sv 1)
      (prompt-choice :corp 8)
      (is (= 0 (get-counters (refresh sv) :credit)) "Credits removed from Sealed Vault")
      (is (= 8 (:credit (get-corp))))
      (is (= 0 (:click (get-corp))) "Spent a click")
      (card-ability state :corp sv 0)
      (prompt-choice :corp 7)
      (is (= 7 (get-counters (refresh sv) :credit)) "7 credits stored on Sealed Vault")
      (is (= 0 (:credit (get-corp))))
      (card-ability state :corp sv 2)
      (prompt-choice :corp 7)
      (is (= 7 (:credit (get-corp))))
      (is (= 2 (count (:discard (get-corp)))) "Sealed Vault trashed"))))

(deftest server-diagnostics
  ;; Server Diagnostics - Gain 2c when turn begins; trashed when ICE is installed
  (do-game
    (new-game (default-corp [(qty "Server Diagnostics" 1) (qty "Pup" 1)
                             (qty "Launch Campaign" 1)])
              (default-runner))
    (play-from-hand state :corp "Server Diagnostics" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (is (= 1 (count (get-content state :remote1))) "Non-ICE install didn't trash Serv Diag")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 5 (:credit (get-corp))) "Gained 2c at start of turn")
    (play-from-hand state :corp "Pup" "HQ")
    (is (= 1 (count (:discard (get-corp)))) "Server Diagnostics trashed by ICE install")))

(deftest shock
  ;; Shock! - do 1 net damage on access
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Shock!" 3)])
                (default-runner))
      (trash-from-hand state :corp "Shock!")
      (play-from-hand state :corp "Shock!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
      (run-empty-server state "Archives")
      (is (= 1 (count (:hand (get-runner)))) "Runner took 1 net damage")))
  (testing "ensure :access flag is cleared on run end. Issue #2319"
    (do-game
      (new-game (default-corp [(qty "Shock!" 3) (qty "Chairman Hiro" 1)])
                (default-runner))
      (trash-from-hand state :corp "Shock!")
      (play-from-hand state :corp "Shock!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
      (is (not (:run @state)) "Run is complete")
      (trash-from-hand state :corp "Chairman Hiro")
      (is (= 2 (count (:discard (get-corp)))) "Hiro and Shock still in archives")
      (is (= 0 (count (:scored (get-runner)))) "Hiro not scored by Runner"))))

(deftest snare
  (testing "Basic test"
    ;; pay 4 on access, and do 3 net damage and give 1 tag
    (do-game
      (new-game (default-corp [(qty "Snare!" 3)])
                (default-runner))
      (play-from-hand state :corp "Snare!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :corp "Yes")
      (is (= 3 (:credit (get-corp))) "Corp had 7 and paid 4 for Snare! 1 left")
      (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
      (is (= 0 (count (:hand (get-runner)))) "Runner took 3 net damage")))
  (testing "Can't afford"
    (do-game
      (new-game (default-corp [(qty "Snare!" 1)])
                (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-from-hand state :corp "Snare!" "New remote")
      (take-credits state :corp)
      (core/lose state :corp :credit 7)
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :corp "Yes")
      (is (= 0 (:tag (get-runner))) "Runner has 0 tags")
      (prompt-choice-partial :runner "Pay")
      (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
      (is (= 0 (count (:discard (get-runner)))) "Runner took no damage")))
  (testing "with Dedicated Response Team"
    (do-game
      (new-game (default-corp [(qty "Snare!" 1) (qty "Dedicated Response Team" 1)])
                (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-from-hand state :corp "Snare!" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/gain state :corp :click 1 :credit 4)
      (let [drt (get-content state :remote2 0)]
        (take-credits state :corp)
        (run-on state "Server 1")
        (core/rez state :corp drt)
        (run-successful state)
        (is (= :waiting (-> @state :runner :prompt first :prompt-type))
            "Runner has prompt to wait for Snare!")
        (prompt-choice :corp "Yes")
        (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
        (prompt-choice-partial :runner "Pay")
        (is (= 5 (count (:discard (get-runner)))) "Runner took 5 damage")))))

(deftest space-camp
  (testing "when in Archives. #1929"
    (do-game
      (new-game (default-corp [(qty "Space Camp" 1) (qty "News Team" 1) (qty "Breaking News" 1)])
                (default-runner))
      (trash-from-hand state :corp "Space Camp")
      (trash-from-hand state :corp "News Team")
      (play-from-hand state :corp "Breaking News" "New remote")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (prompt-choice :runner "News Team")
      (prompt-choice :runner "Take 2 tags")
      (prompt-choice :runner "Space Camp")
      (prompt-choice :corp "Yes")
      (prompt-select :corp (get-content state :remote1 0))
      (is (= 1 (:advance-counter (get-content state :remote1 0))) "Agenda advanced once from Space Camp")
      (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
      (is (not (:run @state)) "Run completed"))))

(deftest student-loans
  ;; Student Loans - costs Runner 2c extra to play event if already same one in discard
  (do-game
    (new-game (default-corp [(qty "Student Loans" 1) (qty "Hedge Fund" 2)])
              (default-runner))
    (core/gain state :corp :credit 2)
    (play-from-hand state :corp "Student Loans" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 5 (:credit (get-corp))) "Corp has 5c")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 9 (:credit (get-corp))) "Corp has 9c - no penalty from Student Loans")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Corp has 13c - no penalty from Student Loans")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (is (= 9 (:credit (get-runner))) "1st Gamble played for 4c")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 11 (:credit (get-runner))) "2nd Gamble played for 2c")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 13 (:credit (get-runner))) "3rd Gamble played for 2c")))

(deftest sundew
  ;; Sundew
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Sundew" 1)])
                (default-runner))
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp sund)
        (take-credits state :corp 2)
        (is (= 5 (:credit (get-corp))) "Cost 2cr to rez")
        ;; spend a click not on a run
        (take-credits state :runner)
        (is (= 7 (:credit (get-corp))) "Corp gained 2cr from Sundew")
        (take-credits state :corp)
        (run-on state "Server 1")
        (is (= 10 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew")
        (is (= 3 (:click (get-runner))) "Runner spent 1 click to start run"))))
  ; (testing "Sundew - Dirty Laundry"
  ;   (do-game
  ;     (new-game (default-corp [(qty "Sundew" 1)])
  ;               (default-runner [(qty "Dirty Laundry" 1)]))
  ;     (play-from-hand state :corp "Sundew" "New remote")
  ;     (let [sund (get-content state :remote1 0)]
  ;       (core/rez state :corp (refresh sund))
  ;       (is (= 3 (:credit (get-corp))) "Cost 2cr to rez")
  ;       (take-credits state :corp)
  ;       (play-from-hand state :runner "Dirty Laundry")
  ;       (prompt-choice :runner "Server 1")
  ;       ;; spend a click on a run through a card, not through click-run
  ;       ; (prn (-> (refresh sund) :zone rest butlast first))
  ;       ; (prn (-> @state :run :server first))
  ;       (prn "let")
  ;       (let [s (-> (refresh sund) :zone rest butlast)
  ;             r (-> @state :run :server)]
  ;         (prn (and (= (first r) (first s))
  ;                   (= (last r) (last s)))))
  ;       (is (= 5 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew"))))
  )

(deftest team-sponsorship
  ;; Team Sponsorship
  (testing "Install from HQ"
    (do-game
      (new-game (default-corp [(qty "Domestic Sleepers" 1)
                               (qty "Team Sponsorship" 1)
                               (qty "Adonis Campaign" 1)])
                (default-runner))
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :corp tsp)
        (score-agenda state :corp ag1)
        (prompt-select :corp (find-card "Adonis Campaign" (:hand (get-corp))))
        (prompt-choice :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:hand (get-corp)))) "No Adonis in hand"))))
  (testing "Install from Archives"
    (do-game
      (new-game (default-corp [(qty "Domestic Sleepers" 1)
                               (qty "Team Sponsorship" 1)
                               (qty "Adonis Campaign" 1)])
                (default-runner))
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (trash-from-hand state :corp "Adonis Campaign")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :corp tsp)
        (score-agenda state :corp ag1)
        (prompt-select :corp (find-card "Adonis Campaign" (:discard (get-corp))))
        (prompt-choice :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:discard (get-corp)))) "No Adonis in discard"))))
  (testing "Multiple installs"
    (do-game
      (new-game (default-corp [(qty "Domestic Sleepers" 1)
                               (qty "Team Sponsorship" 2)
                               (qty "Adonis Campaign" 2)])
                (default-runner))
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (trash-from-hand state :corp "Adonis Campaign")
      (let [ag1 (get-content state :remote3 0)
            tsp2 (get-content state :remote2 0)
            tsp1 (get-content state :remote1 0)]
        (core/rez state :corp tsp1)
        (core/rez state :corp tsp2)
        (score-agenda state :corp ag1)
        (prompt-choice :corp "Team Sponsorship")
        (prompt-select :corp (find-card "Adonis Campaign" (:discard (get-corp))))
        (prompt-choice :corp "New remote")
        (prompt-select :corp (find-card "Adonis Campaign" (:hand (get-corp))))
        (prompt-choice :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote4 0)))
            "Adonis installed by Team Sponsorship")
        (is (= "Adonis Campaign" (:title (get-content state :remote5 0)))
            "Adonis installed by Team Sponsorship"))))
  (testing "Score 5 points in one window"
    (do-game
      (new-game (default-corp [(qty "AstroScript Pilot Program" 3)
                               (qty "Team Sponsorship" 1)
                               (qty "Breaking News" 1)
                               (qty "SanSan City Grid" 1)])
                (default-runner))
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (core/gain state :corp :credit 100 :click 5)
      (core/rez state :corp (get-content state :remote1 0))
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (play-from-hand state :corp "AstroScript Pilot Program" "Server 1")
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (core/rez state :corp (get-content state :remote3 0))
      (score-agenda state :corp (get-content state :remote1 1))
      (prompt-select :corp (find-card "AstroScript Pilot Program" (:hand (get-corp))))
      (is (= 0 (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript not resolved yet")
      (prompt-choice :corp "Server 1")
      (is (= 1 (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript resolved")
      (card-ability state :corp (first (:scored (get-corp))) 0)
      (prompt-select :corp (get-content state :remote1 1))
      (card-ability state :corp (second (:scored (get-corp))) 0)
      (prompt-select :corp (get-content state :remote1 1))
      (core/score state :corp {:card (get-content state :remote1 1)})
      (prompt-select :corp (find-card "Breaking News" (:hand (get-corp))))
      (prompt-choice :corp "Server 1")
      (card-ability state :corp (second (next (:scored (get-corp)))) 0)
      (prompt-select :corp (get-content state :remote1 1))
      (core/score state :corp {:card (get-content state :remote1 1)})
      (prompt-choice :corp "Done")
      (is (= 7 (:agenda-point (get-corp))) "Scored 5 points in one turn"))))

(deftest technoco
  ;; TechnoCo - Increase program / hardware / virtual cost by 1 and gain 1 when they are installed
  (do-game
    (new-game (default-corp [(qty "TechnoCo" 1)])
              (default-runner [(qty "Misdirection" 1)       ;; 0 cost program
                               (qty "Bookmark" 1)           ;; 0 cost hardware
                               (qty "Ice Analyzer" 1)       ;; 0 cost virtual resource
                               (qty "Fall Guy" 1)]))        ;; 0 cost non-virtual resource
    (play-from-hand state :corp "TechnoCo" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (is (= 5 (:credit (get-corp))) "Corp at 5 credits")
    (is (= 5 (:credit (get-runner))) "Runner at 5 credits")
    (play-from-hand state :runner "Misdirection")
    (is (= 6 (:credit (get-corp))) "Corp gained a credit")
    (is (= 4 (:credit (get-runner))) "Runner spent an extra credit")
    (play-from-hand state :runner "Bookmark")
    (is (= 7 (:credit (get-corp))) "Corp gained a credit")
    (is (= 3 (:credit (get-runner))) "Runner spent an extra credit")
    (play-from-hand state :runner "Ice Analyzer")
    (is (= 8 (:credit (get-corp))) "Corp gained a credit")
    (is (= 2 (:credit (get-runner))) "Runner spent an extra credit")
    (play-from-hand state :runner "Fall Guy")
    (is (= 8 (:credit (get-corp))) "Corp did not gain a credit")
    (is (= 2 (:credit (get-runner))) "Runner did not spend an extra credit")))

(deftest tenma-line
  ;; Tenma Line - Swap 2 pieces of installed ICE
  (do-game
    (new-game (default-corp [(qty "Tenma Line" 1) (qty "Harvester" 1)
                             (qty "Aimor" 1) (qty "Lockdown" 1)])
              (default-runner))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Tenma Line" "New remote")
    (play-from-hand state :corp "Harvester" "HQ")
    (play-from-hand state :corp "Aimor" "HQ")
    (play-from-hand state :corp "Lockdown" "R&D")
    (core/rez state :corp (get-content state :rd 0))
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 1 (:click (get-corp))))
    (card-ability state :corp (get-content state :remote1 0) 0)
    (prompt-select :corp (get-ice state :rd 0))
    (prompt-select :corp (get-ice state :hq 1))
    (is (empty? (:prompt (get-corp))))
    (is (= 0 (:click (get-corp))) "Spent 1 click")
    (is (= "Aimor" (:title (get-ice state :rd 0))) "Aimor swapped to R&D")
    (is (= "Lockdown" (:title (get-ice state :hq 1))) "Lockdown swapped to HQ outer position")))

(deftest the-board
  ;; The Board
  (testing "Modify everything in the score area (regression test for #1938)"
    (do-game
      (new-game (default-corp [(qty "The Board" 1)
                               (qty "News Team" 1)
                               (qty "Firmware Updates" 2)])
                (default-runner [(qty "Artist Colony" 3)
                                 (qty "Fan Site" 3)]))
      (play-from-hand state :corp "The Board" "New remote")
      (play-from-hand state :corp "News Team" "New remote")
      (play-from-hand state :corp "Firmware Updates" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Artist Colony")
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Firmware Updates" "New remote")
      (score-agenda state :corp (get-content state :remote4 0))
      (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")
      (is (= 0 (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (take-credits state :corp)
      (run-empty-server state :remote3)
      (prompt-choice :runner "Steal")
      (is (= 2 (count (:scored (get-runner)))) "Firmware Updates stolen")
      (is (= 1 (:agenda-point (get-runner))) "Runner has 1 agenda point")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= -1 (:agenda-point (get-runner))) "Runner has -1 agenda points")
      (run-empty-server state :remote2)
      (prompt-choice :runner "Add News Team to score area")
      (is (= 3 (count (:scored (get-runner)))) "News Team added to Runner score area")
      (is (= -3 (:agenda-point (get-runner))) "Runner has -3 agenda points")
      (card-ability state :runner (get-resource state 0) 0)
      (prompt-choice :runner (->> @state :runner :prompt first :choices first))
      (prompt-select :runner (first (:scored (get-runner))))
      (is (= 2 (count (:scored (get-runner)))) "Fan Site removed from Runner score area")
      (is (= -2 (:agenda-point (get-runner))) "Runner has -2 agenda points")
      (run-empty-server state :remote1)
      (prompt-choice-partial :runner "Pay")
      (is (= 3 (count (:scored (get-runner)))) "The Board added to Runner score area")
      (is (= 2 (:agenda-point (get-runner))) "Runner has 2 agenda points")))
  (testing "handle Fifteen Minutes clicked out of Runner's score area"
    (do-game
      (new-game (default-corp [(qty "The Board" 1)
                               (qty "15 Minutes" 1)])
                (default-runner))
      (play-from-hand state :corp "The Board" "New remote")
      (play-from-hand state :corp "15 Minutes" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (is (= 0 (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (run-empty-server state :remote2)
      (prompt-choice-partial :runner "Steal")
      (is (= 0 (:agenda-point (get-runner))) "Runner stays at 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner has 1 agenda in scored area")
      (take-credits state :runner)
      (let [fifm (first (:scored (get-runner)))]
        (card-ability state :corp (refresh fifm) 0)
        (is (= 0 (:agenda-point (get-runner))) "Runner drops to 0 agenda points")
        (is (empty? (:scored (get-runner))) "Runner has no agendas in scored area"))))
  (testing "Corp scoring agenda shouldn't trigger The Board to lower Runner points"
    (do-game
      (new-game (default-corp [(qty "The Board" 1)
                               (qty "Hostile Takeover" 2)])
                (default-runner))
      (core/gain state :corp :credit 6)
      (play-from-hand state :corp "The Board" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (is (= 0 (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (run-empty-server state :remote3)
      (prompt-choice-partial :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner has 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner has 1 agenda in scored area")
      (take-credits state :runner)
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 0 (:agenda-point (get-runner))) "Runner loses 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner still has 1 agenda in scored area")
      (score-agenda state :corp (get-content state :remote2 0))
      (is (= 0 (:agenda-point (get-runner))) "Runner still has 0 agenda points")
      (is (= 1 (count (:scored (get-runner)))) "Runner still has 1 agenda in scored area")))
  (testing "Scoring two copies should be 4 agenda points"
    (do-game
      (new-game (default-corp [(qty "The Board" 2)])
                (default-runner))
      (core/gain state :corp :credit 6)
      (play-from-hand state :corp "The Board" "New remote")
      (play-from-hand state :corp "The Board" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 14)
      (is (= 0 (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (is (empty? (:scored (get-runner))) "Runner has no agendas")
      (run-empty-server state :remote2)
      (prompt-choice-partial :runner "Pay")
      (is (= 1 (:agenda-point (get-runner))) "Runner has 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner has 1 agenda in scored area")
      (run-empty-server state :remote1)
      (prompt-choice-partial :runner "Pay")
      (is (= 4 (:agenda-point (get-runner))) "Runner has 4 agenda points")
      (is (= 2 (count (:scored (get-runner)))) "Runner has 2 agendas in scored area"))))

(deftest the-root
  ;; The Root - recurring credits refill at Step 1.2
  (do-game
    (new-game (make-deck "Blue Sun: Powering the Future" [(qty "The Root" 1)])
              (default-runner))
    (play-from-hand state :corp "The Root" "New remote")
    (core/gain state :corp :credit 6)
    (let [root (get-content state :remote1 0)]
      (core/rez state :corp root)
      (card-ability state :corp (refresh root) 0)
      (is (= 2 (:rec-counter (refresh root))) "Took 1 credit from The Root")
       (is (= 6 (:credit (get-corp))) "Corp took Root credit into credit pool")
      (take-credits state :corp)
      (take-credits state :runner)
      ;; we expect Step 1.2 to have triggered because of Blue Sun
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (is (= 3 (:rec-counter (refresh root))) "Recurring credits were refilled before Step 1.2 window"))))

(deftest toshiyuki-sakai
  ;; Toshiyuki Sakai - Swap with an asset/agenda from HQ; Runner can choose to access new card or not
  (do-game
    (new-game (default-corp [(qty "Toshiyuki Sakai" 1) (qty "Project Junebug" 1) (qty "Hedge Fund" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    (play-from-hand state :corp "Toshiyuki Sakai" "New remote")
    (let [toshi (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh toshi)})
      (core/advance state :corp {:card (refresh toshi)})
      (take-credits state :corp)
      (is (= 2 (:advance-counter (refresh toshi))) "Toshiyuki has 2 advancements")
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Toshiyuki")
      (prompt-choice :corp "Yes") ; choose to do a swap
      (prompt-select :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= (refresh toshi) (get-content state :remote1 0)) "Toshiyuki still in remote; can't target an operation in hand")
      (prompt-select :corp (find-card "Project Junebug" (:hand (get-corp))))
      (let [june (get-content state :remote1 0)]
        (is (= "Project Junebug" (:title (refresh june))) "Project Junebug swapped into Server 1")
        (is (= 2 (:advance-counter (refresh june))) "Project Junebug has 2 advancements")
        (prompt-choice :runner "Yes") ; choose to access new card
        (prompt-choice :corp "Yes") ; pay 1c to fire Junebug
        (is (= 4 (count (:discard (get-runner)))) "Runner took 4 net damage")))))

(deftest turtlebacks
  ;; Turtlebacks - Gain 1 credit for every new server created
  (do-game
    (new-game (default-corp [(qty "Turtlebacks" 1) (qty "PAD Campaign" 2) (qty "Wraparound" 1)])
              (default-runner))
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Turtlebacks" "New remote")
    (let [tb (get-content state :remote1 0)]
      (core/rez state :corp tb)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit for new server created")
      (play-from-hand state :corp "Wraparound" "Server 1")
      (is (= 4 (:credit (get-corp))) "No credit gained for install into existing server")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit for new server created"))))

(deftest urban-renewal
  ;; Urban renewal meat damage
  (do-game
    (new-game (default-corp [(qty "Urban Renewal" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    ;; Corp turn 1, install and rez urban renewal
    (play-from-hand state :corp "Urban Renewal" "New remote")
    (let [ur (get-content state :remote1 0)]
      (core/rez state :corp (refresh ur))
      (take-credits state :corp)
      ;; Runner turn 1, creds
      (is (= 3 (get-counters (refresh ur) :power)))
      (take-credits state :runner)
      ;; Corp turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :corp)
      ;; Runner turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :runner)
      ;; Corp turn 3
      (is (= 1 (get-counters (refresh ur) :power)))
      (take-credits state :corp)
      ;; Runner turn 3
      (is (= 0 (count (:discard (get-corp)))) "Nothing in Corp trash")
      (is (= 0 (count (:discard (get-runner)))) "Nothing in Runner trash")
      (take-credits state :runner)
      ;; Corp turn 4 - damage fires
      (is (= 1 (count (:discard (get-corp)))) "Urban Renewal got trashed")
      (is (= 4 (count (:discard (get-runner)))) "Urban Renewal did 4 meat damage"))))

(deftest warden-fatuma
  ;; Warden Fatuma - rezzed bioroid ice gains an additional sub
  (do-game
    (new-game (default-corp [(qty "Warden Fatuma" 1) (qty "Kakugo" 1)
                             (qty "Eli 2.0" 1) (qty "Ichi 2.0" 1)])
              (default-runner))
    (core/gain state :corp :credit 20 :click 5)
    (play-from-hand state :corp "Kakugo" "Archives")
    (play-from-hand state :corp "Eli 2.0" "HQ")
    (play-from-hand state :corp "Ichi 2.0" "R&D")
    (play-from-hand state :corp "Warden Fatuma" "New remote")
    (let [wf (get-content state :remote1 0)
          kak (get-ice state :archives 0)
          eli (get-ice state :hq 0)
          ichi (get-ice state :rd 0)]
      (core/rez state :corp kak)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo starts with 1 sub")
      (core/rez state :corp eli)
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 starts with 2 subs")
      (is (= 0 (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 starts with 0 subs")
      (core/rez state :corp wf)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 gains 1 sub")
      (is (= 0 (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 stays at 0 subs")
      (core/rez state :corp ichi)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh ichi)))) "Ichi 2.0 rezzes with 3 subs")
      (core/derez state :corp (refresh wf))
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 reverts")
      (is (= 2 (count (:subroutines (refresh ichi)))) "Ichi 2.0 reverts"))))

(deftest watchdog
  ;; Watchdog - Reduce rez cost of first ICE per turn by number of Runner tags
  (do-game
    (new-game (default-corp [(qty "Watchdog" 1) (qty "Architect" 1) (qty "Wraparound" 1)])
              (default-runner))
    (play-from-hand state :corp "Watchdog" "New remote")
    (play-from-hand state :corp "Wraparound" "HQ")
    (play-from-hand state :corp "Architect" "HQ")
    (let [wd (get-content state :remote1 0)
          arch (get-ice state :hq 1)
          wrap (get-ice state :hq 0)]
      (take-credits state :corp)
      (is (= 4 (:credit (get-corp))))
      (core/gain state :runner :tag 2)
      (run-on state "HQ")
      (core/rez state :corp wd)
      (core/rez state :corp arch)
      (is (= 2 (:credit (get-corp))) "Only 2 credits to rez Architect")
      (core/rez state :corp wrap)
      (is (= 0 (:credit (get-corp))) "No rez discount on Wraparound"))))

(deftest whampoa-reclamation
  ;; Whampoa Reclamation: Enable trashing a card from HQ to place a card in Archives on the bottom of R+D
  (do-game
    (new-game (default-corp [(qty "Whampoa Reclamation" 3)
                             (qty "PAD Campaign" 2)
                             (qty "Global Food Initiative" 3)])
              (default-runner))
    (play-from-hand state :corp "Whampoa Reclamation" "New remote")
    (let [wr (get-content state :remote1 0)]
      (core/draw state :corp)
      (take-credits state :corp)
      (core/rez state :corp wr)
      (let [gfi (find-card "Global Food Initiative" (:hand (get-corp)))]
        (core/trash state :runner gfi)
        (card-ability state :corp wr 0)
        (prompt-choice :corp "Global Food Initiative") ;; into archives
        (prompt-select :corp (first (:discard (get-corp)))) ;; into R&D
        (is (= 0 (count (:discard (get-corp)))) "Only card in discard placed in bottom of R&D")
        (is (= "Global Food Initiative" (:title (last (:deck (get-corp))))) "GFI last card in deck")))))
