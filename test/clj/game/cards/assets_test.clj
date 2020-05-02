(ns game.cards.assets-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest adonis-campaign
  ;; Adonis Campaign
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (let [ac (get-content state :remote1 0)]
        (core/rez state :corp ac)
        (is (= 1 (:credit (get-corp))))
        (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
        (take-credits state :corp)
        (let [credits (:credit (get-corp))
              counters (get-counters (refresh ac) :credit)]
          (take-credits state :runner)
          (is (= (+ credits 3) (:credit (get-corp))) "Gain 3 from Adonis")
          (is (= (- counters 3) (get-counters (refresh ac) :credit)) "9 counter remaining on Adonis"))
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 6 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
        (take-credits state :runner)
        (is (nil? (refresh ac)) "Adonis Campaign should be trashed")
        (is (= "Adonis Campaign" (->> (get-corp) :discard first :title))))))
  (testing "With Gravedigger, async issues"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Adonis Campaign"]}
                 :runner {:hand ["Gravedigger"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (let [ac (get-content state :remote1 0)]
        (core/rez state :corp ac)
        (is (= 1 (:credit (get-corp))))
        (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
        (take-credits state :corp)
        (play-from-hand state :runner "Gravedigger")
        (let [credits (:credit (get-corp))
              counters (get-counters (refresh ac) :credit)]
          (take-credits state :runner)
          (is (= (+ credits 3) (:credit (get-corp))) "Gain 3 from Adonis")
          (is (= (- counters 3) (get-counters (refresh ac) :credit)) "9 counter remaining on Adonis"))
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 6 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
        (take-credits state :runner)
        (is (nil? (refresh ac)) "Adonis Campaign should be trashed")
        (is (= "Adonis Campaign" (->> (get-corp) :discard first :title)))
        (is (= 1 (get-counters (get-program state 0) :virus)))))))

(deftest advanced-assembly-lines
  ;; Advanced Assembly Lines
  (do-game
    (new-game {:corp {:deck ["Advanced Assembly Lines"
                             "PAD Campaign"]}})
    (play-from-hand state :corp "Advanced Assembly Lines" "New remote")
    (let [aal (get-content state :remote1 0)
          credits (:credit (get-corp))
          hq (count (:hand (get-corp)))]
      (core/rez state :corp aal)
      (is (= (+ credits 2) (:credit (get-corp))) "Spend 1 gain 3")
      (card-ability state :corp aal 0)
      (click-card state :corp (find-card "PAD Campaign" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (is (= (dec hq) (count (:hand (get-corp)))) "Installed 1 card, hq is empty"))))

(deftest aggressive-secretary
  ;; Aggressive Secretary
  (do-game
    (new-game {:corp {:deck ["Aggressive Secretary"]}
               :runner {:deck [(qty "Cache" 3)]}})
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
      (click-prompt state :corp "Yes")
      ;; Corp can trash one program
      (click-card state :corp (get-program state 1))
      ;; There should be two Caches left
      (is (= 3 (:credit (get-corp))))
      (is (= 2 (count (get-program state)))))))

(deftest alexa-belsky
  ;; Alexa Belsky
  (do-game
    (new-game {:corp {:deck ["Alexa Belsky" "Hedge Fund" "Breaking News"
                             "Gutenberg" "Product Placement" "Jackson Howard"]}})
    (play-from-hand state :corp "Alexa Belsky" "New remote")
    (let [alexa (get-content state :remote1 0)]
      (core/rez state :corp alexa)
      (card-ability state :corp alexa 0)
      (is (= 1 (count (:discard (get-corp)))) "Alexa Belsky trashed")
      (is (= 5 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))
      (click-prompt state :runner "5") ;Runner chooses to pay 5 credits so 2 cards are prevented from being shuffled
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 3 (count (:deck (get-corp)))))
      (is (zero? (:credit (get-runner)))))))

(deftest alix-t4lb07
  ;; Alix T4LB07
  (do-game
    (new-game {:corp {:deck ["Alix T4LB07" (qty "PAD Campaign" 3)]}})
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
    (new-game {:corp {:deck ["Allele Repression"]}})
    (play-from-hand state :corp "Allele Repression" "New remote")
    (let [ar (get-content state :remote1 0)]
      (core/advance state :corp (refresh ar))
      (core/advance state :corp (refresh ar))
      (core/rez state :corp ar)
      (card-ability state :corp ar 0)
      (is (= 1 (count (:discard (get-corp)))) "Allele Repression is trashed"))))

(deftest amani-senai
  ;; Amani Senai - trace on score/steal to bounce, with base strength = advancement req of the agenda
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Amani Senai"
                               (qty "Medical Breakthrough" 2)]}
                 :runner {:deck ["Analog Dreamers"]}})
      (play-from-hand state :corp "Amani Senai" "New remote")
      (play-from-hand state :corp "Medical Breakthrough" "New remote")
      (play-from-hand state :corp "Medical Breakthrough" "New remote")
      (take-credits state :corp)
      (let [senai (get-content state :remote1 0)
            breakthrough (get-content state :remote3 0)]
        (core/rez state :corp senai)
        (play-from-hand state :runner "Analog Dreamers")
        (run-empty-server state "Server 2")
        (click-prompt state :runner "Steal")
        (is (zero? (count (get-content state :remote2))) "Agenda was stolen")
        (click-prompt state :corp "Medical Breakthrough") ; simult. effect resolution
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "0")
        (is (= 3 (:strength (prompt-map :runner))) "Trace base strength is 3 after stealing first Breakthrough")
        (click-prompt state :runner "0")
        (let [grip (-> (get-runner) :hand count)]
          (is (= 1 (count (get-program state))) "There is an Analog Dreamers installed")
          (click-card state :corp (get-program state 0))
          (is (zero? (count (get-program state))) "Analog Dreamers was uninstalled")
          (is (= (inc grip) (-> (get-runner) :hand count)) "Analog Dreamers was added to hand"))
        (take-credits state :runner)
        (score-agenda state :corp breakthrough)
        ;; (click-prompt state :corp "Medical Breakthrough") ; there is no simult. effect resolution on score for some reason
        (click-prompt state :corp "Yes") ; corp should get to trigger trace even when no runner cards are installed
        (click-prompt state :corp "0")
        (is (= 2 (:strength (prompt-map :runner))) "Trace base strength is 2 after scoring second Breakthrough"))))
  (testing "with Team Sponsorship"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Domestic Sleepers"
                               "Amani Senai"
                               "Team Sponsorship"]
                        :discard ["Adonis Campaign"]}
                 :runner {:hand ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (take-credits state :runner)
      (play-from-hand state :corp "Amani Senai" "New remote")
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (let [amani (get-content state :remote1 0)
            tsp (get-content state :remote2 0)
            sleepers (get-content state :remote3 0)
            corroder (get-program state 0)]
        (core/rez state :corp amani)
        (core/rez state :corp tsp)
        (score-agenda state :corp sleepers)
        (click-prompt state :corp "Amani Senai")
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "3")
        (click-prompt state :runner "0")
        (click-card state :corp "Corroder")
        (is (nil? (refresh corroder)))
        (click-card state :corp "Adonis Campaign")
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote4 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:discard (get-corp)))) "No Adonis in discard"))))
  (testing "with Gang Sign and Leela. Issue #4487"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Amani Senai" "Hostile Takeover" "Hostile Takeover"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :hand ["Gang Sign"]}})
      (play-from-hand state :corp "Amani Senai" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (click-prompt state :corp "Hostile Takeover")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp "Gang Sign")
      (click-prompt state :runner "Done") ; Leela trigger, no Gang Sign prompt
      (is (empty? (:prompt (get-runner))) "Runner doesn't get an access prompt"))))

(deftest anson-rose
  ;; Anson Rose
  (do-game
    (new-game {:corp {:deck ["Anson Rose" "Ice Wall"]}})
    (play-from-hand state :corp "Anson Rose" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ar (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp (refresh ar))
      (is (zero? (get-counters (refresh ar) :advancement)) "Anson Rose should start with 0 advancement counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 1 (get-counters (refresh ar) :advancement)) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should still have 0 counters so far")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 2 (get-counters (refresh ar) :advancement)) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should still have 0 counters so far")
      (core/rez state :corp (refresh iw))
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "2")
      (is (zero? (get-counters (refresh ar) :advancement)) "Anson Rose should lose all advancement counters")
      (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement counter"))))

(deftest api-s-keeper-isobel
  ;; API-S Keeper Isobel
  (do-game
    (new-game {:corp {:deck ["API-S Keeper Isobel" "Ice Wall"]}})
    (play-from-hand state :corp "API-S Keeper Isobel" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ap (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp (refresh ap))
      (core/rez state :corp (refresh iw))
      (core/advance state :corp {:card (refresh iw)})
      (is (= 1 (get-counters (refresh iw) :advancement)) "Ice Wall has 1 advancement token")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (:credit (get-corp))) "Corp starts with 1 credits")
      (card-ability state :corp (refresh ap) 0)
      (click-card state :corp (refresh iw))
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall loses an advancement token")
      (is (= 4 (:credit (get-corp))) "Corp gains 3 credits"))))

(deftest aryabhata-tech
  ;; Aryabhata Tech
  (testing "Credit gain and loss"
    (do-game
      (new-game {:corp {:deck ["Aryabhata Tech"
                               "Hunter"]}})
      (play-from-hand state :corp "Aryabhata Tech" "New remote")
      (play-from-hand state :corp "Hunter" "HQ")
      (let [at (get-content state :remote1 0)
            h (get-ice state :hq 0)]
        (core/rez state :corp (refresh at))
        (core/rez state :corp (refresh h))
        (take-credits state :corp)
        (run-on state :hq)
        (run-continue state)
        (let [c-credits (:credit (get-corp))
              r-credits (:credit (get-runner))]
          (card-subroutine state :corp h 0)
          (click-prompt state :corp "0")
          (click-prompt state :runner "0")
          (is (= 1 (- (:credit (get-corp)) c-credits)))
          (is (= -1 (- (:credit (get-runner)) r-credits)))))))
  (testing "Interaction with trash effects like CtM (issue #2541)"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Aryabhata Tech"]}})
      (play-from-hand state :corp "Aryabhata Tech" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (changes-val-macro
        0 (:credit (get-runner))
        "Runner loses no additional credits from successful trace"
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")))))

(deftest bass-ch1r180g4
  (do-game
    (new-game {:corp {:deck ["Bass CH1R180G4"]}})
    (play-from-hand state :corp "Bass CH1R180G4" "New remote")
    (let [bass (get-content state :remote1 0)]
      (core/rez state :corp bass)
      (is (= 2 (:credit (get-corp))))
      (card-ability state :corp bass 0)
      (is (= 3 (:click (get-corp))))
      (is (nil? (refresh bass)) "Bass CH1R180G4 should be trashed"))))

(deftest bio-ethics-association
  ;; Bio-Ethics Association
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Bio-Ethics Association"]}})
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))))))
  (testing "should be able to prevent damage from multiple copies"
    (do-game
      (new-game {:corp {:deck [(qty "Bio-Ethics Association" 2)]}
                 :runner {:deck ["Feedback Filter" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (take-credits state :runner)
      (let [ff (get-hardware state 0)]
        (is (= 2 (count (:prompt (get-runner)))) "Runner has a single damage prevention prompt")
        (card-ability state :runner ff 0)
        (is (zero? (count (:discard (get-runner)))) "Runner prevented damage")
        (is (= 2 (count (:prompt (get-runner)))) "Runner has a next damage prevention prompt")
        (click-prompt state :runner "Done")
        (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")))))

(deftest bioroid-work-crew
  ;; Bioroid Work Crew
  (letfn [(bwc-test [card]
            (do-game
              (new-game {:corp {:deck ["Bioroid Work Crew" card]}})
              (play-from-hand state :corp "Bioroid Work Crew" "New remote")
              (let [bwc (get-content state :remote1 0)]
                (core/rez state :corp bwc)
                (card-ability state :corp bwc 0)
                (click-card state :corp card)
                (if (= "Research Station" card)
                  (click-prompt state :corp "HQ")
                  (click-prompt state :corp "New remote"))
                (is (zero? (count (:hand (get-corp)))))
                (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now"))))]
    (doall (map bwc-test
                ["Hostile Takeover"
                 "Dedicated Response Team"
                 "Builder"
                 "Research Station"]))))

(deftest blacklist
  ;; Blacklist
  (testing "Blocks moving cards from heap #5044"
    (do-game
      (new-game {:corp {:hand ["Blacklist" "Ice Wall"]}
                 :runner {:hand ["Boomerang"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (click-card state :runner "Ice Wall")
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (get-hardware state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (run-successful state)
      (is (= "Shuffle a copy of Boomerang back into the Stack?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (find-card "Boomerang" (:discard (get-runner))))
      (is (not (find-card "Boomerang" (:deck (get-runner)))))))
  (testing "Blocks installing cards from heap"
    (do-game
      (new-game {:corp {:hand ["Blacklist" "Ice Wall"]}
                 :runner {:discard ["Paperclip"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (= "Install Paperclip?" (:msg (prompt-map :runner))))
      (changes-val-macro
        0 (:credit (get-runner))
        "Spend 0 when Blacklist blocks install"
        (click-prompt state :runner "Yes"))
      (is (nil? (get-program state 0)))))
  (testing "Need to allow steal. #2426"
    (do-game
      (new-game {:corp {:deck [(qty "Fetal AI" 3) "Blacklist"]}})
      (trash-from-hand state :corp "Fetal AI")
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 1 (count (:discard (get-corp)))))
      (take-credits state :corp)
      (run-empty-server state :archives)
      (click-prompt state :runner "Pay to steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner has 2 agenda points")
      (is (= 1 (count (:scored (get-runner))))))))

(deftest brain-taping-warehouse
  ;; Brain-Taping Warehouse - Lower rez cost of Bioroid ICE by 1 for each unspent Runner click
  (do-game
    (new-game {:corp {:deck ["Brain-Taping Warehouse" "Ichi 1.0"
                             "Eli 1.0"]}})
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
      (run-continue state)
      (run-jack-out state)
      (run-on state :hq)
      (is (= 2 (:click (get-runner))))
      (core/rez state :corp eli)
      (is (= 1 (:credit (get-corp))) "Paid only 1c to rez Eli; reduction of 2c"))))

(deftest breached-dome
  ;; Breached Dome
  (do-game
    (new-game {:corp {:deck [(qty "Breached Dome" 10)]}
               :runner {:deck [(qty "Sure Gamble" 10)]}})
    (trash-from-hand state :corp "Breached Dome")
    (play-from-hand state :corp "Breached Dome" "New remote")
    (take-credits state :corp)
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (is (= 4 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 4 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 2 (count (:discard (get-runner)))) "Runner's discard grew by 2")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "No action")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 3 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 4 (count (:discard (get-runner)))) "Runner's discard grew by 2")
    (run-empty-server state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 meat damage")
    (is (= 2 (count (:deck (get-runner)))) "Runner milled 1 card")
    (is (= 6 (count (:discard (get-runner)))) "Runner's discard grew by 2")))

(deftest broadcast-square
  ;; Broadcast Square - Trace 3: Prevent all bad publicity
  (do-game
    (new-game {:corp {:deck ["Profiteering" "Hostile Takeover" "Broadcast Square"]}})
    (play-from-hand state :corp "Broadcast Square" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 3 (:credit (get-corp))) "Corp should have spent 2 credits")
    (play-from-hand state :corp "Profiteering" "New remote")
    (score-agenda state :corp (get-content state :remote2 0))
    (click-prompt state :corp "3")  ;; Take 3 bad publicity from Profiteering, gain 15 (if bad publicity actually taken)
    (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
    (click-prompt state :runner "0")  ;; Runner doesn't pump trace; loses trace
    (is (= 1 (:agenda-point (get-corp))) "Corp should score a 1-point agenda")
    (is (zero? (count-bad-pub state)) "Corp should gain 0 bad publicity")
    (is (= 3 (:credit (get-corp))) "Corp should gain 0 credits")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (score-agenda state :corp (get-content state :remote3 0))
    (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
    (click-prompt state :runner "3")  ;; Runner pumps trace; wins trace
    (is (= 2 (:agenda-point (get-corp))) "Corp should score a 1-point agenda")
    (is (= 1 (count-bad-pub state)) "Corp should gain 1 bad publicity from failed trace")
    (is (= 10 (:credit (get-corp))) "Corp should gain 7 credits")))

(deftest c-i-fund
  ;; C.I. Fund
  (do-game
    (new-game {:corp {:deck ["C.I. Fund" "Hedge Fund"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "C.I. Fund" "New remote")
    (take-credits state :corp)
    (let [ci (get-content state :remote1 0)]
      (core/rez state :corp ci)
      (take-credits state :runner)
      (card-ability state :corp ci 0)
      (click-prompt state :corp "3")
      (is (= 3 (get-counters (refresh ci) :credit)))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp ci 0)
      (click-prompt state :corp "3")
      (is (= 6 (get-counters (refresh ci) :credit)))
      (core/end-phase-12 state :corp nil)
      (is (= 8 (get-counters (refresh ci) :credit)))
      (take-credits state :corp)
      (take-credits state :runner)
      (core/end-phase-12 state :corp nil)
      (is (= 10 (get-counters (refresh ci) :credit)))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp ci 1)
        (is (= 8 (- (:credit (get-corp)) credits)))
        (is (zero? (get-counters (refresh ci) :credit)))))))

(deftest calvin-b4l3y
  ;; Calvin B4L3Y
  (do-game
    (new-game {:corp {:hand ["Calvin B4L3Y"]
                      :deck [(qty "Hedge Fund" 3) (qty "IPO" 2)]}})
    (play-from-hand state :corp "Calvin B4L3Y" "New remote")
    (let [cal (get-content state :remote1 0)]
      (let [hand (count (:hand (get-corp)))
            click (:click (get-corp))]
        (core/rez state :corp cal)
        (card-ability state :corp cal 0)
        (is (= (+ hand 2) (count (:hand (get-corp)))) "Drew 2 cards")
        (card-ability state :corp cal 0)
        (is (= (dec click) (:click (get-corp))) "Second use of Calvin in same turn not allowed"))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (let [hand (count (:hand (get-corp)))]
        (click-prompt state :corp "Yes")
        (is (= (+ hand 2) (count (:hand (get-corp)))) "Drew two cards")
        (is (find-card "Calvin B4L3Y" (:discard (get-corp))) "Calvin is in the discard")))))

(deftest capital-investors
  ;; Capital Investors - Click for 2 credits
  (do-game
    (new-game {:corp {:deck ["Capital Investors"]}})
    (play-from-hand state :corp "Capital Investors" "New remote")
    (let [cap (get-content state :remote1 0)]
      (core/rez state :corp cap)
      (card-ability state :corp cap 0)
      (card-ability state :corp cap 0)
      (is (zero? (:click (get-corp))) "Used twice, spent 2 clicks")
      (is (= 7 (:credit (get-corp))) "Used twice, gained 4 credits"))))

(deftest cerebral-overwriter
  ;; Cerebral Overwriter
  (do-game
    (new-game {:corp {:deck ["Cerebral Overwriter"]}})
    (play-from-hand state :corp "Cerebral Overwriter" "New remote")
    (let [co (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh co)})
      (core/advance state :corp {:card (refresh co)})
      (is (= 2 (get-counters (refresh co) :advancement)))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes") ; choose to do the optional ability
      (is (= 2 (:brain-damage (get-runner))) "Runner takes 2 brain damage"))))

(deftest chairman-hiro
  ;; Chairman Hiro - Reduce Runner max hand size; add as 2 agenda points if Runner trashes him
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Chairman Hiro" 2)]}})
      (play-from-hand state :corp "Chairman Hiro" "New remote")
      (play-from-hand state :corp "Chairman Hiro" "Server 1")
      (click-prompt state :corp "OK")
      (is (= 1 (count (:discard (get-corp)))) "First Hiro trashed")
      (is (zero? (:agenda-point (get-runner))) "No points for Runner if trashed by Corp")
      (let [hiro (get-content state :remote1 0)]
        (core/rez state :corp hiro)
        (is (= 3 (hand-size :runner)) "Runner max hand size reduced by 2")
        (take-credits state :corp)
        (take-credits state :runner 3)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 6 [Credits] to trash")
        (is (= 2 (:credit (get-runner))) "Runner paid 6 credits to trash")
        (is (= 5 (hand-size :runner)) "Runner max hand size restored to 5")
        (is (= 1 (count (get-scored state :runner)))
            "Chairman Hiro added to Runner score area")
        (is (= 2 (:agenda-point (get-runner))) "Runner gained 2 agenda points"))))
  (testing "Interaction with Bacterial Programming. Issue #3090"
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chairman Hiro"
                               "DNA Tracker" "Excalibur" "Fire Wall" "Gemini"]
                        :hand ["Bacterial Programming"]}})
      (play-from-hand state :corp "Bacterial Programming" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Chairman Hiro")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Accelerated Beta Test")
      (click-prompt state :corp "Brainstorm")
      (click-prompt state :corp "DNA Tracker")
      (click-prompt state :corp "Excalibur")
      (click-prompt state :corp "Fire Wall")
      (click-prompt state :corp "Gemini")
      (click-prompt state :corp "Done")
      (is (= ["Bacterial Programming"] (mapv :title (get-scored state :runner))) "Runner shouldn't score Chairman Hiro")
      (is (= ["Chairman Hiro"] (mapv :title (:discard (get-corp)))) "Chairman Hiro should be in Archives"))))

(deftest chief-slee
  ;; Chief Slee
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Chief Slee" "Hive"]
                      :credits 10}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Hive" "HQ")
    (play-from-hand state :corp "Chief Slee" "New remote")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [slee (get-content state :remote1 0)
          hive (get-ice state :hq 0)]
      (core/rez state :corp slee)
      (core/rez state :corp hive)
      (run-continue state)
      (fire-subs state hive)
      (is (not (:run @state)) "Run has been ended")
      (take-credits state :runner)
      (card-ability state :corp slee 0)
      (is (= 5 (count (:discard (get-runner)))) "Chief Slee should do 5 meat damage"))))

(deftest city-surveillance
  ;; City Surveillance - Runner chooses to pay 1 credit or take 1 tag at start of their turn
  (do-game
    (new-game {:corp {:deck ["City Surveillance"]}})
    (play-from-hand state :corp "City Surveillance" "New remote")
    (let [surv (get-content state :remote1 0)]
      (core/rez state :corp surv)
      (take-credits state :corp)
      (is (some #{"Pay 1 [Credits]" "Take 1 tag"} (prompt-buttons :runner)))
      (click-prompt state :runner "Pay 1 [Credits]")
      (is (= 4 (:credit (get-runner))) "Runner paid 1 credit")
      (is (zero? (count-tags state)) "Runner didn't take a tag")
      (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner))) ;; Set Runner's credits to 0 so they can't choose to pay
      (take-credits state :corp)
      (is (some #{"Take 1 tag"} (prompt-buttons :runner)))
      (click-prompt state :runner "Take 1 tag")
      (is (zero? (:credit (get-runner))) "Runner paid no credits")
      (is (= 1 (count-tags state)) "Runner took 1 tag"))
      (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")))

(deftest clone-suffrage-movement
  ;; Clone Suffrage Movement
  (do-game
    (new-game {:corp {:deck ["Clone Suffrage Movement" (qty "Hedge Fund" 2) "Ice Wall"]}})
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
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (core/end-phase-12 state :corp nil)
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Clone Suffrage Movement didn't activate cuz of the ice"))))

(deftest clyde-van-rite
  ;; Clyde Van Rite - Multiple scenarios involving Runner not having credits/cards to trash
  (do-game
    (new-game {:corp {:deck ["Clyde Van Rite"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Restructure" 2) (qty "John Masanori" 2)]}})
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
      (is (some #{"Pay 1 [Credits]" "Trash top card"} (prompt-buttons :runner)))
      (click-prompt state :runner "Pay 1 [Credits]")
      (is (= 8 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner can't pay 1 credit so must trash top card
      (core/lose state :runner :credit (:credit (get-runner)))
      (card-ability state :corp clyde 0)
      (is (zero? (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (is (some #{"Trash top card"} (prompt-buttons :runner)))
      (click-prompt state :runner "Trash top card")
      (is (zero? (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner has 1+ card in Stack and chooses to trash 1 card
      (card-ability state :corp clyde 0)
      (is (= 4 (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (is (some #{"Pay 1 [Credits]" "Trash top card"} (prompt-buttons :runner)))
      (click-prompt state :runner "Trash top card")
      (is (= 4 (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner has no cards in Stack so must pay 1 credit
      (card-ability state :corp clyde 0)
      (is (= 8 (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (is (some #{"Pay 1 [Credits]"} (prompt-buttons :runner)))
      (click-prompt state :runner "Pay 1 [Credits]")
      (is (= 7 (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (take-credits state :corp)
      (dotimes [_ 4]
        (core/click-credit state :runner nil))
      (core/lose state :runner :credit (:credit (get-runner)))
      (core/end-turn state :runner nil)
      ;; Runner has no credits and no cards so nothing happens
      (card-ability state :corp clyde 0)
      (is (zero? (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (is (empty? (:prompt (get-corp)))))))

(deftest commercial-bankers-group
  ;; Commercial Bankers Group - Gain 3 credits at turn start if unprotected by ice
  (do-game
    (new-game {:corp {:deck ["Commercial Bankers Group" "Ice Wall"]}})
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
      (new-game {:corp {:deck ["Constellation Protocol" "Ice Wall" "Fire Wall"]}})
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
        (is (= 1 (get-counters (refresh iw) :advancement)))
        (is (= 1 (get-counters (refresh fw) :advancement)))
        (click-card state :corp (refresh iw))
        (click-card state :corp (refresh fw))
        (is (zero? (get-counters (refresh iw) :advancement)))
        (is (= 2 (get-counters (refresh fw) :advancement)))
        (core/end-phase-12 state :corp nil)
        (is (empty? (:prompt (get-runner)))))))
  (testing "Variable number of advanceable cards"
    (do-game
      (new-game {:corp {:deck ["Constellation Protocol" "Ice Wall" "Hive"]}})
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
      (new-game {:corp {:deck ["Constellation Protocol" "Ice Wall" "Contract Killer"]}})
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
    (new-game {:corp {:deck ["Contract Killer"]}
               :runner {:deck [(qty "Sure Gamble" 2) "Data Dealer"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Contract Killer" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Data Dealer")
    (take-credits state :runner)
    (let [ck (get-content state :remote1 0)]
      (advance state ck 2)
      (core/rez state :corp ck)
      (card-ability state :corp ck 0)
      (click-card state :corp (get-resource state 0))
      (is (= 1 (-> (get-corp) :discard count)) "Contract Killer should be trashed as an ability cost")
      (is (= 1 (-> (get-runner) :discard count)) "Contract Killer should trash Data Dealer"))
    (take-credits state :corp)
    (take-credits state :runner)
    (core/gain state :corp :click 1)
    (core/move state :corp (find-card "Contract Killer" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Contract Killer" "New remote")
    (let [ck (get-content state :remote2 0)]
      (core/rez state :corp ck)
      (advance state ck 2)
      (card-ability state :corp ck 1)
      (is (= 1 (-> (get-corp) :discard count)) "Contract Killer should be trashed as an ability cost")
      (is (= 3 (-> (get-runner) :discard count)) "Contract Killer should do 2 meat damage"))))

(deftest corporate-town
  ;; Corporate Town
  (do-game
    (new-game {:corp {:deck ["Corporate Town" "Hostile Takeover"]}
               :runner {:deck ["Data Dealer"]}})
    (core/gain state :corp :click 1)
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Corporate Town" "New remote")
    (let [ct (get-content state :remote2 0)
          ht (get-scored state :corp 0)]
      (core/rez state :corp ct)
      (click-card state :corp ht)
      (take-credits state :corp)
      (play-from-hand state :runner "Data Dealer")
      (take-credits state :runner)
      (card-ability state :corp ct 0)
      (click-card state :corp (get-resource state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Corporate Town should trash Data Dealer")
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Corporate Town shouldn't activate if there are no resources"))))

(deftest cpc-generator
  ;; CPC Generator
  (do-game
    (new-game {:corp {:deck ["CPC Generator"]}})
    (play-from-hand state :corp "CPC Generator" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (let [credits (:credit (get-corp))]
      (core/click-credit state :runner nil)
      (is (= 1 (- (:credit (get-corp)) credits)) "Should gain one from CPC Generator"))
    (let [credits (:credit (get-corp))]
      (core/click-credit state :runner nil)
      (is (zero? (- (:credit (get-corp)) credits)) "Shouldn't gain another credit from CPC Generator"))))

(deftest csr-campaign
  ;; CSR Campaign
  (do-game
    (new-game {:corp {:deck [(qty "CSR Campaign" 10)]}})
    (play-from-hand state :corp "CSR Campaign" "New remote")
    (play-from-hand state :corp "CSR Campaign" "New remote")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :remote1 0))
    (core/rez state :corp (get-content state :remote2 0))
    (take-credits state :runner)
    (let [cards (count (:hand (get-corp)))
          csr1 (get-content state :remote1 0)
          csr2 (get-content state :remote2 0)]
      (is (:corp-phase-12 @state) "Corp is in Phase 1.2")
      (card-ability state :corp csr1 0)
      (click-prompt state :corp "Yes")                ;; Corp fires first CSR
      (is (= (+ 1 cards) (count (:hand (get-corp)))) "Should draw a card from CSR Campaign")
      (card-ability state :corp csr2 0)
      (click-prompt state :corp "No")                 ;; Corp declines to fire second CSR
      (is (= (+ 1 cards) (count (:hand (get-corp)))) "Should not draw another card" )
      (core/end-phase-12 state :corp nil)
      (is (= (+ 2 cards) (count (:hand (get-corp)))) "Should draw from mandatory draw"))))

(deftest cybernetics-court
  ;; Cybernetics Court
  (do-game
    (new-game {:corp {:deck ["Cybernetics Court"]}})
    (play-from-hand state :corp "Cybernetics Court" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 9 (hand-size :corp)) "Corp should have hand size of 9")))

(deftest daily-business-show
  ;; Daily Business Show
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck [(qty "Daily Business Show" 3) "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"]}})
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
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp)))) ;invalid target
      (click-card state :corp (find-card "Resistor" (:hand (get-corp))))
      (click-card state :corp (find-card "Product Placement" (:hand (get-corp))))
      (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (empty? (:prompt (get-runner))) "Runner prompt cleared")
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
      (new-game {:corp {:deck ["Daily Business Show" (qty "Sensie Actors Union" 2)
                               "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"]}})
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
        (click-card state :corp (find-card "Resistor" (:hand (get-corp)))) ; DBS target
        (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp)))) ; Sensie target
        (is (= 3 (count (:hand (get-corp)))))
        (is (= "Hedge Fund" (:title (last (:deck (get-corp))))) "Hedge Fund last card in deck")
        (is (= "Resistor" (:title (last (butlast (:deck (get-corp))))))
            "Resistor second last card in deck")
        ;; Try to use first Sensie again
        (card-ability state :corp sensie1 0)
        (is (empty? (:prompt (get-runner))) "Sensie didn't activate")
        (is (= 3 (count (:hand (get-corp)))))
        ;; Use second Sensie
        (starting-hand state :corp ["Hedge Fund" "Jackson Howard"])
        (is (= 2 (count (:hand (get-corp)))))
        (card-ability state :corp sensie2 0)
        (is (= 5 (count (:hand (get-corp)))) "Drew 3 cards with Sensie, DBS didn't activate")
        (click-card state :corp (find-card "Breaking News" (:hand (get-corp)))) ; Sensie target
        (is (= "Breaking News" (:title (last (:deck (get-corp))))) "Breaking News last card in deck"))))
  (testing "Should not trigger if rezzed after mandatory draw"
    (do-game
      (new-game {:corp {:deck [(qty "Daily Business Show" 3) "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"]}})
      (starting-hand state :corp ["Daily Business Show"])
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/draw state :corp)
      (is (= 1 (count (:hand (get-corp)))) "DBS did not fire on manual draw")
      (is (empty? (:prompt (get-corp))) "Corp is not being asked to bury a card with DBS")))
  (testing "Fire on Runner turn"
    (do-game
      (new-game {:corp {:deck ["Daily Business Show" "Hedge Fund"
                               "Resistor" "Product Placement" "Breaking News"]}
                 :runner {:deck ["Fisk Investment Seminar"]}})
      (starting-hand state :corp ["Daily Business Show"])
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (is (empty? (:hand (get-corp))) "Corp hand is empty")
      (play-from-hand state :runner "Fisk Investment Seminar")
      (is (= 4 (count (:hand (get-corp)))) "Drew an additional card from FIS")
      (is (not-empty (:prompt (get-runner))) "Runner is waiting for Corp to use DBS")
      (click-card state :corp (find-card "Resistor" (:hand (get-corp))))
      (is (empty? (:prompt (get-runner))) "Runner prompt cleared")
      (is (= 3 (count (:hand (get-corp)))))))
  (testing "Interaction with Rashida and Start of Turn effects. Issue #4582"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Daily Business Show" "Rashida Jaheem"]}})
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (take-credits state :corp)
      (core/rez state :corp (get-content state :remote1 0)) ;; DBS
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp has opportunity to use Rashida")
      (core/rez state :corp (get-content state :remote2 0)) ;; RJ
      (card-ability state :corp (get-content state :remote2 0) 0)
      (click-prompt state :corp "Yes")
      (is (nil? (get-content state :remote2 0)) "Rashida is trashed")
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (core/end-phase-12 state :corp nil)
      (is (empty? (:prompt (get-corp))) "DBS doesn't trigger on mandatory draw"))))

(deftest daily-quest
  ;; Daily Quest
  (testing "Can only rez during Corp's action phase"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daily Quest"]}})
      (play-from-hand state :corp "Daily Quest" "New remote")
      (let [dq (get-content state :remote1 0)]
        (core/rez state :corp dq)
        (is (rezzed? (refresh dq)) "Can rez on Corp turn")
        (core/derez state :corp dq)
        (take-credits state :corp)
        (core/rez state :corp dq)
        (is (not (rezzed? (refresh dq))) "Cannot rez on Runner turn"))))
  (testing "Runner gains credits on successful runs"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daily Quest"]}})
      (play-from-hand state :corp "Daily Quest" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (run-empty-server state :remote1)
      (click-prompt state :runner "No action")
      (is (= 7 (:credit (get-runner))))
      (run-empty-server state :remote1)
      (click-prompt state :runner "No action")
      (is (= 9 (:credit (get-runner))))
      (run-on state :remote1)
      (run-jack-out state)
      (is (= 9 (:credit (get-runner))))
      (is (= 6 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 6 (:credit (get-corp))) "Corp didn't gain credits due to successful run on Daily Quest server")))
  (testing "Corp gains credits on no successful runs last turn"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daily Quest"]}})
      (play-from-hand state :corp "Daily Quest" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :hq)
      (run-empty-server state :rd)
      (click-prompt state :runner "No action")
      (run-empty-server state :archives)
      (run-on state :remote1)
      (run-jack-out state)
      (is (= 5 (:credit (get-runner))) "No successful runs on Daily Quest server")
      (is (= 6 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Corp gained credits due to no successful runs on Daily Quest server")))
  (testing "Corp gains credits on no successful runs last turn when hosted on Rec Studio. Issue #4193"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daily Quest" "Full Immersion RecStudio"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (core/rez state :corp fir)
        (card-ability state :corp fir 0)
        (click-card state :corp "Daily Quest")
        (let [dq (first (:hosted (refresh fir)))]
          (core/rez state :corp dq)
          (take-credits state :corp)
          (run-empty-server state :hq)
          (run-empty-server state :rd)
          (click-prompt state :runner "No action")
          (run-empty-server state :archives)
          (run-on state :remote1)
          (run-jack-out state)
          (is (= 5 (:credit (get-runner))) "No successful runs on Daily Quest server")
          (is (= 3 (:credit (get-corp))))
          (take-credits state :runner)
          (is (= 6 (:credit (get-corp))) "Corp gained credits due to no successful runs on Daily Quest server")))))
  (testing "Corp gains credits when there were no runs last turn. Issue #4447"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daily Quest"]}})
      (play-from-hand state :corp "Daily Quest" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (is (= 6 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Corp gained credits due to no successful runs on Daily Quest server"))))

(deftest dedicated-response-team
  ;; Dedicated Response Team - Do 2 meat damage when successful run ends if Runner is tagged
  (do-game
    (new-game {:corp {:deck ["Dedicated Response Team"]}})
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (let [drt (get-content state :remote1 0)]
      (core/rez state :corp drt)
      (take-credits state :corp)
      (run-empty-server state :rd)
      (is (empty? (:discard (get-runner))) "Not tagged, no damage done")
      (gain-tags state :runner 1)
      (run-on state :rd)
      (run-jack-out state)
      (is (empty? (:discard (get-runner))) "Tagged but run unsuccessful, no damage done")
      (run-empty-server state :rd)
      (is (= 2 (count (:discard (get-runner)))) "Suffered 2 damage for successful run w/ tag"))))

(deftest dedicated-server
  ;; Dedicated Server
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Dedicated Server"]}})
      (play-from-hand state :corp "Dedicated Server" "New remote")
      (let [server (get-content state :remote1 0)]
        (core/rez state :corp server)
        (is (= 2 (get-counters (refresh server) :recurring)) "Should have 2 recurring credits"))))
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:deck ["Dedicated Server" "Ice Wall"]}})
      (play-from-hand state :corp "Dedicated Server" "New remote")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [server (get-content state :remote1 0)
            iw (get-ice state :hq 0)]
        (core/rez state :corp server)
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 1 credit from Dedicated Server"
                           (core/rez state :corp iw)
                           (click-card state :corp server))))))

(deftest director-haas
  ;; Director Haas
  (do-game
    (new-game {:corp {:deck [(qty "Director Haas" 2)]}})
    (play-from-hand state :corp "Director Haas" "New remote")
    (play-from-hand state :corp "Director Haas" "Server 1")
    (click-prompt state :corp "OK")
    (is (= 1 (count (:discard (get-corp)))) "First Haas trashed")
    (is (zero? (:agenda-point (get-runner))) "No points for Runner if trashed by Corp")
    (let [dh (get-content state :remote1 0)]
      (core/rez state :corp dh))
    (is (= 1 (:click (get-corp))) "Corp should not gain a click")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 4 (:click (get-corp))) "Corp should have an extra click each turn")
    (take-credits state :corp)
    (take-credits state :runner 3)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should be back to 3 clicks")
    (is (= 1 (count (get-scored state :runner))) "Director Haas added to Runner score area")
    (is (= 2 (:agenda-point (get-runner))) "Runner gained 2 agenda points")))

(deftest docklands-crackdown
  ;; Docklands Crackdown
  (letfn [(dlcd-test [number]
            (do-game
              (new-game {:corp {:deck ["Docklands Crackdown"]}
                         :runner {:deck ["Cache"]}})
              (play-from-hand state :corp "Docklands Crackdown" "New remote")
              (let [dlcd (get-content state :remote1 0)]
                (core/rez state :corp dlcd)
                (core/add-counter state :corp dlcd :power number)
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (is (= (- 4 number) (:credit (get-runner)))))))]
    (doall (map dlcd-test [0 1 2 3 4]))))

(deftest drudge-work
  ;; Drudge Work - Shuffle agenda from HQ or Archives into R&D, and gain credits = to agenda points
  ;; TODO: do some Noah magic on this test to test several agendas from several locations
  (do-game
    (new-game {:corp {:deck ["Drudge Work"
                             "Hostile Takeover" "Standoff" "Global Food Initiative" "Armed Intimidation"
                             "Hedge Fund"]}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Drudge Work" "New remote")
    (play-from-hand state :corp "Armed Intimidation" "New remote")
    (trash-from-hand state :corp "Hostile Takeover")
    (let [hand (:hand (get-corp))
          drudge-work (get-content state :remote1 0)
          ai (get-content state :remote2 0)
          ht (find-card "Hostile Takeover" (:discard (get-corp)))
          standoff (find-card "Standoff" hand)
          gfi (find-card "Global Food Initiative" hand)
          hf (find-card "Hedge Fund" hand)]
      (core/rez state :corp drudge-work)
      (testing "Rez cost and placing counters on Drudge Work on rez"
        (is (= (- 5 2) (:credit (get-corp))) "Cost 2 credits to rez Drudge Work")
        (is (= 3 (get-counters (refresh drudge-work) :power)) "Drudge Work gained 3 power counters on rez"))
      (testing "Selecting installed agenda, or Operation in hand"
        (card-ability state :corp (refresh drudge-work) 0)
        (click-card state :corp ai)
        (is (some? (get-content state :remote2 0)) "Armed Intimidation still installed in a remote server")
        (is (= 3 (get-counters (refresh drudge-work) :power)) "Did not use a counter when selecting incorrect target")
        (is (= (- 5 2) (:credit (get-corp))) "Did not gain credits when selecting installed Agenda")
        (is (empty? (:deck (get-corp))) "No cards in R&D")
        (click-card state :corp hf)
        (is (= 3 (count (:hand (get-corp)))) "Hedge Fund still in HQ")
        (is (= 3 (get-counters (refresh drudge-work) :power)) "Did not use a counter when selecting incorrect target")
        (is (= (- 5 2) (:credit (get-corp))) "Did not gain credits when selecting operation")
        (is (empty? (:deck (get-corp))) "No cards in R&D"))
      (testing "Gaining credits and shuffling in agenda from HQ"
        (click-card state :corp gfi)
        (is (= 1 (count (:deck (get-corp)))) "One card (the GFI) shuffled into R&D")
        (is (= 2 (count (:hand (get-corp)))) "Two cards left in HQ (Standoff and Hedge Fund)")
        (is (= 2 (:click (get-corp))) "Used a click to activate Drudge Work")
        (is (= 2 (get-counters (refresh drudge-work) :power)) "Used a counter when selecting GFI from HQ")
        (is (= (+ 5 -2 3) (:credit (get-corp))) "Gained 3 credits when revealing GFI"))
      (testing "Gaining credits and shuffling in agenda from Archives"
        (card-ability state :corp (refresh drudge-work) 0)
        (click-card state :corp ht)
        (is (= 2 (count (:deck (get-corp)))) "One more card (the Hostile Takeover) shuffled into R&D")
        (is (= 2 (count (:hand (get-corp)))) "Two cards left in HQ (Standoff and Hedge Fund)")
        (is (= 1 (:click (get-corp))) "Used a click to activate Drudge Work")
        (is (= 1 (get-counters (refresh drudge-work) :power)) "Used a counter when selecting Hostile Takeover from Archives")
        (is (= (+ 5 -2 3 1) (:credit (get-corp))) "Gained 1 credits when revealing Hostile Takeover"))
      (testing "Gain 0 credits when shuffling in Standoff, trashing Drudge Work after 3 uses"
        (card-ability state :corp (refresh drudge-work) 0)
        (click-card state :corp standoff)
        (is (= 3 (count (:deck (get-corp)))) "One more card (the Standoff) shuffled into R&D")
        (is (= 1 (count (:hand (get-corp)))) "Only the Hedge Fund left in HQ")
        (is (zero? (:click (get-corp))) "Used a click to activate Drudge Work")
        (is (= "Drudge Work" (-> (get-corp) :discard first :title)) "Drudge Work trashed after no counters left")
        (is (= (+ 5 -2 3 1) (:credit (get-corp))) "Gained 0 credits when revealing Standoff")))))

(deftest early-premiere
  ;; Early Premiere - Pay 1c at start of turn to place an advancement on a card in a server
  (do-game
    (new-game {:corp {:deck ["Early Premiere" "Ice Wall"
                             "Ghost Branch" "Blacklist"]}})
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
      (click-card state :corp iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall can't targeted, not in server")
      (click-card state :corp bl)
      (is (zero? (get-counters (refresh bl) :advancement)) "Blacklist can't targeted, can't be advanced")
      (click-card state :corp gb)
      (is (= 1 (get-counters (refresh gb) :advancement)) "1 advancement on Ghost Branch")
      (is (= 4 (:credit (get-corp)))))))

(deftest echo-chamber
  ;; Echo Chamber - 3 clicks to become 1 point agenda
  (do-game
    (new-game {:corp {:deck ["Echo Chamber"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Echo Chamber" "New remote")
    (let [ec (get-content state :remote1 0)]
      (core/rez state :corp ec)
      (card-ability state :corp ec 0))
    (is (= 1 (:agendapoints (get-scored state :corp 0))) "Echo Chamber added to Corp score area")))

(deftest edge-of-world
  ;; Edge of World
  (do-game
    (new-game {:corp {:deck [(qty "Edge of World" 3) (qty "Ice Wall" 3)]}})
    (core/gain state :corp :credit 6 :click 1)
    (play-from-hand state :corp "Edge of World" "New remote")
    (play-from-hand state :corp "Edge of World" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (take-credits state :corp)
    (run-on state "Server 1")
    ;; ice wall 1
    (run-continue state)
    ;; ice wall 2
    (run-continue state)
    ;; server
    (run-successful state)
    (is (= :waiting (prompt-type :runner)) "Runner waiting for Corp to act")
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "Pay 0 [Credits] to trash")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
    (run-empty-server state "Server 2")
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "Pay 0 [Credits] to trash")
    (is (= 2 (:brain-damage (get-runner))) "Runner did not take brain damage when no ICE protected Edge of World")))

(deftest eliza-s-toybox
  ;; Eliza's Toybox - Rez a card ignoring all costs
  (do-game
    (new-game {:corp {:deck ["Eliza's Toybox" "Wotan" "Archer"]}})
    (play-from-hand state :corp "Wotan" "R&D")
    (play-from-hand state :corp "Archer" "HQ")
    (play-from-hand state :corp "Eliza's Toybox" "New remote")
    (let [wotan (get-ice state :rd 0)
          archer (get-ice state :hq 0)
          eliza (get-content state :remote1 0)]
      (core/rez state :corp eliza)
      (is (= 1 (:credit (get-corp))))
      (is (zero? (:click (get-corp))) "3 clicks spent")
      (core/gain state :corp :click 6)
      (card-ability state :corp eliza 0)
      (click-card state :corp wotan)
      (is (rezzed? (refresh wotan)))
      (is (= 3 (:click (get-corp))) "3 clicks spent")
      (is (= 1 (:credit (get-corp))) "No credits spent")
      (card-ability state :corp eliza 0)
      (click-card state :corp archer)
      (is (rezzed? (refresh archer)))
      (is (zero? (:click (get-corp))) "3 clicks spent")
      (is (= 1 (:credit (get-corp))) "No credits or agendas spent"))))

(deftest elizabeth-mills
  ;; Elizabeth Mills - Remove 1 bad publicity when rezzed; click-trash to trash a location
  (do-game
    (new-game {:corp {:deck ["Elizabeth Mills"]}
               :runner {:deck ["Earthrise Hotel"]}})
    (core/gain state :corp :bad-publicity 1)
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (take-credits state :runner)
    (let [liz (get-content state :remote1 0)
          hotel (get-resource state 0)]
      (core/rez state :corp liz)
      (is (zero? (count-bad-pub state)) "1 bad publicity removed")
      (card-ability state :corp liz 0)
      (click-card state :corp hotel)
      (is (= 1 (count (:discard (get-runner)))) "Earthrise trashed")
      (is (= 1 (count (:discard (get-corp)))) "Elizabeth Mills trashed")
      (is (= 1 (count-bad-pub state)) "1 bad publicity taken from trashing a location"))))

(deftest encryption-protocol
  ;; Encryption Protocol - Trash cost of installed cards increased by 1
  (do-game
    (new-game {:corp {:deck [(qty "Encryption Protocol" 2)]}})
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
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (run-empty-server state "Server 2")
      (is (= 3 (core/trash-cost state :runner (refresh ep2)))
          "Trash cost increased to 3 by one active Encryption Protocol"))))

(deftest estelle-moon
  ;; Estelle Moon
  (testing "Basic test"
    (letfn [(estelle-test [number]
              (do-game
                (new-game {:corp {:deck ["Estelle Moon" (qty "Encryption Protocol" 20)]}})
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
  (testing "Estelle Moon triggers multiple time after derez #4601"
    (do-game
      (new-game {:corp {:hand ["Estelle Moon" "Divert Power" (qty "Encryption Protocol" 3)]
                        :deck [(qty "Encryption Protocol" 20)]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Estelle Moon" "New remote")
      (let [em (get-content state :remote1 0)]
        (core/rez state :corp (refresh em))
        (play-from-hand state :corp "Encryption Protocol" "New remote")
        (is (= 1 (get-counters (refresh em) :power)) "Moon has one power token")
        (play-from-hand state :corp "Divert Power")
        (click-card state :corp (refresh em)) ;derez moon
        (click-card state :corp (refresh em)) ;rez again
        (play-from-hand state :corp "Encryption Protocol" "New remote")
        (is (= 2 (get-counters (refresh em) :power)) "Moon has two power tokens")))))

(deftest eve-campaign
  ;; Eve Campaign
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"]}})
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (let [eve (get-content state :remote1 0)]
      (core/rez state :corp eve)
      (is (zero? (:credit (get-corp))))
      (is (= 16 (get-counters (refresh eve) :credit)))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 4 (:credit (get-corp))))
      (is (= 14 (get-counters (refresh eve) :credit))))))

(deftest executive-boot-camp
  ;; Executive Boot Camp
  (testing "suppress the start-of-turn event on a rezzed card. Issue #1346"
    (do-game
      (new-game {:corp {:deck ["Eve Campaign" "Executive Boot Camp"]}})
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
        (click-card state :corp eve)
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
      (new-game {:corp {:deck ["15 Minutes" "Executive Boot Camp"
                               "Tithonium"]}})
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
        (is (not (rezzed? (refresh tith))) "Tithonium not rezzed")
        (is (:corp-phase-12 @state) "Corp in Step 1.2")
        (card-ability state :corp ebc 0)
        (click-card state :corp tith)
        (click-prompt state :corp "No")
        (is (and (:installed (refresh tith)) (rezzed? (refresh tith))) "Rezzed Tithonium")
        (is (= 1 (:credit (get-corp))) "EBC saved 1 credit on the rez of Tithonium"))))
  (testing "works with pay-credits prompt for Mumba Temple"
    (do-game
      (new-game {:corp {:deck ["Mumba Temple" "Eve Campaign" "Executive Boot Camp"]}})
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Executive Boot Camp" "New remote")
      (play-from-hand state :corp "Mumba Temple" "New remote")
      (take-credits state :corp)
      (is (= 5 (:credit (get-corp))) "Corp ends turn with 5 credits")
      (let [eve (get-content state :remote1 0)
            ebc (get-content state :remote2 0)
            mum (get-content state :remote3 0)]
        (core/rez state :corp ebc)
        (core/rez state :corp mum)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp in Step 1.2")
        (card-ability state :corp ebc 0)
        (click-card state :corp eve)
        (dotimes [_ 2]
          (click-card state :corp mum))
        (is (= 2 (:credit (get-corp))) "EBC + Mumba saved 3 credit on the rez of Eve")
        (is (= 16 (get-counters (refresh eve) :credit)))
        (core/end-phase-12 state :corp nil)
        (is (= 2 (:credit (get-corp))) "Corp did not gain credits from Eve")
        (is (= 16 (get-counters (refresh eve) :credit)) "Did not take counters from Eve")
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (:corp-phase-12 @state)) "With nothing to rez, EBC does not trigger Step 1.2")
        (is (= 14 (get-counters (refresh eve) :credit)) "Took counters from Eve")))))

(deftest executive-search-firm
  ;; Executive Search Firm
  (do-game
    (new-game {:corp {:deck ["Executive Search Firm" "Elizabeth Mills"
                             "Midori" "Shannon Claire"]}})
    (starting-hand state :corp ["Executive Search Firm"])
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Executive Search Firm" "New remote")
    (doseq [card ["Elizabeth Mills" "Midori" "Shannon Claire"]]
      (let [esf (get-content state :remote1 0)
            number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (core/rez state :corp esf)
        (card-ability state :corp esf 0)
        (click-prompt state :corp (find-card card (:deck (get-corp))))
        (is (= card (-> (get-corp) :hand first :title)) (str card " should be in hand"))
        (core/move state :corp (find-card card (:hand (get-corp))) :deck)
        (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Should be shuffled")))))

(deftest expose
  ;; Exposé
  (do-game
    (new-game {:corp {:deck ["Exposé"]}})
    (core/gain state :corp :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :corp "Exposé" "New remote")
      (let [expose (get-content state (keyword (str "remote" (inc i))) 0)]
        (core/rez state :corp (refresh expose))
        (is (zero? (count-bad-pub state)) "Corp should have 0 bad publicity to start with")
        (when (pos? i)
          (core/gain-bad-publicity state :corp i)
          (is (= i (count-bad-pub state)) (str "Corp should gain " i " bad publicity"))
          (advance state (refresh expose) i))
        (card-ability state :corp (refresh expose) 0)
        (is (zero? (count-bad-pub state)) "Corp should have 0 bad publicity after using Exposé's ability")
        (is (= 1 (-> (get-corp) :discard count)) "Archives should have 1 card in it")
        (is (= "Exposé" (-> (get-corp) :discard first :title)) "Only card in Archives should be Exposé")
        (core/move state :corp (find-card "Exposé" (:discard (get-corp))) :hand)))))

(deftest false-flag
  ;; False Flag
  (testing "when the corp attempts to score False Flag"
    (testing "and False Flag has 7 advancements"
      (do-game
        (new-game {:corp {:deck ["False Flag"]}})
        (play-from-hand state :corp "False Flag" "New remote")
        (let [ff (get-content state :remote1 0)]
          (core/add-counter state :corp ff :advancement 7)
          (core/rez state :corp (refresh ff))
          (card-ability state :corp (refresh ff) 0)
          (is (nil? (get-content state :remote1 0))
              "False Flag is no longer in remote")
          (is (= 3 (:agendapoints (get-scored state :corp 0)))
              "the corp can score False Flag")
          (is (= 1 (:click (get-corp)))
              "scoring False Flag costs one click"))))
    (testing "and False Flag has less than 7 advancements"
      (do-game
        (new-game {:corp {:deck ["False Flag"]}})
        (play-from-hand state :corp "False Flag" "New remote")
        (let [ff (get-content state :remote1 0)]
          (core/add-counter state :corp ff :advancement 6)
          (core/rez state :corp (refresh ff))
          (card-ability state :corp (refresh ff) 0)
          (is (not (nil? (get-content state :remote1 0)))
              "False Flag remains in the remote")
          (is (nil? (:agendapoints (get-scored state :corp 0)))
              "the corp cannot score false flag")
          (is (= 2 (:click (get-corp)))
              "the corp does not lose a click")))))
  (testing "when the runner accesses False Flag"
    (letfn [(false-flag-tags-test
              [[advancements expected-tags]]
              (testing (str "and False Flag has " advancements " advancements")
                (do-game
                  (new-game {:corp {:deck ["False Flag"]}})
                  (play-from-hand state :corp "False Flag" "New remote")
                  (core/add-prop state :corp
                                 (get-content state :remote1 0)
                                 :advance-counter advancements)
                  (take-credits state :corp)
                  (run-empty-server state "Server 1")
                  (click-prompt state :runner "No action")
                  (let [tags (count-tags state)]
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
    (new-game {:corp {:deck ["Franchise City" "Accelerated Beta Test"]}})
    (play-from-hand state :corp "Franchise City" "New remote")
    (play-from-hand state :corp "Accelerated Beta Test" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp 1)
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (is (zero? (count (get-content state :remote2))) "Agenda was stolen")
    (is (= 2 (:agenda-point (get-runner))) "Runner stole 2 points")
    (is (zero? (count (get-content state :remote2)))
        "Franchise City no longer installed")
    (is (find-card "Franchise City" (:scored (get-corp))) "Franchise City in corp scored area")
    (is (= 1 (:agenda-point (get-corp))) "Corp has 1 point")))

(deftest full-immersion-recstudio
  ;; Full Immmersion RecStudio - install directly, and via Interns
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck ["Full Immersion RecStudio"
                               (qty "Interns" 2)
                               (qty "Launch Campaign" 3)]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (core/rez state :corp fir)
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Launch Campaign" (:hand (get-corp))))
        (let [lc (first (:hosted (refresh fir)))]
          (is lc "Launch Campaign hosted on Full Immersion RecStudio")
          (core/rez state :corp lc)
          (is (and (:installed (refresh lc)) (rezzed? (refresh lc))) "Rezzed Launch Campaign")
          (take-credits state :corp)
          (take-credits state :runner)
          (is (= 5 (:credit (get-corp))) "Gained 2cr from Launch Campaign")
          (is (= 4 (get-counters (refresh lc) :credit)) "4cr left on Launch Campaign")
          (play-from-hand state :corp "Interns")
          (click-card state :corp (find-card "Launch Campaign" (:hand (get-corp))))
          (click-prompt state :corp (refresh fir))
          (is (= 2 (count (:hosted (refresh fir)))) "Interns installed onto FIR")))))
  (testing "hosting an asset with events does not double-register events. Issue #1827"
    (do-game
      (new-game {:corp {:deck ["Full Immersion RecStudio" "Sandburg" "Vanilla"
                               "Oaktown Renovation"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (play-from-hand state :corp "Vanilla" "HQ")
      (let [fir (get-content state :remote1 0)
            van (get-ice state :hq 0)]
        (core/rez state :corp fir)
        (core/rez state :corp van)
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Sandburg" (:hand (get-corp))))
        (core/gain state :corp :credit 7 :click 3)
        (core/rez state :corp (first (:hosted (refresh fir))))
        (is (= 2 (:current-strength (refresh van))) "Vanilla at 2 strength")
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (core/advance state :corp {:card (last (:hosted (refresh fir)))})
        (is (= 11 (:credit (get-corp))) "Gained 1cr from advancing Oaktown")))))

(deftest fumiko-yamamori
  ;; Fumiko Yamamori
  (do-game
    (new-game {:corp {:deck ["Fumiko Yamamori"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Fumiko Yamamori" "New remote")
    (let [fumiko (get-content state :remote1 0)]
      (core/rez state :corp (refresh fumiko))
      (core/psi-game state :corp (refresh fumiko)
                     {:equal {:msg "resolve equal bets effect"}
                      :not-equal {:msg "resolve unequal bets effect"}})
      (click-prompt state :corp "2 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should discard a card to meat damage"))))

(deftest gene-splicer
  ;; Gene Splicer
  (testing "Runner accesses an unadvanced Gene Splicer and doesn't trash
    ;; No net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-runner)))) "Runner took no net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses an unadvanced Gene Splicer and trashes it.
    No net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (zero? (count (:discard (get-runner)))) "Runner took no net damage")
      (is (nil? (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Runner accesses a single-advanced Gene Splicer and doesn't trash.
    1 net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 1)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses a single-advanced Gene Splicer and trashes it.
    1 net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 1)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (is (nil? (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Runner accesses a double-advanced Gene Splicer and doesn't trash
    2 net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 2)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses a double-advanced Gene Splicer and trashes it.
    2 net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 2)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
      (is (nil? (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Corp triple-advances a Gene Splicer and uses its ability to add to their score area as a 1 point agenda"
    (do-game
      (new-game {:corp {:deck [(qty "Gene Splicer" 2) (qty "Ice Wall" 3) (qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (let [gs (get-content state :remote1 0)]
        (core/add-counter state :corp gs :advancement 2)
        (take-credits state :runner)
        (core/add-counter state :corp (refresh gs) :advancement 1)
        (core/rez state :corp (refresh gs))
        (card-ability state :corp (refresh gs) 0)
        (is (nil? (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
        (is (= 1 (:agendapoints (get-scored state :corp 0))) "Gene Splicer added to Corp score area"))))
  (testing "Corp double-advances a Gene Splicer and fails to use its ability to add to their score area as a 1 point agenda"
    (do-game
      (new-game {:corp {:hand ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (let [gs (get-content state :remote1 0)]
        (dotimes [_ 2] (core/advance state :corp {:card (refresh gs)}))
        (take-credits state :runner)
        (core/rez state :corp (refresh gs))
        (card-ability state :corp (refresh gs) 0)
        (is (refresh gs) "Gene Splicer is still in remote")
        (is (empty? (get-scored state :corp)) "Score area is still empty")))))

(deftest genetics-pavilion
  ;; Genetics Pavilion - Limit Runner to 2 draws per turn, but only during Runner's turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Genetics Pavilion"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]
                          :hand ["Diesel" "Sports Hopper"]}})
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :corp)
        (core/rez state :corp gp)
        (play-from-hand state :runner "Sports Hopper")
        (play-from-hand state :runner "Diesel")
        (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
        (take-credits state :runner)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (let [hopper (get-hardware state 0)]
          (card-ability state :runner hopper 0)
          (is (= 3 (count (:hand (get-runner)))) "Able to draw 3 cards during Corp's turn")
          (core/derez state :corp (refresh gp))
          (take-credits state :corp)))))
  (testing "Disables further draws after drawing"
    (do-game
      (new-game {:corp {:deck ["Genetics Pavilion"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]
                          :hand ["Diesel"]}})
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :corp)
        (is (= 1 (count (:hand (get-runner)))))
        (play-from-hand state :runner "Diesel")
        (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards with Diesel")
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/rez state :corp (refresh gp))
        (core/draw state :runner)
        (is (= 2 (count (:hand (get-runner)))) "No card drawn; GP counts cards drawn prior to rez"))))
  (testing "vs Fisk Investment Seminar"
    (do-game
      (new-game {:corp {:deck ["Genetics Pavilion" (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Fisk Investment Seminar" (qty "Sure Gamble" 3)]}})
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
        (is (zero? (count (:hand (get-corp)))))
        (play-from-hand state :runner "Fisk Investment Seminar")
        (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
        (is (= 3 (count (:hand (get-corp)))) "Drew all 3 cards"))))
  (testing "Mr. Li interaction. #1594"
    (do-game
      (new-game {:corp {:deck ["Genetics Pavilion"]}
                 :runner {:deck ["Mr. Li" "Account Siphon" "Faerie"
                                 "Sure Gamble" "John Masanori" "Desperado"]}})
      (starting-hand state :runner ["Mr. Li"])
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Mr. Li")
      (let [mrli (get-resource state 0)]
        (is (zero? (count (:hand (get-runner)))))
        ; use Mr. Li with 2 draws allowed
        (card-ability state :runner mrli 0)
        (is (= 2 (count (:hand (get-runner)))))
        (click-card state :runner (first (:hand (get-runner))))
        (is (= 1 (count (:hand (get-runner)))))
        ; use Mr. Li with 0 draws allowed
        (card-ability state :runner mrli 0)
        (is (= 1 (count (:hand (get-runner)))))
        (is (empty? (:prompt (get-runner))) "No runner prompt open")
        (take-credits state :runner)
        (take-credits state :corp)
        (core/draw state :runner)
        (is (= 2 (count (:hand (get-runner)))))
        ; use Mr. Li with 1 draw allowed - should draw 1, then insist it's put back
        (card-ability state :runner mrli 0)
        (is (= 3 (count (:hand (get-runner)))))
        (click-card state :runner (first (:hand (get-runner)))) ; will fail
        (click-card state :runner (second (:hand (get-runner)))) ; will fail
        (is (= 3 (count (:hand (get-runner)))) "Clicking invalid cards caused no discards")
        (click-card state :runner (second (rest (:hand (get-runner)))))
        (is (= 2 (count (:hand (get-runner)))) "Clicking the single valid card did"))))
  (testing "No cards in stack but draw effects. #4192"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Genetics Pavilion"]}
                 :runner {:hand ["Labor Rights" (qty "Crowdfunding" 2)]
                          :discard ["Account Siphon" "Bankroll" "Cache"]}})
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Crowdfunding")
      (play-from-hand state :runner "Crowdfunding")
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-runner)))) "Labor Rights is in the grip")
      (play-from-hand state :runner "Labor Rights")
      (is (zero? (count (:hand (get-runner)))))
      (click-card state :runner "Account Siphon")
      (click-card state :runner "Bankroll")
      (click-card state :runner "Cache")
      (is (= 1 (count (:hand (get-runner)))) "Drew a card from Labor Rights"))))

(deftest ghost-branch
  ;; Ghost Branch - Advanceable; give the Runner tags equal to advancements when accessed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ghost Branch"]}})
      (play-from-hand state :corp "Ghost Branch" "New remote")
      (let [gb (get-content state :remote1 0)]
        (core/advance state :corp {:card (refresh gb)})
        (core/advance state :corp {:card (refresh gb)})
        (is (= 2 (get-counters (refresh gb) :advancement)))
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (click-prompt state :corp "Yes") ; choose to do the optional ability
        (is (= 2 (count-tags state)) "Runner given 2 tags"))))
  (testing "with Dedicated Response Team"
    (do-game
      (new-game {:corp {:deck ["Ghost Branch" "Dedicated Response Team"]}})
      (play-from-hand state :corp "Ghost Branch" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/gain state :corp :click 1)
      (let [gb (get-content state :remote1 0)
            drt (get-content state :remote2 0)]
        (core/advance state :corp {:card gb})
        (core/advance state :corp {:card (refresh gb)})
        (is (= 2 (get-counters (refresh gb) :advancement)) "Ghost Branch advanced twice")
        (take-credits state :corp)
        (run-on state "Server 1")
        (core/rez state :corp drt)
        (run-continue state)
        (run-successful state)
        (is (prompt-is-type? state :runner :waiting) "Runner has prompt to wait for Ghost Branch")
        (click-prompt state :corp "Yes")
        (is (= 2 (count-tags state)) "Runner has 2 tags")
        (click-prompt state :runner "Pay 0 [Credits] to trash")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage")))))

(deftest grndl-refinery
  ;; GRNDL Refinery
  (do-game
    (new-game {:corp {:deck ["GRNDL Refinery"]}})
    (core/gain state :corp :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :corp "GRNDL Refinery" "New remote")
      (let [grndl (get-content state (keyword (str "remote" (inc i))) 0)
            credits (- (:credit (get-corp)) i)]
        (core/rez state :corp grndl)
        (when (pos? i)
          (advance state (refresh grndl) i)
          (is (= i (get-counters (refresh grndl) :advancement)) (str "GRNDL Refinery should have " i " advancement counters on it")))
        (card-ability state :corp (refresh grndl) 0)
        (is (= (+ credits (* i 4)) (:credit (get-corp))) (str "Corp should gain " (* i 4) " credits"))
        (is (= 1 (-> (get-corp) :discard count)) "Archives should have 1 card in it")
        (is (= "GRNDL Refinery" (-> (get-corp) :discard first :title)) "Only card in Archives should be GRNDL Refinery")
        (core/move state :corp (find-card "GRNDL Refinery" (:discard (get-corp))) :hand)))))

(deftest haas-arcology-ai
  ;; Haas Arcology AI - Click and advancement to gain 2 clicks, once per turn
  (do-game
    (new-game {:corp {:deck ["Haas Arcology AI"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Haas Arcology AI" "New remote")
    (let [haa (get-content state :remote1 0)]
      (advance state haa 2)
      (core/rez state :corp (refresh haa))
      (is (= 1 (:click (get-corp))))
      (is (= 2 (get-counters (refresh haa) :advancement)))
      (card-ability state :corp (refresh haa) 0)
      (is (= 1 (get-counters (refresh haa) :advancement)) "Spent 1 advancement")
      (is (= 2 (:click (get-corp))) "Spent last click to gain 2 clicks")
      (card-ability state :corp (refresh haa) 0)
      (is (= 1 (get-counters (refresh haa) :advancement)) "Can't use twice in a turn")
      (is (= 2 (:click (get-corp))) "Didn't spend a click"))))

(deftest honeyfarm
  ;; Honeyfarm - lose one credit on access
  (do-game
    (new-game {:corp {:deck [(qty "Honeyfarm" 3)]}})
    (trash-from-hand state :corp "Honeyfarm")
    (play-from-hand state :corp "Honeyfarm" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "No action")
    (is (= 4 (:credit (get-runner))))
    (run-empty-server state "Archives")
    (is (= 3 (:credit (get-runner))))
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action")
    (is (= 2 (:credit (get-runner))))))

(deftest hostile-infrastructure
  ;; Hostile Infrastructure - do 1 net damage when runner trashes a corp card
  (do-game
    (new-game {:corp {:deck [(qty "Hostile Infrastructure" 3)]}})
    (core/gain state :runner :credit 50)
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (is (= 1 (count (:discard (get-runner)))) "Took 1 net damage")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (is (= 2 (count (:discard (get-runner)))) "Took 1 net damage")))

(deftest hyoubu-research-facility
  ;; Hyoubu Research Facility
  (do-game
    (new-game {:corp {:deck ["Hyoubu Research Facility" "Snowflake"]}})
    (play-from-hand state :corp "Hyoubu Research Facility" "New remote")
    (play-from-hand state :corp "Snowflake" "HQ")
    (let [hrf (get-content state :remote1 0)
          sf (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (core/rez state :corp hrf)
      (core/rez state :corp sf)
      (run-continue state)
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "2 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 2c from Hyoubu")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "2 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 3 (:credit (get-corp))) "No credits gained from Hyoubu"))))

(deftest ibrahim-salem
  ;; Ibrahim Salem
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Ibrahim Salem"]}
               :runner {:deck ["Sure Gamble" "Astrolabe" "Paperclip" "Daily Casts"]}})
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Ibrahim Salem" "New remote")
    (let [ibrahim (get-content state :remote2 0)]
      (core/rez state :corp (refresh ibrahim))
      (click-card state :corp (-> (get-corp) :scored first))
      (doseq [[i [card-type card-name]]
              (map-indexed vector ['("Event" "Sure Gamble")
                                   '("Hardware" "Astrolabe")
                                   '("Program" "Paperclip")
                                   '("Resource" "Daily Casts")])]
        (take-credits state :corp)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp is in Step 1.2")
        (card-ability state :corp ibrahim 0)
        (click-prompt state :corp card-type)
        (click-prompt state :corp (find-card card-name (:hand (get-runner))))
        (core/end-phase-12 state :corp nil)
        (is (= (inc i) (-> (get-runner) :discard count)))))))

(deftest illegal-arms-factory
  ;; Illegal Arms Factory; draw a card, gain a credit, bad pub when trashed while rezzed
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"
                             "Beanstalk Royalties"
                             "IPO"
                             (qty "Illegal Arms Factory" 3)]}})
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
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (zero? (count-bad-pub state)) "Took no bad pub on unrezzed trash")
      (take-credits state :runner)
      (is (= 3 (count (:hand (get-corp)))) "Drew a card from IAF + mandatory")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit from IAF")
      (take-credits state :corp)
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (= 1 (count-bad-pub state)) "Took a bad pub on rezzed trash"))))

(deftest indian-union-stock-exchange
  ;; Indian Union Stock Exchange
  (do-game
    (new-game {:corp {:id "Argus Security: Protection Guaranteed"
                      :deck ["Indian Union Stock Exchange" "Beanstalk Royalties"
                             "Kill Switch" "Net Police"]}})
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

(deftest isabel-mcguire
  ;; Isabel McGuire
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Isabel McGuire"]}})
    (play-from-hand state :corp "Isabel McGuire" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (zero? (-> (get-corp) :hand count)))
    (let [isabel (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp isabel)
      (card-ability state :corp isabel 0)
      (click-card state :corp (refresh iw))
      (is (= 1 (-> (get-corp) :hand count))))))

(deftest it-department
  ;; IT Department - Add strength to rezzed ICE until end of turn
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["IT Department" "Wall of Static"]}})
    (play-from-hand state :corp "IT Department" "New remote")
    (play-from-hand state :corp "Wall of Static" "Server 1")
    (let [itd (get-content state :remote1 0)
          wos (get-ice state :remote1 0)]
      (core/rez state :corp itd)
      (core/rez state :corp wos)
      (card-ability state :corp itd 0)
      (is (zero? (:click (get-corp))) "Spent 1 click")
      (is (= 1 (get-counters (refresh itd) :power)) "IT Dept has 1 counter")
      (core/add-counter state :corp (refresh itd) :power 4)
      (is (= 5 (get-counters (refresh itd) :power)) "IT Dept has 5 counters")
      (card-ability state :corp itd 1)
      (click-card state :corp wos)
      ;; refer to online guides for summary of how this ludicrous formula is calculated
      (is (= 8 (:current-strength (refresh wos))) "Gained 5 strength")
      (is (= 4 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 1)
      (click-card state :corp wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 3 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 1)
      (click-card state :corp wos)
      (is (= 12 (:current-strength (refresh wos))) "Gained total of 9 strength")
      (is (= 2 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 1)
      (click-card state :corp wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 1 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (take-credits state :corp)
      (is (= 3 (:current-strength (refresh wos))) "Back to default strength"))))

(deftest jackson-howard
  ;; Jackson Howard - Draw 2 cards
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Jackson Howard"]
                      :discard ["Ice Wall" "Enigma" "Rototurret"]}})
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (let [jhow (get-content state :remote1 0)]
      (core/rez state :corp jhow)
      (is (zero? (count (:hand (get-corp)))))
      (is (= 2 (:click (get-corp))))
      (card-ability state :corp jhow 0)
      (is (= 2 (count (:hand (get-corp)))) "Drew 2 cards")
      (is (= 1 (:click (get-corp))))
      (card-ability state :corp jhow 1)
      (click-card state :corp "Ice Wall")
      (click-card state :corp "Enigma")
      (click-card state :corp "Rototurret")
      (is (find-card "Jackson Howard" (:rfg (get-corp))) "Jackson is rfg'd")
      (is (find-card "Ice Wall" (:deck (get-corp))) "Ice Wall is shuffled back into the deck")
      (is (find-card "Enigma" (:deck (get-corp))) "Enigma is shuffled back into the deck")
      (is (find-card "Rototurret" (:deck (get-corp))) "Rototurret is shuffled back into the deck"))))

(deftest jeeves-model-bioroids
  ;; Jeeves Model Bioroids
  (do-game
    (new-game {:corp {:deck ["Jeeves Model Bioroids" "TGTBT"
                             (qty "Melange Mining Corp." 2)]}
               :runner {:deck [(qty "Ghost Runner" 3)]}})
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
    (gain-tags state :runner 1)
    (core/trash-resource state :corp nil)
    (click-card state :corp (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))))
    (core/trash-resource state :corp nil)
    (click-card state :corp (get-resource state 0))
    (is (= 2 (count (:discard (get-runner)))))
    (core/trash-resource state :corp nil)
    (click-card state :corp (get-resource state 0))
    (is (= 3 (count (:discard (get-runner)))))
    (is (= 1 (:click (get-corp))))))

(deftest kala-ghoda-real-tv
  ;; Kala Ghoda Real TV
  (do-game
    (new-game {:corp {:deck ["Kala Ghoda Real TV"]}})
    (starting-hand state :runner ["Sure Gamble"])
    (play-from-hand state :corp "Kala Ghoda Real TV" "New remote")
    (let [tv (get-content state :remote1 0)]
      (core/rez state :corp tv)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp tv 0)
      (click-prompt state :corp "OK")
      (card-ability state :corp tv 1)
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 1 (count (:discard (get-runner)))))
      (is (last-log-contains? state "Sure Gamble")
          "Kala Ghoda did log trashed card names"))))

(deftest kuwinda-k4h1u3
  ;; Kuwinda K4H1U3
  (do-game
    (new-game {:corp {:deck ["Kuwinda K4H1U3"]}})
    (core/gain state :corp :credit 100)
    (core/gain state :runner :credit 100)
    (play-from-hand state :corp "Kuwinda K4H1U3" "New remote")
    (let [kuwinda (get-content state :remote1 0)]
      (core/rez state :corp kuwinda)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp (refresh kuwinda) 0)
      (is (zero? (:base (prompt-map :corp))) "Base Trace should start at 0")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (zero? (-> (get-runner) :discard count)) "Runner shouldn't take any damage")
      (is (= 1 (get-counters (refresh kuwinda) :power)) "Kuwinda should gain 1 power counter")
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp (refresh kuwinda) 0)
      (is (= 1 (:base (prompt-map :corp))) "Base Trace should now start at 1")
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      (is (zero? (-> (get-runner) :discard count)) "Runner shouldn't take any damage")
      (is (= 2 (get-counters (refresh kuwinda) :power)) "Kuwinda should gain another power counter")
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp (refresh kuwinda) 0)
      (is (= 2 (:base (prompt-map :corp))) "Base Trace should be up to 2")
      (click-prompt state :corp "1")
      (click-prompt state :runner "0")
      (is (= 1 (:brain-damage (get-runner))) "Trace succeeded so runner should take 1 brain damage")
      (is (= 1 (-> (get-runner) :discard count)) "Trace succeeded so runner should discard card from damage")
      (is (= 1 (-> (get-corp) :discard count)) "Kuwinda should be in Archives")
      (is (= "Kuwinda K4H1U3" (-> (get-corp) :discard first :title)) "Kuwinda should be in Archives")
      (core/end-phase-12 state :corp nil))))

(deftest lady-liberty
  ;; Lady Liberty - Score agenda from hand equal to number of power counters on Lady Libery
  (testing "Basic behavior"
    (do-game
      (new-game {:corp {:deck ["Lady Liberty" "Breaking News" "Ikawah Project"]}})
      (play-from-hand state :corp "Lady Liberty" "New remote")
      (let [ll (get-content state :remote1 0)]
        (core/rez state :corp ll)
        (take-credits state :corp)
        (is (zero? (get-counters (refresh ll) :power)) "Lady Liberty starts with 0 counters")
        (take-credits state :runner)
        (is (= 1 (get-counters (refresh ll) :power)) "Lady Liberty gains a power counter on start of turn")
        (is (= 2 (count (:hand (get-corp)))) "Two cards in hand")
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
        (is (= 1 (count (:hand (get-corp)))) "One card in hand")
        (is (= 1 (count (:scored (get-corp)))) "One card in score area")
        (is (= 1 (:agenda-point (get-corp))) "Gained agenda point")
        (take-credits state :corp)
        (take-credits state :runner)
        (card-ability state :corp (refresh ll) 0)
        (is (empty? (:prompt (get-corp))) "No prompt if no matching agenda")
        (take-credits state :corp)
        (take-credits state :runner)
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Ikawah Project" (:hand (get-corp))))
        (is (empty? (:hand (get-corp))) "No cards in hand")
        (is (= 2 (count (:scored (get-corp)))) "Two cards in score area")
        (is (= 4 (:agenda-point (get-corp))) "Gained 3 agenda points"))))
  (testing "Agenda constant effects"
    (do-game
      (new-game {:corp {:deck ["Lady Liberty" "Self-Destruct Chips"]}})
      (play-from-hand state :corp "Lady Liberty" "New remote")
      (let [ll (get-content state :remote1 0)]
        (core/rez state :corp ll)
        (take-credits state :corp)
        (take-credits state :runner)
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Self-Destruct Chips" (:hand (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Gained 1 agenda points")
        (is (= 4 (hand-size :runner)) "Runner hand size reduced by 1"))))
  (testing "Agenda events"
    (do-game
      (new-game {:corp {:deck ["Lady Liberty" "Puppet Master"]}})
      (play-from-hand state :corp "Lady Liberty" "New remote")
      (let [ll (get-content state :remote1 0)]
        (core/rez state :corp ll)
        (dotimes [i 3]
          (take-credits state :corp)
          (take-credits state :runner))
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Puppet Master" (:hand (get-corp))))
        (is (= 3 (:agenda-point (get-corp))) "Gained 3 agenda points")
        (take-credits state :corp)
        (run-empty-server state "HQ")
        (is (= "Select a card to place 1 advancement token on" (:msg (prompt-map :corp))) "Puppet Master event fired")))))

(deftest lakshmi-smartfabrics
  ;; Lakshmi Smartfabrics - Gain power counter when rezzing a card; use counters to protect agenda in HQ
  (do-game
    (new-game {:corp {:deck ["Lakshmi Smartfabrics" "Vanilla"
                             "Marked Accounts" "Elective Upgrade"]}})
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
      (click-card state :corp (find-card "Elective Upgrade" (:hand (get-corp))))
      (is (last-log-contains? state "Elective Upgrade") "Revealed agenda")
      (is (zero? (get-counters (refresh lak) :power)) "Spent 3 power counters")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (empty? (:scored (get-runner))) "Steal prevented by Smartfabrics")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 3 (:agenda-point (get-runner))) "Runner could steal on later turn"))))

(deftest launch-campaign
  ;; Launch Campaign
  (do-game
    (new-game {:corp {:deck ["Launch Campaign"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [launch (get-content state :remote1 0)]
      (core/rez state :corp launch)
      (is (= 4 (:credit (get-corp))))
      (is (= 6 (get-counters (refresh launch) :credit)))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))))
      (is (= 4 (get-counters (refresh launch) :credit))))))

(deftest levy-university
  ;; Levy University
  (do-game
    (new-game {:corp {:deck ["Levy University" "Ice Wall" (qty "Fire Wall" 10)]}})
    (starting-hand state :corp ["Levy University"])
    (play-from-hand state :corp "Levy University" "New remote")
    (let [levy (get-content state :remote1 0)
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
      (core/rez state :corp levy)
      (is (zero? (-> (get-corp) :hand count)) "HQ should be empty")
      (let [clicks (:click (get-corp))
            credits (:credit (get-corp))]
        (card-ability state :corp (refresh levy) 0)
        (click-prompt state :corp (find-card "Ice Wall" (:deck (get-corp))))
        (is (= (dec credits) (:credit (get-corp))) "Levy University ability should cost 1 credit")
        (is (= (dec clicks) (:click (get-corp))) "Levy University ability should cost 1 click"))
      (is (= 1 (-> (get-corp) :hand count)) "HQ should have 1 card")
      (is (= "Ice Wall" (-> (get-corp) :hand first :title)) "HQ should contain Ice Wall")
      (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle deck"))))

(deftest lily-lockwell
  ;; Lily Lockwell
  (do-game
    (new-game {:corp {:deck [(qty "Fire Wall" 10)]
                      :hand ["Lily Lockwell" "Beanstalk Royalties"]
                      :credits 10}
               :runner {:tags 2}})
    (play-from-hand state :corp "Lily Lockwell" "New remote")
    (let [lily (get-content state :remote1 0)
          clicks (:click (get-corp))
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))
          hand (-> (get-corp) :hand count)]
      (core/rez state :corp lily)
      (is (= (+ 3 hand) (-> (get-corp) :hand count)) "Rezzing Lily Lockwell should draw 3 cards")
      (core/move state :corp (find-card "Beanstalk Royalties" (:hand (get-corp))) :deck)
      (card-ability state :corp (refresh lily) 0)
      (click-prompt state :corp "Beanstalk Royalties")
      (is (= "Beanstalk Royalties" (-> (get-corp) :deck first :title)) "Beanstalk Royalties should be moved to top of R&D")
      (is (= 1 (count-tags state)) "Runner should have 1 tag from Lily Lockwell ability")
      (is (= (dec clicks) (:click (get-corp))) "Lily Lockwell ability should cost 1 click")
      (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle deck")
      (core/draw state :corp)
      (card-ability state :corp (refresh lily) 0)
      (click-prompt state :corp "No action")
      (is (last-log-contains? state "does not find") "Lily Lockwell's ability didn't find an operation")
      (is (zero? (count-tags state)) "Runner should have 0 tags from Lily Lockwell ability even when no operation found"))))

(deftest long-term-investment
  ;; Long-Term Investment
  (do-game
    (new-game {:corp {:deck ["Long-Term Investment"]}})
    (play-from-hand state :corp "Long-Term Investment" "New remote")
    (let [lti (get-content state :remote1 0)]
      (core/rez state :corp lti)
      (dotimes [i 4]
        (is (= (* i 2) (get-counters (refresh lti) :credit)) "Long-Term Investement should gain 2 credits at start of turn")
        (take-credits state :corp)
        (take-credits state :runner))
      (is (= 8 (get-counters (refresh lti) :credit)) "Long-Term Investment should have 8 credit after 4 turns")
      (let [credits (:credit (get-corp))]
        (card-ability state :corp (refresh lti) 0)
        (click-prompt state :corp "8")
        (is (= (+ credits 8) (:credit (get-corp))) "Corp should gain 8 credits from Long-Term Investment ability")))))

(deftest malia-z0l0k4
  ;; Malia Z0L0K4 - blank an installed non-virtual runner resource
  (do-game
    (new-game {:corp {:deck [(qty "Malia Z0L0K4" 2)]}
               :runner {:deck ["Rachel Beckman" "Daily Casts" "Rumor Mill"]}})
    (play-from-hand state :corp "Malia Z0L0K4" "New remote")
    (play-from-hand state :corp "Malia Z0L0K4" "New remote")
    (take-credits state :corp)
    (let [malia1 (get-content state :remote1 0)
          malia2 (get-content state :remote2 0)]
      (play-from-hand state :runner "Daily Casts")
      (take-credits state :runner)
      (let [N (:credit (get-runner))]
        (core/rez state :corp malia1)
        (click-card state :corp (get-resource state 0))
        (take-credits state :corp)
        (is (= N (:credit (get-runner))) "Daily casts did not trigger when blanked"))
      (take-credits state :runner)
      (core/derez state :corp malia1)
      (let [N (:credit (get-runner))]
        (take-credits state :corp)
        (is (= (+ N 2) (:credit (get-runner))) "Daily casts triggers again when unblanked"))
      (play-from-hand state :runner "Rachel Beckman")
      (is (= 3 (:click (get-runner))) "Runner doesn't gain a click immediately from playing Beckman")
      (core/rez state :corp malia1)
      (click-card state :corp (get-resource state 1))
      (is (= 3 (:click (get-runner))) "Runner doesn't gain a click when Beckman is blanked")
      (core/derez state :corp malia1)
      (is (= 3 (:click (get-runner))) "Runner still has 3 clicks after Beckman is unblanked")
      (core/rez state :corp malia1)
      (click-card state :corp (get-resource state 1))
      (gain-tags state :corp 1)
      (is (and (= 1 (count-tags state))
               (zero? (count (:discard (get-runner))))) "Runner has 1 tag, but Rachel Beckman not trashed")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-corp)))) "Malia is not in hand")
      (core/move-card state :corp {:card malia1 :server "HQ"})
      (is (= 1 (count (:hand (get-corp)))) "Malia is in hand")
      (is (= 1 (count (:discard (get-runner)))) "Rachel Beckman got trashed on unblanking")
      (core/rez state :corp malia2)
      (click-card state :corp (get-resource state 0))
      (let [N (:credit (get-runner))]
        (take-credits state :corp)
        (is (= N (:credit (get-runner))) "Daily casts is blank, so no drip")))
    (play-from-hand state :runner "Rumor Mill")
    (take-credits state :runner)
    (let [N (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (+ N 2) (:credit (get-runner)))))))

(deftest marilyn-campaign
  ;; Marilyn Campaign
  (do-game
    (new-game {:corp {:deck ["Marilyn Campaign"]}})
    (play-from-hand state :corp "Marilyn Campaign" "New remote")
    (let [marilyn (get-content state :remote1 0)]
      (core/rez state :corp marilyn)
      (is (= 8 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should start with 8 credits")
      (is (zero? (-> (get-corp) :deck count)) "R&D should be empty")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 6 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 4 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (zero? (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (click-prompt state :corp "Yes")
      (is (= 1 (-> (get-corp) :hand count)) "HQ should have 1 card in it, after mandatory draw")
      (is (= "Marilyn Campaign" (-> (get-corp) :hand first :title)) "Marilyn Campaign should be in HQ, after mandatory draw"))))

(deftest mark-yale
  ;; Mark Yale
  (do-game
    (new-game {:corp {:deck ["Mark Yale" "Project Atlas" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Mark Yale" "Project Atlas"])
    (core/gain state :corp :credit 100 :click 100)
    (play-from-hand state :corp "Mark Yale" "New remote")
    (play-from-hand state :corp "Project Atlas" "New remote")
    (let [mark (get-content state :remote1 0)
          atlas (get-content state :remote2 0)]
      (core/rez state :corp mark)
      (advance state atlas 5)
      (core/score state :corp {:card (refresh atlas)}))
    (let [mark (get-content state :remote1 0)
          scored-atlas (get-scored state :corp 0)
          credits (:credit (get-corp))]
      (card-ability state :corp mark 1)
      (click-card state :corp scored-atlas)
      (is (= (+ credits 3) (:credit (get-corp))) "Mark Yale spending an agenda counter should gain 3 credits")
      (card-ability state :corp scored-atlas 0)
      (click-prompt state :corp (find-card "Ice Wall" (:deck (get-corp))))
      (is (= (+ credits 4) (:credit (get-corp))) "Spending an agenda counter for another reason should gain 1 credit")
      (card-ability state :corp mark 0)
      (is (= (+ credits 6) (:credit (get-corp))) "Mark Yale trashing itself should gain 2 credits"))))

(deftest marked-accounts
  ;; Marked Accounts
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Marked Accounts"]}})
      (play-from-hand state :corp "Marked Accounts" "New remote")
      (let [ma (get-content state :remote1 0)]
        (core/rez state :corp ma)
        (is (zero? (get-counters (refresh ma) :credit)) "Marked Accounts should start with 0 credits on it")
        (card-ability state :corp ma 1)
        (is (= 3 (get-counters (refresh ma) :credit)) "Marked Accounts should gain 3 credits when ability is used")
        (take-credits state :corp)
        (let [credits (:credit (get-corp))]
          (take-credits state :runner)
          (is (= (inc credits) (:credit (get-corp))) "Should gain 1 credit at beginning of turn from Marked Accounts")))))
  (testing "Marked Accounts can go negative #4599"
    (do-game
      (new-game {:corp {:hand [(qty "Marked Accounts" 2)]
                        :deck [(qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Marked Accounts" "New remote")
      (play-from-hand state :corp "Marked Accounts" "New remote")
      (let [ma1 (get-content state :remote1 0)
            ma2 (get-content state :remote2 0)
            take-credits-both (fn [state] (doseq [side [:corp :runner]] (take-credits state side)))]
        (core/rez state :corp ma1)
        (core/rez state :corp ma2)
        (is (zero? (get-counters (refresh ma1) :credit)) "First Marked Accounts should start with 0 credits on it")
        (is (zero? (get-counters (refresh ma2) :credit)) "Second Marked Accounts should start with 0 credits on it")
        (card-ability state :corp ma2 1)
        (is (= 3 (get-counters (refresh ma2) :credit)) "Second Marked Accounts should gain 3 credits when ability is used")
        (take-credits-both state)
        (is (= 6 (:credit (get-corp))) "Took 1c from second Marked Accounts, but none from first Marked Accounts")
        (is (zero? (get-counters (refresh ma1) :credit)) "Still no credits on first Marked Accounts")
        (is (= 2 (get-counters (refresh ma2) :credit)) "Two credits left on second Marked Accounts")
        (dotimes [_ 2] (take-credits-both state))
        (is (zero? (get-counters (refresh ma1) :credit)) "Still no credits on first Marked Accounts")
        (is (zero? (get-counters (refresh ma2) :credit)) "No credits left on second Marked Accounts")
        (is (= 14 (:credit (get-corp))) "Took 3c from second Marked Accounts in total and 6c by clicking")
        (card-ability state :corp ma1 1)
        (is (= 3 (get-counters (refresh ma1) :credit)) "First Marked Accounts should gain 3 credits when ability is used")
        (card-ability state :corp ma2 1)
        (is (= 3 (get-counters (refresh ma2) :credit)) "Second Marked Accounts should gain 3 credits when ability is used")))))

(deftest mca-austerity-policy
  (do-game
    (new-game {:corp {:deck ["MCA Austerity Policy"]}})
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

(deftest melange-mining-corp
  ;; Melange Mining Corp.
  (do-game
    (new-game {:corp {:deck ["Melange Mining Corp."]}})
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (take-credits state :corp)
    (take-credits state :runner)
    (core/rez state :corp (get-content state :remote1 0))
    (let [mmc (get-content state :remote1 0)
          credits (:credit (get-corp))]
      (is (= 3 (:click (get-corp))) "Corp should have 3 clicks")
      (card-ability state :corp mmc 0)
      (is (zero? (:click (get-corp))) "Corp should have 0 clicks after using Melange Mining Corp ability")
      (is (= (+ credits 7) (:credit (get-corp))) "Corp should gain 7 credits from Melange Mining Corp ability"))))

(deftest mental-health-clinic
  ;; Mental Health Clinic - Gain 1 credit when turn begins; Runner max hand size increased by 1
  (do-game
    (new-game {:corp {:deck ["Mental Health Clinic"]}})
    (play-from-hand state :corp "Mental Health Clinic" "New remote")
    (let [mhc (get-content state :remote1 0)]
      (core/rez state :corp mhc)
      (is (= 6 (hand-size :runner)) "Runner max hand size increased by 1")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Gained 1 credit at start of turn"))))

(deftest mr-stone
  ;; Mr Stone
  (do-game
    (new-game {:corp {:deck ["Mr. Stone"]}})
    (play-from-hand state :corp "Mr. Stone" "New remote")
    (let [stone (get-content state :remote1 0)]
      (core/rez state :corp stone)
      (gain-tags state :runner 1)
      (is (= 1 (-> (get-runner) :discard count)) "Runner should take 1 meat damage from gaining 1 tag")
      (gain-tags state :corp 5)
      (is (= 2 (-> (get-runner) :discard count)) "Runner should take 1 meat damage from gaining 5 tags"))))

(deftest mumba-temple
  ;; Mumba Temple
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Mumba Temple"]}})
      (play-from-hand state :corp "Mumba Temple" "New remote")
      (let [mumba (get-content state :remote1 0)]
        (core/rez state :corp mumba)
        (is (= 2 (get-counters (refresh mumba) :recurring)) "Should have 2 recurring credits"))))
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:deck ["Mumba Temple" "Ice Wall" "PAD Campaign"]}})
      (play-from-hand state :corp "Mumba Temple" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [mumba (get-content state :remote1 0)
            pad (get-content state :remote2 0)
            iw (get-ice state :hq 0)]
        (core/rez state :corp mumba)
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 1 credit from Mumba"
                           (core/rez state :corp iw)
                           (click-card state :corp mumba))
        (is (empty? (:prompt (get-corp))) "Rezzing done")
        (changes-val-macro -1 (:credit (get-corp)) ; 1 credit left on Mumba
                           "Used 1 credit from Mumba"
                           (core/rez state :corp pad)
                           (click-card state :corp mumba))))))

(deftest mumbad-city-hall
  ;; Mumbad City Hall
  (do-game
    (new-game {:corp {:deck ["Mumbad City Hall"
                             "PAD Factory"
                             "Salem's Hospitality"]}})
    (core/gain state :corp :click 3 :credit 100)
    (starting-hand state :corp ["Mumbad City Hall"])
    (play-from-hand state :corp "Mumbad City Hall" "New remote")
    (let [mumbad (get-content state :remote1 0)]
      (core/rez state :corp mumbad)
      (card-ability state :corp mumbad 0)
      (click-prompt state :corp (find-card "PAD Factory" (:deck (get-corp))))
      (click-prompt state :corp "New remote")
      (is (= "PAD Factory" (:title (get-content state :remote2 0))))
      (card-ability state :corp mumbad 0)
      (click-prompt state :corp (find-card "Salem's Hospitality" (:deck (get-corp))))
      (click-prompt state :corp "Sure Gamble")
      (is (= 3 (-> (get-runner) :discard count)) "Runner should have discarded all cards from Salem's Hospitality"))))

(deftest mumbad-construction-co
  ;; Mumbad Construction Co.
  (do-game
    (new-game {:corp {:deck ["Mumbad Construction Co."
                             "Oaktown Renovation"]}})
    (play-from-hand state :corp "Mumbad Construction Co." "New remote")
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [mcc (get-content state :remote1 0)
          oak (get-content state :remote2 0)]
      (core/rez state :corp mcc)
      (is (zero? (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should start with 0 counters")
      (is (zero? (get-counters (refresh oak) :advancement)) "Oaktown Renovation should start with 0 counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should gain 1 counter at start of turn")
      (let [credits (:credit (get-corp))]
        (card-ability state :corp mcc 0)
        (click-card state :corp (refresh oak))
        (is (zero? (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should lose 1 counter when using ability")
        (is (= 1 (get-counters (refresh oak) :advancement)) "Oaktown Renovation should gain 1 counter from MCC ability")
        (is (= (- credits 2) (:credit (get-corp))) "Mumbad Construction Co ability should cost 2 [Credits]")))))

(deftest museum-of-history
  ;; Museum of History
  (do-game
    (new-game {:corp {:deck ["Museum of History" "Beanstalk Royalties"
                             (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Beanstalk Royalties" "Museum of History"])
    (play-from-hand state :corp "Beanstalk Royalties")
    (play-from-hand state :corp "Museum of History" "New remote")
    (let [museum (get-content state :remote1 0)
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
      (core/rez state :corp museum)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp museum 0)
      (click-card state :corp (find-card "Beanstalk Royalties" (:discard (get-corp))))
      (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle deck")
      (is (zero? (-> (get-corp) :discard count)) "Archives should be empty after shuffling Beanstalk into R&D"))))

(deftest nanoetching-matrix
  ;; Nanoetching Matrix - click for 2c once per turn. Gain 2c when Runner trashes it.
  (do-game
    (new-game {:corp {:deck ["Nanoetching Matrix"]}})
    (play-from-hand state :corp "Nanoetching Matrix" "New remote")
    (let [nm (get-content state :remote1 0)]
      (core/rez state :corp (refresh nm))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp (refresh nm) 0)
        (is (= (:credit (get-corp)) (+ 2 credits)) "Gain 2c from ability")
        (card-ability state :corp (refresh nm) 0)
        (is (= (:credit (get-corp)) (+ 2 credits)) "Can only use once per turn")
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (let [new-credits (:credit (get-corp))]
          (is (zero? (count (:discard (get-corp)))) "Nothing trashed")
          (click-prompt state :runner "Pay 3 [Credits] to trash")
          (click-prompt state :corp "Yes")
          (is (= 1 (count (:discard (get-corp)))) "Nanoetching Matrix trashed")
          (is (= (:credit (get-corp)) (+ 2 new-credits)) "Gain 2c when runner trashes"))))))

(deftest nasx
  ;; NASX
  (do-game
    (new-game {:corp {:deck ["NASX"]}})
    (play-from-hand state :corp "NASX" "New remote")
    (let [nasx (get-content state :remote1 0)]
      (core/rez state :corp nasx)
      (take-credits state :corp)
      (let [credits (:credit (get-corp))]
        (take-credits state :runner)
        (is (= (inc credits) (:credit (get-corp))) "Corp should gain 1 credit at start of turn from NASX"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp nasx 1)
        (is (= (dec credits) (:credit (get-corp))) "Corp should spend 1 credit on NASX ability")
        (is (= 1 (get-counters (refresh nasx) :power)) "NASX should gain 1 power counter from spent credits"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp nasx 2)
        (is (= (- credits 2) (:credit (get-corp))) "Corp should spend 2 credit on NASX ability")
        (is (= 3 (get-counters (refresh nasx) :power)) "NASX should gain 2 power counter from spent credits"))
      (let [credits (:credit (get-corp))
            counters (get-counters (refresh nasx) :power)]
        (card-ability state :corp nasx 3)
        (is (= (+ credits (* 2 counters)) (:credit (get-corp))) (str "Corp should gain " (* 2 counters) " from NASX trash ability"))
        (is (= 1 (-> (get-corp) :discard count)) "Corp should trash NASX for ability")
        (is (= "NASX" (-> (get-corp) :discard first :title)) "NASX should be in archives")))))

(deftest net-analytics
  ;; Draw a card when runner avoids or removes 1 or more tags
  (do-game
    (new-game {:corp {:deck [(qty "Ghost Branch" 3) (qty "Net Analytics" 3)]}
               :runner {:deck [(qty "New Angeles City Hall" 3)]}})
    (starting-hand state :corp ["Net Analytics" "Ghost Branch"])
    (play-from-hand state :corp "Ghost Branch" "New remote")
    (play-from-hand state :corp "Net Analytics" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "New Angeles City Hall")
    (take-credits state :runner)
    (let [gb (get-content state :remote1 0)
          net (get-content state :remote2 0)
          nach (get-resource state 0)]
      (core/rez state :corp (refresh net))
      (core/advance state :corp {:card (refresh gb)})
      (is (= 1 (get-counters (refresh gb) :advancement)))
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))) "Corp hand size is 1 before run")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes") ; Ghost Branch ability
      (card-ability state :runner nach 0)
      (click-prompt state :corp "Yes") ; Draw from Net Analytics
      (click-prompt state :runner "Done")
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
      (is (zero? (count-tags state)) "Avoided 1 Ghost Branch tag")
      (is (= 2 (count (:hand (get-corp)))) "Corp draw from NA")
      ; tag removal
      (gain-tags state :runner 1)
      (click-prompt state :runner "Done") ; Don't prevent the tag
      (core/remove-tag state :runner 1)
      (click-prompt state :corp "Yes") ; Draw from Net Analytics
      (is (= 3 (count (:hand (get-corp)))) "Corp draw from NA"))))

(deftest net-police
  ;; Net Police - Recurring credits equal to Runner's link
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Net Police" "Snatch and Grab"]}
               :runner {:id "Sunny Lebeau: Security Specialist"
                        :deck ["Dyson Mem Chip"
                               "Access to Globalsec"]}})
    (play-from-hand state :corp "Net Police" "New remote")
    (is (= 2 (:link (get-runner))))
    (let [netpol (get-content state :remote1 0)]
      (core/rez state :corp netpol)
      (is (= 2 (get-counters (refresh netpol) :recurring)) "2 recurring for Runner's 2 link")
      (take-credits state :corp)
      (play-from-hand state :runner "Dyson Mem Chip")
      (take-credits state :runner)
      (is (= 3 (get-counters (refresh netpol) :recurring)) "3 recurring for Runner's 3 link")
      (take-credits state :corp)
      (play-from-hand state :runner "Access to Globalsec")
      (take-credits state :runner)
      (is (= 4 (get-counters (refresh netpol) :recurring)) "4 recurring for Runner's 4 link")
      (play-from-hand state :corp "Snatch and Grab")
      (is (= (+ (:credit (get-corp)) (get-counters (refresh netpol) :recurring))
             (:choices (prompt-map :corp))) "13 total available credits for the trace")
      (click-prompt state :corp "13")
      (dotimes [_ 4]
        (click-card state :corp netpol))
      (is (zero? (get-counters (refresh netpol) :recurring)) "Has used recurring credit")
      (is (= 16 (:strength (prompt-map :runner))) "Current trace strength should be 16"))))

(deftest neurostasis
  ;; Neurostasis - ambush, shuffle cards into the stack
  (do-game
    (new-game {:corp {:deck ["Neurostasis"]}
               :runner {:deck [(qty "Cache" 3)]}})
    (play-from-hand state :corp "Neurostasis" "New remote")
    (let [neuro (get-content state :remote1 0)]
      ;; Single advance Neurostasis
      (core/advance state :corp {:card (refresh neuro)})
      (take-credits state :corp)
      ;; Run on Neurostasis with 3 programs
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (run-empty-server state "Server 1")
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :corp "Yes")
      ;; Corp can shuffle one program
      (click-card state :corp (get-program state 1))
      (click-prompt state :runner "No action")
      ;; There should be two Caches left
      (is (= 2 (:credit (get-corp))) "Spent 3 credits to fire ambush")
      (is (= 2 (count (get-program state))) "Removed one installed program")
      (is (= 1 (count (:deck (get-runner)))) "Shuffled one program into the stack")
      (take-credits state :runner)
      (core/advance state :corp {:card (refresh neuro)})
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 3 (:credit (get-corp))) "Corp starts with 3 credits")
      (click-prompt state :corp "Yes")
      ;; Corp can shuffle two programs
      (click-card state :corp (get-program state 1))
      (click-card state :corp (get-program state 0))
      (is (zero? (:credit (get-corp))) "Spent 3 credits to fire ambush")
      (is (empty? (get-program state)) "Removed one installed program")
      (is (= 3 (count (:deck (get-runner)))) "Shuffled two programs into the stack"))))

(deftest news-team
  ;; News Team - on access take 2 tags or take as agenda worth -1
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "News Team" 3) "Blacklist"]}})
      (trash-from-hand state :corp "News Team")
      (play-from-hand state :corp "Blacklist" "New remote")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (click-prompt state :runner "Take 2 tags")
      (is (= 2 (count-tags state)) "Runner has 2 tags")
      (run-empty-server state :archives)
      (click-prompt state :runner "Add News Team to score area")
      (is (= 1 (count (:scored (get-runner)))) "News Team added to Runner score area")
      (trash-from-hand state :corp "News Team")
      (core/rez state :corp (get-content state :remote1 0))
      (run-empty-server state :archives)
      (click-prompt state :runner "Add News Team to score area")
      (is (= 2 (count (:scored (get-runner)))) "News Team added to Runner score area with Blacklist rez")))
  (testing "interaction with Maw. Issue #4214"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Government Takeover" "News Team"]}
                 :runner {:hand ["Maw"]
                          :credits 10}})
      (play-from-hand state :corp "News Team" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Maw")
      (is (zero? (count (:discard (get-corp)))) "Corp has 0 cards in archives to start")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Add News Team to score area")
      (is (= 1 (count (:discard (get-corp)))) "Corp discards a card from hand")
      (is (= "Government Takeover" (-> (get-corp) :discard first :title))
          "Corp discards card from hand from Maw"))))

(deftest ngo-front
  ;; NGO Front - full test
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "NGO Front" 3)]}})
      (core/gain state :corp :click 3)
      (play-from-hand state :corp "NGO Front" "New remote")
      (play-from-hand state :corp "NGO Front" "New remote")
      (play-from-hand state :corp "NGO Front" "New remote")
      (let [ngo1 (get-content state :remote1 0)
            ngo2 (get-content state :remote2 0)
            ngo3 (get-content state :remote3 0)]
        (advance state ngo2)
        (advance state (refresh ngo3))
        (advance state (refresh ngo3))
        (core/rez state :corp (refresh ngo1))
        (core/rez state :corp (refresh ngo2))
        (core/rez state :corp (refresh ngo3))
        (is (= 2 (:credit (get-corp))) "Corp at 2 credits")
        (card-ability state :corp ngo1 1)
        (card-ability state :corp ngo1 0)
        (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
        (is (zero? (count (:discard (get-corp)))) "Nothing trashed")
        (card-ability state :corp ngo2 1)
        (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
        (is (zero? (count (:discard (get-corp)))) "Nothing trashed")
        (card-ability state :corp ngo2 0)
        (is (= 7 (:credit (get-corp))) "Corp gained 5 credits")
        (is (= 1 (count (:discard (get-corp)))) "1 NGO Front Trashed")
        (card-ability state :corp ngo3 1)
        (is (= 15 (:credit (get-corp))) "Corp gained 8 credits")
        (is (= 2 (count (:discard (get-corp)))) "2 NGO Front Trashed"))))
  (testing "Run ends when used mid-run"
    (do-game
      (new-game {:corp {:deck ["NGO Front"]}})
      (play-from-hand state :corp "NGO Front" "New remote")
      (let [ngo (get-content state :remote1 0)]
        (core/rez state :corp ngo)
        (advance state (refresh ngo))
        (take-credits state :corp)
        (run-on state :remote1)
        (card-ability state :corp ngo 0)
        (is (nil? (refresh ngo)))
        (is (nil? (:run @state)))))))

(deftest open-forum
  ;; Open Forum
  (do-game
    (new-game {:corp {:deck ["Open Forum" "Ice Wall" "Fire Wall" "Enigma"]}})
    (play-from-hand state :corp "Open Forum" "New remote")
    (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
    (is (-> @state :corp :hand count zero?))
    (let [forum (get-content state :remote1 0)]
      (core/rez state :corp forum)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (last-log-contains? state "Fire Wall") "Mandatory Draw was Ice Wall, Open Forum should reveal Fire Wall")
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (is (= 2 (-> @state :corp :deck count)) "Two cards should remain in R&D")
      (is (= "Ice Wall" (-> @state :corp :deck first :title)) "Top card in R&D should be Ice Wall"))))

(deftest pad-campaign
  ;; PAD Campaign
  (do-game
    (new-game {:corp {:deck ["PAD Campaign"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :corp pad)
      (take-credits state :corp)
      (let [credits (:credit (get-corp))]
        (take-credits state :runner)
        (is (= (inc credits) (:credit (get-corp))) "Should gain 1 credit at start of turn from PAD Campaign")))))

(deftest pad-factory
  ;; PAD Factory - Click to place an advancement, cannot score target until next turn
  (do-game
    (new-game {:corp {:deck ["PAD Factory" "15 Minutes"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "PAD Factory" "New remote")
    (play-from-hand state :corp "15 Minutes" "New remote")
    (let [pf (get-content state :remote1 0)
          fif (get-content state :remote2 0)]
      (core/rez state :corp pf)
      (card-ability state :corp (refresh pf) 0)
      (click-card state :corp fif)
      (card-ability state :corp (refresh pf) 0)
      (click-card state :corp (refresh fif))
      (is (zero? (:click (get-corp))) "Spent 2 clicks using PAD Factory twice")
      (is (= 2 (get-counters (refresh fif) :advancement)) "Agenda has 2 advancements")
      (core/score state :corp {:card (refresh fif)})
      (is (empty? (:scored (get-corp))) "Prevented from scoring this turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/score state :corp {:card (refresh fif)})
      (is (= 1 (count (:scored (get-corp)))) "Scored agenda"))))

(deftest palana-agroplex
  ;; Pālanā Agroplex - Both players draw 1 at start of Corp turn
  (do-game
    (new-game {:corp {:deck ["Pālanā Agroplex" (qty "Hedge Fund" 3)]}})
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
  (testing "Vanilla test"
    (do-game
     (new-game {:corp {:deck ["Personalized Portal"]}
                :runner {:deck [(qty "Daily Casts" 3) (qty "Dyson Mem Chip" 3)]}})
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
  (testing "When paired with The Class Act"
    (do-game
     (new-game {:corp {:deck ["Personalized Portal"]}
                :runner {:deck [(qty "Daily Casts" 5) "The Class Act" "Motivation"]}})
     (starting-hand state :runner ["The Class Act" "Motivation"])
     (play-from-hand state :corp "Personalized Portal" "New remote")
     (core/rez state :corp (get-content state :remote1 0))
     (take-credits state :corp)
     (play-from-hand state :runner "The Class Act")
     (core/move state :runner (find-card "Motivation" (:hand (get-runner))) :deck {:front true}) ;ensure easy mark is on the top
     (is (= "Motivation" (-> (get-runner) :deck first :title)) "Motivation is on top of deck")
     (is (empty? (:hand (get-runner))) "Runner's grip is empty to start")
     (is (= 4 (:credit (get-corp))) "Corp starts with 4 credits")
     (core/lose state :runner :click 3)
     (is (empty? (:hand (get-runner))) "Runner's grip is still empty")
     (core/end-turn state :runner nil)
     (is (= 5 (count (prompt-buttons :runner))) "Runner has 5 card choices")
     (is (= 6 (count (:deck (get-runner)))) "No cards have been drawn yet")
     (is (not (empty? (:prompt (get-runner)))) "Runner prompted to be classy")
     (is (not (empty? (:prompt (get-corp)))) "Corp waiting for Runner to be classy")
     (click-prompt state :runner "Motivation")
     (is (empty? (:prompt (get-runner))) "Runner done being classy")
     (is (empty? (:prompt (get-corp))) "Corp not waiting for Runner to be classy")
     (core/start-turn state :corp nil) ;; this causes portals to trigger
     (is (= 2 (count (prompt-buttons :runner))) "Runner has 2 card choices")
     (is (= 2 (count (:deck (get-runner)))) "2 cards in deck before drawing")
     (is (= 4 (:credit (get-corp))) "Corp has not gained credits yet")
     (click-prompt state :runner "Motivation")
     (is (= 5 (count (:hand (get-runner)))) "Runner is sitting on 5 cards after bottoming a card")
     (is (= 6 (:credit (get-corp))) "Corp only gained 5/2 = 2 credits, not 3")
     (is (empty? (:prompt (get-runner))) "Runner not prompted")
     (is (empty? (:prompt (get-corp))) "Corp not waiting for Runner to be classy"))))

(deftest plan-b
  ;; Plan B - score agenda with adv cost <= # of adv counters
  (do-game
    (new-game {:corp {:deck ["Plan B"
                             "Braintrust"
                             "The Future Perfect"
                             "Mushin No Shin"]}})
    (play-from-hand state :corp "Mushin No Shin")
    (click-card state :corp (find-card "Plan B" (:hand (get-corp))))
    (take-credits state :corp)
    (run-empty-server state :remote1)
    ;; prompt for corp to use Plan B
    (click-prompt state :corp "Yes")
    ;; Pick TFP, does not score
    (click-card state :corp (find-card "The Future Perfect" (:hand (get-corp))))
    (is (find-card "The Future Perfect" (:hand (get-corp))) "TFP is not scored")
    ;; Pick Brain Trust, scores
    (click-card state :corp (find-card "Braintrust" (:hand (get-corp))))
    (is (find-card "Braintrust" (:scored (get-corp))) "Braintrust is scored")))

(deftest political-dealings
  ;; Political Dealings
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck ["Political Dealings" "Medical Breakthrough" "Oaktown Renovation"]}})
      (core/move state :corp (find-card "Medical Breakthrough" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Oaktown Renovation" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Political Dealings" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      ;; Install Medical Breakthrough
      (core/draw state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "New remote")
      (is (= "Medical Breakthrough" (:title (get-content state :remote2 0)))
          "Medical Breakthrough installed by Political Dealings")
      ;; Install Oaktown Renovation
      (core/draw state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "New remote")
      (is (= "Oaktown Renovation" (:title (get-content state :remote3 0)))
          "Oaktown Renovation installed by Political Dealings")
      (is (rezzed? (get-content state :remote3 0))
          "Oaktown Renovation installed face up")))
  (testing "Daily Business Show interaction - Draw 2 agendas, install both of them but return 1 to bottom of R&D"
    (do-game
      (new-game {:corp {:deck ["Political Dealings" "Daily Business Show" "Turtlebacks"
                               "Breaking News" "Project Beale"]}})
      (starting-hand state :corp ["Political Dealings" "Daily Business Show" "Turtlebacks"])
      (core/gain state :corp :credit 3)
      (play-from-hand state :corp "Political Dealings" "New remote")
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Turtlebacks" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (take-credits state :corp)
      (is (zero? (count (:hand (get-corp)))))
      (let [agenda1 (first (:deck (get-corp)))
            agenda2 (second (:deck (get-corp)))]
        (take-credits state :runner)
        ;; Install first agenda
        (is (= 2 (count (:hand (get-corp)))))
        (is (zero? (:credit (get-corp))))
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "New remote")
        (is (= (:title agenda1) (:title (get-content state :remote4 0))))
        (is (= 1 (:credit (get-corp))) "Turtlebacks triggered")
        ;; Install second agenda
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "New remote")
        (is (= (:title agenda2) (:title (get-content state :remote5 0))))
        (is (= 2 (:credit (get-corp))) "Turtlebacks triggered")
        ;; DBS - put first agenda at bottom of R&D
        (click-card state :corp (get-content state :remote4 0))
        (is (zero? (count (:hand (get-corp)))))
        (is (= (:title agenda1) (:title (last (:deck (get-corp))))))))))

(deftest prana-condenser
  ;; Prāna Condenser
  (testing "Basic test"
    (do-game
      (new-game {:corp {:hand ["Prāna Condenser" (qty "Neural EMP" 2)]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Prāna Condenser" "New remote")
      (let [pc (get-content state :remote1 0)]
        (core/rez state :corp pc)
        (take-credits state :corp)
        (run-empty-server state :archives)
        (take-credits state :runner)
        (play-from-hand state :corp "Neural EMP")
        (let [corp-credits (:credit (get-corp))]
          (is (= 5 (count (:hand (get-runner)))) "No damage dealt")
          (click-prompt state :corp "Yes")
          (is (= 1 (get-counters (refresh pc) :power)) "Added 1 power token")
          (is (= (+ 3 corp-credits) (:credit (get-corp))) "Gained 3 credits")
          (play-from-hand state :corp "Neural EMP")
          (is (= 5 (count (:hand (get-runner)))) "No damage dealt")
          (click-prompt state :corp "Yes")
          (is (= 2 (get-counters (refresh pc) :power)) "Added another power token")
          (is (= (+ 4 corp-credits) (:credit (get-corp))) "Gained another 3 credits (and paid 2 for EMP)")
          (is (= 5 (count (:hand (get-runner)))) "No damage dealt"))
        (take-credits state :runner)
        (card-ability state :corp (refresh pc) 0)
        (is (= 3 (count (:hand (get-runner)))) "2 damage dealt"))))
  (testing "Refuse to prevent damage"
    (do-game
      (new-game {:corp {:hand ["Prāna Condenser" "Neural EMP"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Prāna Condenser" "New remote")
      (let [pc (get-content state :remote1 0)]
        (core/rez state :corp pc)
        (take-credits state :corp)
        (run-empty-server state :archives)
        (take-credits state :runner)
        (play-from-hand state :corp "Neural EMP")
        (let [corp-credits (:credit (get-corp))]
          (is (= 5 (count (:hand (get-runner)))) "No damage dealt")
          (click-prompt state :corp "No")
          (is (= 4 (count (:hand (get-runner)))) "1 net damage dealt")
          (is (= 0 (get-counters (refresh pc) :power)) "No power token added")
          (is (= corp-credits (:credit (get-corp))) "No credits gained")))))
  (testing "Only prevents 1 net damage"
    (do-game
      (new-game {:corp {:hand ["Prāna Condenser" "Snare!"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Prāna Condenser" "New remote")
      (let [pc (get-content state :remote1 0)]
        (core/rez state :corp pc)
        (take-credits state :corp)
        (run-empty-server state :hq)
        (is (= 5 (count (:hand (get-runner)))) "No damage dealt")
        (click-prompt state :corp "Yes") ;Snare
        (click-prompt state :corp "Yes") ;Prana
        (is (= 3 (count (:hand (get-runner)))) "2 net damage dealt")
        (is (= 1 (get-counters (refresh pc) :power)) "Only 1 power token added"))))
  (testing "Runner preventing damage on their turn"
    (do-game
      (new-game {:corp {:hand ["Prāna Condenser" "Shock!"]}
                 :runner {:hand [(qty "Caldera" 5)]
                          :credits 6}})
      (play-from-hand state :corp "Prāna Condenser" "New remote")
      (let [pc (get-content state :remote1 0)]
        (core/rez state :corp pc)
        (take-credits state :corp)
        (play-from-hand state :runner "Caldera")
        (is (= 4 (count (:hand (get-runner)))) "Runner starts with 4 cards in grip")
        (run-empty-server state :hq)
        (card-ability state :runner (get-resource state 0) 0)
        (click-prompt state :runner "No action")
        (is (= 4 (count (:hand (get-runner)))) "Runner took no damage")
        (is (empty? (:prompt (get-corp))) "No Prana prompt for Corp"))))
  (testing "Corp gets Prana prompt first"
    (do-game
      (new-game {:corp {:hand ["Prāna Condenser" "Neural EMP"]}
                 :runner {:hand [(qty "Caldera" 5)]
                          :credits 6}})
      (play-from-hand state :corp "Prāna Condenser" "New remote")
      (let [pc (get-content state :remote1 0)]
        (core/rez state :corp pc)
        (take-credits state :corp)
        (play-from-hand state :runner "Caldera")
        (run-empty-server state :archives)
        (take-credits state :runner)
        (play-from-hand state :corp "Neural EMP")
        ; TODO: Implement interrupts to make these prompts work correctly
        ; (is (not-empty (:prompt (get-corp))) "Prana prompt for Corp")
        ; (is (empty? (:prompt (get-runner))) "No Caldera prompt for Runner")
        ))))

(deftest primary-transmission-dish
  ;; Primary Transmission Dish
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Primary Transmission Dish" "Snatch and Grab"]}
               :runner {:hand ["Kati Jones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (take-credits state :runner)
    (play-from-hand state :corp "Primary Transmission Dish" "New remote")
    (let [dish (get-content state :remote1 0)
          kati (get-resource state 0)]
      (core/rez state :corp dish)
      (is (= 3 (get-counters (refresh dish) :recurring)) "Should have 3 recurring credits")
      (play-from-hand state :corp "Snatch and Grab")
      (is (= (+ (:credit (get-corp)) (get-counters (refresh dish) :recurring))
             (:choices (prompt-map :corp))) "9 total available credits for the trace")
      (click-prompt state :corp "9")
      (dotimes [_ 3]
        (click-card state :corp dish))
      (is (zero? (get-counters (refresh dish) :recurring)) "Has used recurring credit")
      (is (= 12 (:strength (prompt-map :runner))) "Current trace strength should be 12")
      (click-prompt state :runner "0")
      (is (refresh kati) "Kati Jones still installed")
      (click-card state :corp "Kati Jones")
      (click-prompt state :runner "No")
      (is (nil? (refresh kati)) "Kati Jones no longer installed"))))

(deftest private-contracts
  ;; Private Contracts
  (do-game
    (new-game {:corp {:deck ["Private Contracts"]}})
    (play-from-hand state :corp "Private Contracts" "New remote")
    (let [pri (get-content state :remote1 0)]
      (core/rez state :corp pri)
      (is (= 14 (get-counters (refresh pri) :credit)) "Should start with 14 credits")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have 0 cards in Archives")
      (core/gain state :corp :click 7)
      (core/lose state :corp :credit 2)
      (dotimes [_ 7]
        (card-ability state :corp pri 0))
      (is (= 1 (-> (get-corp) :discard count)) "Private Contracts should be in discard")
      (is (= 14 (:credit (get-corp))) "Corp should now have 14 credits"))))

(deftest project-junebug
  ;; Project Junebug
  (do-game
    (new-game {:corp {:deck ["Project Junebug"]}
               :runner {:deck [(qty "Sure Gamble" 100)]}})
    (play-from-hand state :corp "Project Junebug" "New remote")
    (advance state (get-content state :remote1 0) 2)
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (let [credits (:credit (get-corp))]
      (click-prompt state :corp "Yes")
      (is (= (dec credits) (:credit (get-corp))) "Corp should pay 1 for Project Junebug ability")
      (is (= 4 (-> (get-runner) :discard count)) "Project Junebug should do 4 net damage"))))

(deftest psychic-field
  ;; Psychic Field - Do 1 net damage for every card in Runner's hand when accessed/exposed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Psychic Field" 2)]}
                 :runner {:deck [(qty "Infiltration" 3) (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (play-from-hand state :corp "Psychic Field" "New remote")
      (let [psyf1 (get-content state :remote1 0)
            psyf2 (get-content state :remote2 0)]
        (take-credits state :corp)
        (starting-hand state :runner ["Infiltration" "Sure Gamble" "Sure Gamble"])
        (play-from-hand state :runner "Infiltration")
        (click-prompt state :runner "Expose a card")
        (click-card state :runner psyf1)
        (is (= 2 (count (:hand (get-runner)))))
        (click-prompt state :corp "2 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 3 (count (:discard (get-runner)))) "Suffered 2 net damage on expose and psi loss")
        (core/gain state :runner :click 3)
        (core/draw state :runner 3)
        (is (= 3 (count (:hand (get-runner)))))
        (run-empty-server state :remote2)
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 6 (count (:discard (get-runner)))) "Suffered 3 net damage on access and psi loss"))))
  (testing "when in Archives. #1965"
    (do-game
      (new-game {:corp {:deck [(qty "Psychic Field" 2) (qty "Shock!" 2) (qty "Clone Retirement" 2)]}})
      (trash-from-hand state :corp "Psychic Field")
      (trash-from-hand state :corp "Shock!")
      (trash-from-hand state :corp "Clone Retirement")
      (take-credits state :corp)
      ;; Runner run on archives to trigger access choice
      (run-empty-server state :archives)
      (is (not-any? #{"Psychic Field"} (prompt-buttons :runner))
          "Psychic Field is not a choice to access in Archives")))
  (testing "Interaction with Neutralize All Threats and Hostile Infrastructure, #1208"
    (do-game
      (new-game {:corp {:deck [(qty "Psychic Field" 3) (qty "Hostile Infrastructure" 3)]}
                 :runner {:deck ["Neutralize All Threats" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Neutralize All Threats")
      (run-empty-server state :remote1)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (not (get-content state :remote1)) "Psychic Field trashed by Neutralize All Threats")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))
  (testing "Interaction with Fumiko Yamamori"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Psychic Field" "Fumiko Yamamori"]
                        :credit 10}
                 :runner {:hand [(qty "Sure Gamble" 2)]
                          :credit 10}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (play-from-hand state :corp "Fumiko Yamamori" "New remote")
      (let [field (get-content state :remote1 0)
            fumiko (get-content state :remote2 0)]
        (core/rez state :corp fumiko)
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (is (= 2 (count (:hand (get-runner)))))
        (is (not (:winner @state)) "No one has won yet")
        (click-prompt state :corp "2 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 2 (count (:discard (get-runner)))) "Suffered 2 net damage on expose and psi loss")
        (is (= :corp (:winner @state)) "Corp wins because of 3 damage")))))

(deftest public-health-portal
  (do-game
    (new-game {:corp {:deck ["Public Health Portal" "Aiki" "Ben Musashi" "Celebrity Gift"]}})
    (play-from-hand state :corp "Public Health Portal" "New remote")
    (core/move state :corp (find-card "Aiki" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Ben Musashi" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Celebrity Gift" (:hand (get-corp))) :deck)
    (is (-> @state :corp :hand count zero?))
    (let [php (get-content state :remote1 0)]
      (core/rez state :corp php)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (last-log-contains? state "Aiki") "Public Health Portal should reveal Aiki")
      (is (= "Ben Musashi" (-> @state :corp :deck first :title)) "Top card in R&D should be Ben Musashi"))))

(deftest public-support
  ;; Public support scoring and trashing
  ;; TODO could also test for NOT triggering "when scored" events
  (do-game
    (new-game {:corp {:deck [(qty "Public Support" 2)]}})
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
      (is (zero? (:agenda-point (get-corp))))
      (is (nil? (:agendapoints (refresh publics1))))
      (take-credits state :corp)
      ;; Runner turn 2, run and trash publics2
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash
      (is (= 5 (:credit (get-runner))))
      (take-credits state :runner)
      ;; Corp turn 3, check how publics1 is doing
      (is (= 1 (get-counters (refresh publics1) :power)))
      (is (zero? (:agenda-point (get-corp))))
      (take-credits state :corp)
      ;; Runner turn 3, boring
      (take-credits state :runner)
      ;; Corp turn 4, check the delicious agenda points
      (let [scored-pub (get-scored state :corp 0)]
        (is (= 1 (:agenda-point (get-corp))) "Gained 1 agenda point")
        (is (= "Public Support" (:title scored-pub)))
        (is (= 1 (:agendapoints scored-pub)))))))

(deftest quarantine-system
  ;; Forfeit agenda to rez up to 3 ICE with 2 credit discount per agenda point
  (do-game
    (new-game {:corp {:deck [(qty "Chiyashi" 3) "Quarantine System" "Project Beale"]}})
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
      (click-card state :corp (get-scored state :corp 0))
      (click-card state :corp ch1)
      (click-card state :corp ch2)
      (click-card state :corp ch3)
      ; pay 8 per Chiyashi - 24 total
      (is (= 77 (:credit (get-corp))) "Corp has 77 creds")
      (is (empty? (:prompt (get-corp))) "No prompt to rez ICE"))))

(deftest raman-rai
  ;; Raman Rai
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]
                      :hand ["Raman Rai"]
                      :discard ["Fire Wall"]}})
    (play-from-hand state :corp "Raman Rai" "New remote")
    (let [raman (get-content state :remote1 0)]
      (core/rez state :corp raman)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp raman 0)
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-card state :corp (find-card "Fire Wall" (:discard (get-corp))))
      (is (= "Fire Wall" (-> (get-corp) :hand first :title)))
      (is (= "Ice Wall" (-> (get-corp) :discard first :title))))))

(deftest rashida-jaheem
  ;; Rashida Jaheem
  (testing "when there are enough cards in R&D"
    (do-game
      (new-game {:corp {:deck ["Rashida Jaheem" (qty "Hedge Fund" 3)]}})
      (starting-hand state :corp ["Rashida Jaheem"])
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (let [credits (:credit (get-corp))
            cards (count (:hand (get-corp)))
            rj (get-content state :remote1 0)]
        (card-ability state :corp rj 0)
        (click-prompt state :corp "Yes")
        (is (= (+ 3 credits) (:credit (get-corp))))
        (is (= (+ 3 cards) (count (:hand (get-corp))))))))
  (testing "when there aren't enough cards in R&D"
    (do-game
      (new-game {:corp {:deck ["Rashida Jaheem" (qty "Hedge Fund" 4)]}})
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
        (click-prompt state :corp "Yes")
        (is (= (+ 3 credits) (:credit (get-corp))))
        (is (= (+ 2 cards) (count (:hand (get-corp)))))
        (is (= :runner (:winner @state)) "Runner wins"))))
  (testing "when other start-of-turn cards like Marilyn Campaign fire. Issue #3855"
    (do-game
      (new-game {:corp {:deck ["Rashida Jaheem" "Marilyn Campaign" (qty "Hedge Fund" 4)]}})
      (starting-hand state :corp ["Rashida Jaheem" "Marilyn Campaign"])
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (play-from-hand state :corp "Marilyn Campaign" "New remote")
      (let [rj (get-content state :remote1 0)
            mc (get-content state :remote2 0)]
        (core/rez state :corp mc)
        (core/command-counter state :corp '("2"))
        (click-card state :corp mc)
        (take-credits state :corp)
        (core/rez state :corp rj)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp is in Step 1.2")
        (core/end-phase-12 state :corp nil)
        (is (= 2 (-> (prompt-map :corp) :choices count)) "Corp should have two abilities to trigger")
        (click-prompt state :corp "Marilyn Campaign")
        (click-prompt state :corp "Yes")
        (is (find-card "Marilyn Campaign" (:deck (get-corp))))
        (is (zero? (-> (get-corp) :hand count)) "Corp should have 3 cards in hand")
        (click-prompt state :corp "Yes")
        (is (= 4 (-> (get-corp) :hand count)) "Corp should draw 3 cards from Rashida plus 1 from Mandatory Draw")
        (is (nil? (:corp-phase-12 @state)) "Corp is not in Step 1.2")))))

(deftest reality-threedee
  ;; Reality Threedee - Take 1 bad pub on rez; gain 1c at turn start (2c if Runner tagged)
  (do-game
    (new-game {:corp {:deck ["Reality Threedee"]}})
    (play-from-hand state :corp "Reality Threedee" "New remote")
    (let [r3d (get-content state :remote1 0)]
      (core/rez state :corp r3d)
      (is (= 1 (count-bad-pub state)) "Took 1 bad pub on rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Gained 1 credit")
      (take-credits state :corp)
      (gain-tags state :runner 1)
      (take-credits state :runner)
      (is (= 13 (:credit (get-corp))) "Gained 2 credits because Runner is tagged"))))

(deftest reconstruction-contract
  ;; Reconstruction Contract - place advancement token when runner takes meat damage
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Reconstruction Contract"]}
               :runner {:hand [(qty "Sure Gamble" 10)]}})
    (play-from-hand state :corp "Reconstruction Contract" "New remote")
    (let [rc (get-content state :remote1 0)]
      (core/rez state :corp (refresh rc))
      (core/damage state :corp :meat 1)
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract has 1 advancement token")
      (core/damage state :corp :net 1)
      (is (= 2 (count (:discard (get-runner)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract doesn't get advancement token for net damage"))))

(deftest reversed-accounts
  ;; Reversed Accounts - Trash to make Runner lose 4 credits per advancement
  (do-game
    (new-game {:corp {:deck ["Reversed Accounts"]}})
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
      (is (= 4 (get-counters (refresh rev) :advancement)))
      (core/rez state :corp (refresh rev))
      (card-ability state :corp rev 0)
      (is (= 1 (count (:discard (get-corp)))) "Reversed Accounts trashed")
      (is (= 2 (:credit (get-runner))) "Runner lost 16 credits"))))

(deftest rex-campaign
  ;; Rex Campaign
  (testing "Gain 5 credits"
    (do-game
      (new-game {:corp {:deck ["Rex Campaign"]}})
      (play-from-hand state :corp "Rex Campaign" "New remote")
      (let [rex (get-content state :remote1 0)]
        (core/rez state :corp rex)
        (is (= 3 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 2 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 1 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (let [credits (:credit (get-corp))]
          (is (zero? (get-counters (refresh rex) :power)))
          (click-prompt state :corp "Gain 5 [Credits]")
          (is (= (+ 5 credits) (:credit (get-corp))))
          (is (= "Rex Campaign" (-> (get-corp) :discard first :title)))))))
  (testing "Lose 1 bad publicity"
    (do-game
      (new-game {:corp {:deck ["Rex Campaign"]}})
      (core/gain-bad-publicity state :corp 1)
      (play-from-hand state :corp "Rex Campaign" "New remote")
      (let [rex (get-content state :remote1 0)]
        (core/rez state :corp rex)
        (is (= 3 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 2 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 1 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (zero? (get-counters (refresh rex) :power)))
        (click-prompt state :corp "Remove 1 bad publicity")
        (is (zero? (count-bad-pub state)) "Should not have the same amount of bad publicity")
        (is (= "Rex Campaign" (-> (get-corp) :discard first :title)))))))

(deftest ronald-five
  ;; Ronald Five - Runner loses a click every time they trash a Corp card
  (do-game
    (new-game {:corp {:deck ["Ronald Five" "Melange Mining Corp."]}})
    (play-from-hand state :corp "Ronald Five" "New remote")
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :remote1 0))
    (run-empty-server state :remote2)
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (= 2 (:click (get-runner))) "Lost 1 click")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay 3 [Credits] to trash")
    (is (zero? (:click (get-runner))) "Lost 1 click")))

(deftest ronin
  ;; Ronin - Click-trash to do 3 net damage when it has 4 or more advancements
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ronin" "Mushin No Shin"]}})
      (play-from-hand state :corp "Mushin No Shin")
      (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
      (let [ron (get-content state :remote1 0)]
        (is (= 3 (get-counters (refresh ron) :advancement)))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/rez state :corp (refresh ron))
        (card-ability state :corp (refresh ron) 0)
        (is (= 3 (count (:hand (get-runner)))) "Ronin ability didn't fire with only 3 advancements")
        (take-credits state :corp)
        (take-credits state :runner)
        (core/advance state :corp {:card (refresh ron)})
        (is (= 4 (get-counters (refresh ron) :advancement)))
        (card-ability state :corp ron 0)
        (is (= 3 (count (:discard (get-runner)))) "Ronin did 3 net damage")
        (is (= 2 (count (:discard (get-corp)))) "Ronin trashed"))))
  (testing "doesn't fire (or crash) if no advance counters"
    (do-game
      (new-game {:corp {:deck ["Ronin"]}})
      (play-from-hand state :corp "Ronin" "New remote")
      (let [ron (get-content state :remote1 0)]
        (is (zero? (get-counters (refresh ron) :advancement)) "Ronin starts with no counters")
        (core/rez state :corp (refresh ron))
        (card-ability state :corp (refresh ron) 0)
        (is (zero? (get-counters (refresh ron) :advancement)) "Ronin didn't gain counters")
        (is (= 3 (count (:hand (get-runner)))) "Ronin ability didn't fire with 0 advancements")))))

(deftest roughneck-repair-squad
  ;; Roughneck Repair Squad - gain 6c, may remove 1 bad publicity
  (do-game
    (new-game {:corp {:deck ["Roughneck Repair Squad"]}})
    (core/gain state :corp :click 7)
    (play-from-hand state :corp "Roughneck Repair Squad" "New remote")
    (let [rrs (get-content state :remote1 0)
          start-credits (:credit (get-corp))]
      (core/rez state :corp rrs)
      (is (zero? (count-bad-pub state)) "Start with no bad pub")
      (card-ability state :corp rrs 0)
      (is (= (:credit (get-corp)) (+ 6 start-credits)) "Gained 6 credits")
      (is (empty? (:prompt (get-corp))) "No prompt if no bad pub")
      (core/gain state :corp :bad-publicity 1)
      (is (= 1 (count-bad-pub state)) "Start with 1 bad pub")
      (card-ability state :corp rrs 0)
      (is (= (:credit (get-corp)) (+ 12 start-credits)) "Gained 6 credits")
      (click-prompt state :corp "No")
      (is (= 1 (count-bad-pub state)) "Kept 1 bad pub")
      (card-ability state :corp rrs 0)
      (is (= (:credit (get-corp)) (+ 18 start-credits)) "Gained 6 credits")
      (click-prompt state :corp "Yes")
      (is (zero? (count-bad-pub state)) "Removed 1 bad pub"))))

(deftest sandburg
  ;; Sandburg - +1 strength to all ICE for every 5c when Corp has over 10c
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Sandburg" (qty "Ice Wall" 2) (qty "Hedge Fund" 3)]}})
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
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (is (= 1 (:current-strength (refresh iwall1))) "Strength back to default")
        (is (= 1 (:current-strength (refresh iwall2))) "Strength back to default"))))
  (testing "Changes on rez"
    (do-game
      (new-game {:corp {:hand ["Sandburg" (qty "Ice Wall" 2) "Mlinzi" "Hedge Fund"]
                        :deck [(qty "Hedge Fund" 3)]
                        :credits 10}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Sandburg" "New remote")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Mlinzi" "Archives")
      (let [sb (get-content state :remote1 0)
            iwall1 (get-ice state :hq 0)
            iwall2 (get-ice state :rd 0)
            mlinzi (get-ice state :archives 0)]
        (core/rez state :corp iwall1)
        (core/rez state :corp iwall2)
        (core/rez state :corp sb)
        (is (= 8 (:credit (get-corp))))
        (play-from-hand state :corp "Hedge Fund")
        (is (= 3 (:current-strength (refresh iwall1))) "Strength boosted by 2")
        (is (= 12 (:credit (get-corp))))
        (core/rez state :corp mlinzi)
        (is (= 5 (:credit (get-corp))))
        (is (= 1 (:current-strength (refresh iwall1))) "Strength back to base")))))

(deftest sealed-vault
  ;; Sealed Vault - Store credits for 1c, retrieve credits by trashing or spending click
  (do-game
    (new-game {:corp {:deck ["Sealed Vault" "Hedge Fund"]}})
    (play-from-hand state :corp "Sealed Vault" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (let [sv (get-content state :remote1 0)]
      (core/rez state :corp sv)
      (card-ability state :corp sv 0)
      (click-prompt state :corp "8")
      (is (= 8 (get-counters (refresh sv) :credit)) "8 credits stored on Sealed Vault")
      (is (zero? (:credit (get-corp))))
      (card-ability state :corp sv 1)
      (click-prompt state :corp "8")
      (is (zero? (get-counters (refresh sv) :credit)) "Credits removed from Sealed Vault")
      (is (= 8 (:credit (get-corp))))
      (is (zero? (:click (get-corp))) "Spent a click")
      (card-ability state :corp sv 0)
      (click-prompt state :corp "7")
      (is (= 7 (get-counters (refresh sv) :credit)) "7 credits stored on Sealed Vault")
      (is (zero? (:credit (get-corp))))
      (card-ability state :corp sv 2)
      (click-prompt state :corp "7")
      (is (= 7 (:credit (get-corp))))
      (is (= 2 (count (:discard (get-corp)))) "Sealed Vault trashed"))))

(deftest security-subcontract
  ;; Security Subcontract
  (do-game
    (new-game {:corp {:deck ["Security Subcontract" "Ice Wall"]}})
    (play-from-hand state :corp "Security Subcontract" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ss (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp ss)
      (core/rez state :corp iw)
      (let [credits (:credit (get-corp))
            clicks (:click (get-corp))]
        (card-ability state :corp ss 0)
        (click-card state :corp iw)
        (is (= (+ credits 4) (:credit (get-corp))) "Corp should gain 4 from Security Subcontract ability")
        (is (= "Ice Wall" (-> (get-corp) :discard first :title)) "Ice Wall should be in Archives from Security Subcontract ability")
        (is (= (dec clicks) (:click (get-corp))) "Corp should lose 1 click from Security Subcontract ability")))))

(deftest sensie-actors-union
  ;; Sensie Actors Union
  (do-game
    (new-game {:corp {:deck ["Sensie Actors Union" "Ronin" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Sensie Actors Union" "Ronin"])
    (core/move state :corp (find-card "Ronin" (:hand (get-corp))) :deck {:front true})
    (play-from-hand state :corp "Sensie Actors Union" "New remote")
    (let [sau (get-content state :remote1 0)]
      (core/rez state :corp sau)
      (take-credits state :corp)
      (is (zero? (count (:hand (get-corp)))) "Corp should have no cards in hand before starting turn")
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp sau 0)
      (is (= 3 (count (:hand (get-corp)))) "Corp should draw 3 cards from Sensie Actors Union's ability")
      (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
      (is (= "Ronin" (-> (get-corp) :deck last :title)) "Ronin should be on bottom of deck")
      (core/end-phase-12 state :corp nil)
      (is (= 3 (count (:hand (get-corp)))) "Corp should have 3 cards in hand after putting one on bottom of R&D and mandatory draw")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (nil? (:corp-phase-12 @state)) "Sensie Actors Union doesn't trigger if protected by ice"))))

(deftest server-diagnostics
  ;; Server Diagnostics - Gain 2c when turn begins; trashed when ICE is installed
  (do-game
    (new-game {:corp {:deck ["Server Diagnostics" "Pup"
                             "Launch Campaign"]}})
    (play-from-hand state :corp "Server Diagnostics" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (is (= 1 (count (get-content state :remote1))) "Non-ICE install didn't trash Serv Diag")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 5 (:credit (get-corp))) "Gained 2c at start of turn")
    (play-from-hand state :corp "Pup" "HQ")
    (is (= 1 (count (:discard (get-corp)))) "Server Diagnostics trashed by ICE install")))

(deftest shannon-claire
  ;; Shannon Claire
  (do-game
    (new-game {:corp {:deck ["Shannon Claire" "Hostile Takeover" "Ronin" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Shannon Claire" "Ronin"])
    (core/move state :corp (find-card "Ronin" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Shannon Claire" "New remote")
    (let [shannon (get-content state :remote1 0)]
      (core/rez state :corp shannon)
      (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in hand to start")
      (card-ability state :corp shannon 0)
      (is (= "Ronin" (-> (get-corp) :hand first :title)) "Corp should draw Ronin with Shannon's click ability")
      (let [number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (card-ability state :corp shannon 1)
        (click-prompt state :corp (find-card "Hostile Takeover" (:deck (get-corp))))
        (is (= "Hostile Takeover" (-> (get-corp) :deck last :title))
            "Agenda selected with Shannon's R&D ability should be on bottom of deck")
        (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Searching R&D should shuffle deck")))
    (core/move state :corp (find-card "Hostile Takeover" (:deck (get-corp))) :discard)
    (core/move state :corp (find-card "Shannon Claire" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Shannon Claire" "New remote")
    (let [shannon (get-content state :remote2 0)
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
      (core/rez state :corp shannon)
      (card-ability state :corp shannon 2)
      (click-prompt state :corp (find-card "Hostile Takeover" (:discard (get-corp))))
      (is (= "Hostile Takeover" (-> (get-corp) :deck last :title))
          "Agenda selected with Shannon's Archives ability should be on bottom of deck")
      (is (= number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck)))
          "Searching Archives shouldn't shuffle deck"))))

(deftest shattered-remains
  ;; Shattered Remains
  (do-game
    (new-game {:corp {:deck [(qty "Shattered Remains" 2)]}
               :runner {:deck ["Cyberfeeder" "Lemuria Codecracker"]}})
    (play-from-hand state :corp "Shattered Remains" "New remote")
    (play-from-hand state :corp "Shattered Remains" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Cyberfeeder")
    (play-from-hand state :runner "Lemuria Codecracker")
    (take-credits state :runner)
    (let [remains1 (get-content state :remote1 0)
          remains2 (get-content state :remote2 0)
          cyber (get-hardware state 0)
          lemuria (get-hardware state 1)]
      (core/rez state :corp remains1)
      (advance state remains2 1)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (is (empty? (:prompt (get-corp))) "Corp shouldn't get Shattered Remains ability prompt when no counters")
      (click-prompt state :runner "No action")
      (run-empty-server state :remote2)
      (let [credits (:credit (get-corp))]
        (click-prompt state :corp "Yes")
        (click-card state :corp cyber)
        (is (= (dec credits) (:credit (get-corp))) "Shattered Remains ability should cost 1")
        (is (count (:discard (get-runner))) "Cyberfeeder should be in discard from Shattered Remains")))))

(deftest shi-kyu
  ;; Shi.Kyū
  ; (testing "Basic test"
  ;   (do-game
  ;     (new-game {:corp {:deck ["Shi.Kyū"]}
  ;                :runner {:deck [(qty "Sure Gamble" 5)]}})
  ;     (play-from-hand state :corp "Shi.Kyū" "New remote")
  ;     (take-credits state :corp)
  ;     (run-empty-server state "Server 1")
  ;     (click-prompt state :corp "Yes")
  ;     (click-prompt state :corp "5")
  ;     (is (= "Take 5 net damage" (first (prompt-buttons :runner))))
  ;     (click-prompt state :runner "Take 5 net damage")
  ;     (click-prompt state :runner "No action")
  ;     (is (zero? (count (:hand (get-runner)))) "Runner took 5 net damage from Shi.Kyū")
  ;     (run-empty-server state "Server 1")
  ;     (click-prompt state :corp "Yes")
  ;     (click-prompt state :corp "2")
  ;     (is (= "Take 2 net damage" (first (prompt-buttons :runner))))
  ;     (click-prompt state :runner "Add Shi.Kyū to score area")
  ;     (is (empty? (prompt-map :runner)) "Runner shouldn't get the option to trash Shi.Kyū as it was added to agenda area")
  ;     (is (= -1 (:agenda-point (get-runner))) "Runner should be at -1 agenda points after adding Shi.Kyū to agenda area")))
  (testing "interaction with Maw. Issue #4214"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Government Takeover" "Shi.Kyū"]}
                 :runner {:hand ["Maw"]
                          :credits 10}})
      (play-from-hand state :corp "Shi.Kyū" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Maw")
      (is (zero? (count (:discard (get-corp)))) "Corp has 0 cards in archives to start")
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "5")
      (click-prompt state :runner "Add Shi.Kyū to score area")
      (is (= 1 (count (:discard (get-corp)))) "Corp discards a card from hand")
      (is (= "Government Takeover" (-> (get-corp) :discard first :title))
          "Corp discards card from hand from Maw"))))

(deftest shock
  ;; Shock! - do 1 net damage on access
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Shock!" 3)]}})
      (trash-from-hand state :corp "Shock!")
      (play-from-hand state :corp "Shock!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
      (run-empty-server state "Archives")
      (is (= 1 (count (:hand (get-runner)))) "Runner took 1 net damage")))
  (testing "ensure :access flag is cleared on run end. Issue #2319"
    (do-game
      (new-game {:corp {:deck [(qty "Shock!" 3) "Chairman Hiro"]}})
      (trash-from-hand state :corp "Shock!")
      (play-from-hand state :corp "Shock!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
      (is (not (:run @state)) "Run is complete")
      (trash-from-hand state :corp "Chairman Hiro")
      (is (= 2 (count (:discard (get-corp)))) "Hiro and Shock still in archives")
      (is (zero? (count (:scored (get-runner)))) "Hiro not scored by Runner"))))

(deftest siu
  ;; SIU
  (testing "Flags 1.2 and trace for tag with base 3"
    (do-game
      (new-game {:corp {:deck [(qty "SIU" 10)]}})
      (play-from-hand state :corp "SIU" "New remote")
      (let [siu (get-content state :remote1 0)]
        (core/rez state :corp siu)
        (card-ability state :corp (refresh siu) 0) ; try to trigger SIU outside phase 1.2
        (is (zero? (-> (get-corp) :discard count)) "SIU should not trigger because it's not 1.2")
        (take-credits state :corp)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp is in Step 1.2 because SIU is on the table")
        (card-ability state :corp (refresh siu) 0)
        (is (= 1 (-> (get-corp) :discard count)) "SIU should discard to fire trace")
        (is (= 3 (:base (prompt-map :corp))) "Base Trace should be 3")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (count-tags state)) "Runner has 1 tag")))))

(deftest snare
  (testing "Basic test"
    ;; pay 4 on access, and do 3 net damage and give 1 tag
    (do-game
      (new-game {:corp {:deck [(qty "Snare!" 3)]}})
      (play-from-hand state :corp "Snare!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= :waiting (prompt-type :runner))
          "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (is (= 3 (:credit (get-corp))) "Corp had 7 and paid 4 for Snare! 1 left")
      (is (= 1 (count-tags state)) "Runner has 1 tag")
      (is (zero? (count (:hand (get-runner)))) "Runner took 3 net damage")))
  (testing "Can't afford"
    (do-game
      (new-game {:corp {:deck ["Snare!"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-from-hand state :corp "Snare!" "New remote")
      (take-credits state :corp)
      (core/lose state :corp :credit 7)
      (run-empty-server state "Server 1")
      (is (= :waiting (prompt-type :runner))
          "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (is (zero? (count-tags state)) "Runner has 0 tags")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
      (is (zero? (count (:discard (get-runner)))) "Runner took no damage")))
  (testing "with Dedicated Response Team"
    (do-game
      (new-game {:corp {:deck ["Snare!" "Dedicated Response Team"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-from-hand state :corp "Snare!" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/gain state :corp :click 1 :credit 4)
      (let [drt (get-content state :remote2 0)]
        (take-credits state :corp)
        (run-on state "Server 1")
        (core/rez state :corp drt)
        (run-continue state)
        (run-successful state)
        (is (= :waiting (prompt-type :runner))
            "Runner has prompt to wait for Snare!")
        (click-prompt state :corp "Yes")
        (is (= 1 (count-tags state)) "Runner has 1 tag")
        (click-prompt state :runner "Pay 0 [Credits] to trash")
        (is (= 5 (count (:discard (get-runner)))) "Runner took 5 damage")))))

(deftest space-camp
  (testing "when in Archives. #1929"
    (do-game
      (new-game {:corp {:deck ["Space Camp" "News Team" "Breaking News"]}})
      (trash-from-hand state :corp "Space Camp")
      (trash-from-hand state :corp "News Team")
      (play-from-hand state :corp "Breaking News" "New remote")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (click-prompt state :runner "News Team")
      (click-prompt state :runner "Take 2 tags")
      (click-prompt state :runner "Space Camp")
      (click-prompt state :corp "Yes")
      (click-card state :corp (get-content state :remote1 0))
      (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "Agenda advanced once from Space Camp")
      (is (= 2 (count-tags state)) "Runner has 2 tags")
      (is (not (:run @state)) "Run completed"))))

(deftest storgotic-resonator
  ;; Storgotic Resonator - Gains power counters on Corp trashing card with same faction as runner ID.
  ;; Click+counter is 1 net damage
  (testing "Basic behavior"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Storgotic Resonator" "Breached Dome" "Hostile Infrastructure" "Launch Campaign"]
                        :credits 20}
                 :runner {:id "Null: Whistleblower"
                          :deck [(qty "Sure Gamble" 5)]
                          :hand [(qty "Mimic" 3)]
                          :credits 10}})
      (play-from-hand state :corp "Storgotic Resonator" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (let [sr (get-content state :remote1 0)
            hi (get-content state :remote2 0)]
        (core/rez state :corp (refresh sr))
        (core/rez state :corp (refresh hi))
        (take-credits state :corp)
        (is (zero? (get-counters (refresh sr) :power)) "No power counters initially")
        (is (empty? (:discard (get-runner))) "Nothing in trash")
        (run-empty-server state :hq)
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh sr) :power)) "Gained power counter when trashing Mimic")
        (is (= 2 (count (:discard (get-runner)))) "Mimic was trashed")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state "Server 3")
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (is (= 2 (get-counters (refresh sr) :power)))
        (take-credits state :runner)
        (card-ability state :corp (refresh sr) 0)
        (is (= 2 (get-counters (refresh sr) :power)) "Spent power counter, did damage, got counter back")
        (is (= 4 (count (:discard (get-runner)))) "Did net damage")))))

(deftest student-loans
  ;; Student Loans - costs Runner 2c extra to play event if already same one in discard
  (do-game
    (new-game {:corp {:deck ["Student Loans" (qty "Hedge Fund" 2)]}})
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
  ;; Sundew - gain 2cr when the runner spends his first click each turn, unless
  ;; it is a run on this server
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Sundew"]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp sund)
        (take-credits state :corp 2)
        (is (= 5 (:credit (get-corp))) "Cost 2cr to rez")
        ;; spend a click not on a run
        (take-credits state :runner)
        (is (= 7 (:credit (get-corp))) "Corp gained 2cr from Sundew")
        (take-credits state :corp)
        (is (= 10 (:credit (get-corp))) "Corp now has 10cr")
        ;; spend a click running but not on the sundew server
        (run-on state "HQ")
        (is (= 12 (:credit (get-corp))) "Corp gained 2cr from Sundew")
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 15 (:credit (get-corp))) "Corp now has 15cr")
        ;; spend a click running the sundew server
        (run-on state "Server 1")
        (is (= 15 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew")
        (is (= 3 (:click (get-runner))) "Runner spent 1 click to start run"))))
  (testing "Multiple Sundews"
    (do-game
      (new-game {:corp {:deck [(qty "Sundew" 2)]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)
            sund2 (get-content state :remote2 0)]
        (is (= 5 (:credit (get-corp))) "Corp has 5cr")
        (core/rez state :corp sund)
        (is (= 3 (:credit (get-corp))) "Cost 2cr to rez")
        (core/rez state :corp sund2)
        (is (= 1 (:credit (get-corp))) "Cost 2cr to rez")
        (take-credits state :corp)
        (is (= 2 (:credit (get-corp))) "Corp now has 2cr")
        ;; spend a click not on a run
        (take-credits state :runner)
        (is (= 6 (:credit (get-corp))) "Corp gained 4cr from Sundew")
        (take-credits state :corp)
        (is (= 9 (:credit (get-corp))) "Corp now has 9cr")
        ;; spend a click running one of the sundew servers
        (run-on state "Server 1")
        (is (= 11 (:credit (get-corp))) "Corp gain 2cr from one Sundew")
        (run-jack-out state)
        ;; spend a second click running the other sundew server
        (run-on state "Server 2")
        (is (= 11 (:credit (get-corp))) "Corp did not gain credits from Sundew"))))
  (testing "Sundew - Data Breach"
    (do-game
      (new-game {:corp {:deck ["Sundew"]}
                 :runner {:deck ["Data Breach"]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp sund)
        (take-credits state :corp 2)
        (is (= 5 (:credit (get-corp))) "Corp now has 5cr")
        (play-from-hand state :runner "Data Breach")
        (is (= 7 (:credit (get-corp))) "Corp gained 2cr from Sundew")
        (run-continue state)
        (run-successful state)
        (click-prompt state :runner "Yes")
        (is (= 7 (:credit (get-corp))) "Corp did not gain credits from second run"))))
  (testing "Sundew - Deuces Wild"
    (do-game
      (new-game {:corp {:deck ["Sundew" "Wraparound"]}
                 :runner {:deck ["Deuces Wild"]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (play-from-hand state :corp "Wraparound" "Server 1")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp sund)
        (take-credits state :corp 2)
        (is (= 4 (:credit (get-corp))) "Corp now has 4cr")
        (play-from-hand state :runner "Deuces Wild")
        (click-prompt state :runner "Expose 1 ice and make a run")
        (click-card state :runner (get-ice state :remote1 0))
        (click-prompt state :runner "Server 1")
        (is (= 6 (:credit (get-corp))) "Corp gained 2cr from Sundew because DW is not a run event"))))
  (testing "Sundew - Out of the Ashes"
    (do-game
      (new-game {:corp {:deck ["Sundew"]}
                 :runner {:deck [(qty "Out of the Ashes" 2)]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp sund)
        (take-credits state :corp 2)
        (is (= 5 (:credit (get-corp))) "Corp now has 5cr")
        ; spend click on run event
        (play-from-hand state :runner "Out of the Ashes")
        (click-prompt state :runner "Archives")
        (is (= 7 (:credit (get-corp))) "Corp gained 2cr from Sundew")
        (run-continue state)
        (run-successful state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 10 (:credit (get-corp))) "Corp now has 10cr")
        ; run without spending click
        (is (some? (prompt-map :runner)))
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Archives")
        (is (= 10 (:credit (get-corp))) "Corp did not gain credits from Ashes (no click spent)")
        ; spend click on credits
        (take-credits state :runner 1)
        (is (= 12 (:credit (get-corp))) "Corp gained 2cr from Sundew")
        (play-from-hand state :runner "Out of the Ashes")
        (click-prompt state :runner "Archives")
        (run-continue state)
        (run-successful state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 15 (:credit (get-corp))) "Corp now has 15cr")
        ; run without spending click
        (is (some? (prompt-map :runner)))
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Archives")
        (is (= 15 (:credit (get-corp))) "Corp did not gain credits from Ashes (no click spent)")
        (run-jack-out state)
        ; spend click on run
        (run-on state "Archives")
        (is (= 17 (:credit (get-corp))) "Corp gained 2cr from Sundew"))))
  (testing "Sundew - Dirty Laundry"
    (do-game
      (new-game {:corp {:deck ["Sundew"]}
                 :runner {:deck [(qty "Dirty Laundry" 2)]}})
      (play-from-hand state :corp "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :corp (refresh sund))
        (is (= 3 (:credit (get-corp))) "Cost 2cr to rez")
        (take-credits state :corp)
        (is (= 5 (:credit (get-corp))) "Corp now has 5cr")
        ;; spend a click on a run through a card, not through click-run
        (play-from-hand state :runner "Dirty Laundry")
        (click-prompt state :runner "Server 1")
        (is (= 5 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew")
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 8 (:credit (get-corp))) "Corp now has 8cr")
        ;; spend a click on a run through a card, not through click-run
        (play-from-hand state :runner "Dirty Laundry")
        (click-prompt state :runner "HQ")
        (is (= 10 (:credit (get-corp))) "Corp gained 2cr from Sundew")))))

(deftest synth-dna-modification
  ;; Synth DNA Modification
  (do-game
    (new-game {:corp {:deck ["Synth DNA Modification" "Data Mine"]}})
    (play-from-hand state :corp "Synth DNA Modification" "New remote")
    (play-from-hand state :corp "Data Mine" "HQ")
    (let [dna (get-content state :remote1 0)
          data (get-ice state :hq 0)]
      (core/rez state :corp dna)
      (core/rez state :corp data)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp data 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner should take 1 net damage from Data Mine")
      (is (= 1 (count (:discard (get-corp)))) "Data Mine should trash self after subroutine fires")
      (card-ability state :corp dna 0)
      (is (= 2 (count (:discard (get-runner))))
          "Runner should take 1 net damage from Synth DNA Modification after Data Mine subroutine"))))

(deftest team-sponsorship
  ;; Team Sponsorship
  (testing "Install from HQ"
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"]}})
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :corp tsp)
        (score-agenda state :corp ag1)
        (click-card state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:hand (get-corp)))) "No Adonis in hand"))))
  (testing "Install from Archives"
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"]}})
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (trash-from-hand state :corp "Adonis Campaign")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :corp tsp)
        (score-agenda state :corp ag1)
        (click-card state :corp (find-card "Adonis Campaign" (:discard (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:discard (get-corp)))) "No Adonis in discard"))))
  (testing "Multiple installs"
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers"
                               (qty "Team Sponsorship" 2)
                               (qty "Adonis Campaign" 2)]}})
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
        (click-prompt state :corp "Team Sponsorship")
        (click-card state :corp (find-card "Adonis Campaign" (:discard (get-corp))))
        (click-prompt state :corp "New remote")
        (click-card state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote4 0)))
            "Adonis installed by Team Sponsorship")
        (is (= "Adonis Campaign" (:title (get-content state :remote5 0)))
            "Adonis installed by Team Sponsorship"))))
  (testing "Score 5 points in one window"
    (do-game
      (new-game {:corp {:deck [(qty "AstroScript Pilot Program" 3)
                               "Team Sponsorship"
                               "Breaking News"
                               "SanSan City Grid"]}})
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (core/gain state :corp :credit 100 :click 5)
      (core/rez state :corp (get-content state :remote1 0))
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (play-from-hand state :corp "AstroScript Pilot Program" "Server 1")
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (core/rez state :corp (get-content state :remote3 0))
      (score-agenda state :corp (get-content state :remote1 1))
      (click-card state :corp (find-card "AstroScript Pilot Program" (:hand (get-corp))))
      (is (zero? (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript not resolved yet")
      (click-prompt state :corp "Server 1")
      (is (= 1 (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript resolved")
      (card-ability state :corp (first (:scored (get-corp))) 0)
      (click-card state :corp (get-content state :remote1 1))
      (card-ability state :corp (second (:scored (get-corp))) 0)
      (click-card state :corp (get-content state :remote1 1))
      (core/score state :corp {:card (get-content state :remote1 1)})
      (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
      (click-prompt state :corp "Server 1")
      (card-ability state :corp (second (next (:scored (get-corp)))) 0)
      (click-card state :corp (get-content state :remote1 1))
      (core/score state :corp {:card (get-content state :remote1 1)})
      (click-prompt state :corp "Done")
      (is (= 7 (:agenda-point (get-corp))) "Scored 5 points in one turn"))))

(deftest tech-startup
  ;; Tech Startup
  (do-game
    (new-game {:corp {:deck ["Tech Startup" "TechnoCo" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Tech Startup"])
    (play-from-hand state :corp "Tech Startup" "New remote")
    (let [tech (get-content state :remote1 0)]
      (core/rez state :corp (refresh tech))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (zero? (-> (get-corp) :discard count)) "Corp should start with 0 cards in Archives")
      (card-ability state :corp tech 0)
      (click-prompt state :corp (find-card "TechnoCo" (:deck (get-corp))))
      (click-prompt state :corp "New remote")
      (is (= "TechnoCo" (:title (get-content state :remote2 0)))
          "TechnoCo should be installed in a new remote from Tech Startup's ability")
      (is (= 1 (-> (get-corp) :discard count)) "Tech Startup should now be in discard"))))

(deftest technoco
  ;; TechnoCo - Increase program / hardware / virtual cost by 1 and gain 1 when they are installed
  (do-game
    (new-game {:corp {:deck ["TechnoCo"]}
               :runner {:deck ["Misdirection"       ;; 0 cost program
                               "Bookmark"           ;; 0 cost hardware
                               "Ice Analyzer"       ;; 0 cost virtual resource
                               "Fall Guy"]}})        ;; 0 cost non-virtual resource
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
    (new-game {:corp {:deck ["Tenma Line" "Harvester"
                             "Aimor" "Lockdown"]}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Tenma Line" "New remote")
    (play-from-hand state :corp "Harvester" "HQ")
    (play-from-hand state :corp "Aimor" "HQ")
    (play-from-hand state :corp "Lockdown" "R&D")
    (core/rez state :corp (get-ice state :rd 0))
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 1 (:click (get-corp))))
    (card-ability state :corp (get-content state :remote1 0) 0)
    (click-card state :corp (get-ice state :rd 0))
    (click-card state :corp (get-ice state :hq 1))
    (is (empty? (:prompt (get-corp))))
    (is (zero? (:click (get-corp))) "Spent 1 click")
    (is (= "Aimor" (:title (get-ice state :rd 0))) "Aimor swapped to R&D")
    (is (= "Lockdown" (:title (get-ice state :hq 1))) "Lockdown swapped to HQ outer position")))

(deftest test-ground
  ;; Test Ground
  (do-game
    (new-game {:corp {:deck ["Test Ground" "Ice Wall" "News Team"]}})
    (core/gain state :corp :credit 100 :click 100)
    (play-from-hand state :corp "Test Ground" "New remote")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "News Team" "New remote")
    (let [ground (get-content state :remote1 0)
          iw (get-ice state :remote2 0)
          news (get-content state :remote3 0)]
      (core/rez state :corp ground)
      (core/rez state :corp iw)
      (core/rez state :corp news)
      (advance state ground 2)
      (is (rezzed? (refresh iw)) "Ice Wall should be rezzed")
      (is (rezzed? (refresh news)) "News Team should be rezzed")
      (is (zero? (-> (get-corp) :discard count)) "Corp should start with 0 cards in Archives")
      (card-ability state :corp ground 0)
      (click-card state :corp iw)
      (click-card state :corp news)
      (is (not (rezzed? (refresh iw))) "Ice Wall should not be rezzed")
      (is (not (rezzed? (refresh news))) "News Team should not be rezzed")
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in discard"))))

(deftest the-board
  ;; The Board
  (testing "Modify everything in the score area (regression test for #1938)"
    (do-game
      (new-game {:corp {:deck ["The Board" "News Team" (qty "Firmware Updates" 2)]}
                 :runner {:deck [(qty "Artist Colony" 3) (qty "Fan Site" 3)]}})
      (starting-hand state :runner ["Artist Colony" "Fan Site"])
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
      (is (zero? (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (take-credits state :corp)
      (run-empty-server state :remote3)
      (click-prompt state :runner "Steal")
      (is (= 2 (count (:scored (get-runner)))) "Firmware Updates stolen")
      (is (= 1 (:agenda-point (get-runner))) "Runner has 1 agenda point")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= -1 (:agenda-point (get-runner))) "Runner has -1 agenda points")
      (run-empty-server state :remote2)
      (click-prompt state :runner "Add News Team to score area")
      (is (= 3 (count (:scored (get-runner)))) "News Team added to Runner score area")
      (is (= -3 (:agenda-point (get-runner))) "Runner has -3 agenda points")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner (find-card "Fan Site" (:deck (get-runner))))
      (click-card state :runner (first (:scored (get-runner))))
      (is (= 2 (count (:scored (get-runner)))) "Fan Site removed from Runner score area")
      (is (= -2 (:agenda-point (get-runner))) "Runner has -2 agenda points")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 7 [Credits] to trash")
      (is (= 3 (count (:scored (get-runner)))) "The Board added to Runner score area")
      (is (= 2 (:agenda-point (get-runner))) "Runner has 2 agenda points")))
  (testing "handle Fifteen Minutes clicked out of Runner's score area"
    (do-game
      (new-game {:corp {:deck ["The Board" "15 Minutes"]}})
      (play-from-hand state :corp "The Board" "New remote")
      (play-from-hand state :corp "15 Minutes" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (is (zero? (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (run-empty-server state :remote2)
      (click-prompt state :runner "Steal")
      (is (zero? (:agenda-point (get-runner))) "Runner stays at 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner has 1 agenda in scored area")
      (take-credits state :runner)
      (let [fifm (first (:scored (get-runner)))]
        (card-ability state :corp (refresh fifm) 0)
        (is (zero? (:agenda-point (get-runner))) "Runner drops to 0 agenda points")
        (is (empty? (:scored (get-runner))) "Runner has no agendas in scored area"))))
  (testing "Corp scoring agenda shouldn't trigger The Board to lower Runner points"
    (do-game
      (new-game {:corp {:deck ["The Board" (qty "Hostile Takeover" 2)]}})
      (core/gain state :corp :credit 6)
      (play-from-hand state :corp "The Board" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (is (zero? (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (run-empty-server state :remote3)
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner has 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner has 1 agenda in scored area")
      (take-credits state :runner)
      (core/rez state :corp (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-runner))) "Runner loses 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner still has 1 agenda in scored area")
      (score-agenda state :corp (get-content state :remote2 0))
      (is (zero? (:agenda-point (get-runner))) "Runner still has 0 agenda points")
      (is (= 1 (count (:scored (get-runner)))) "Runner still has 1 agenda in scored area")))
  (testing "Scoring two copies should be 4 agenda points"
    (do-game
      (new-game {:corp {:deck [(qty "The Board" 2)]}})
      (core/gain state :corp :credit 6)
      (play-from-hand state :corp "The Board" "New remote")
      (play-from-hand state :corp "The Board" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 14)
      (is (zero? (:agenda-point (get-runner))) "Runner has 0 agenda points")
      (is (empty? (:scored (get-runner))) "Runner has no agendas")
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 7 [Credits] to trash")
      (is (= 1 (:agenda-point (get-runner))) "Runner has 1 agenda point")
      (is (= 1 (count (:scored (get-runner)))) "Runner has 1 agenda in scored area")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 7 [Credits] to trash")
      (is (= 4 (:agenda-point (get-runner))) "Runner has 4 agenda points")
      (is (= 2 (count (:scored (get-runner)))) "Runner has 2 agendas in scored area"))))

(deftest the-news-now-hour
  ;; The News Now Hour
  (do-game
    (new-game {:corp {:deck ["The News Now Hour"]}
               :runner {:deck ["Rumor Mill"]}})
    (play-from-hand state :corp "The News Now Hour" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Rumor Mill")
    (is (find-card "Rumor Mill" (:hand (get-runner))) "Rumor Mill should still be in hand after trying to play it")))

(deftest the-root
  ;; The Root
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:hand ["The Root" (qty "Ice Wall" 2)]}})
      (core/gain state :corp :click 1 :credit 1)
      (play-from-hand state :corp "The Root" "New remote")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [root (get-content state :remote1 0)]
        (core/rez state :corp root)
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 1 credit from The Root to install"
                           (play-from-hand state :corp "Ice Wall" "HQ")
                           (click-card state :corp root))
        (is (= 2 (get-counters (refresh root) :recurring)) "Took 1 credit from The Root")
        (let [iw (get-ice state :hq 1)]
          (changes-val-macro 0 (:credit (get-corp))
                             "Used 1 credit from The Root to advance"
                             (core/advance state :corp {:card (refresh iw)})
                             (click-card state :corp root))
          (is (= 1 (get-counters (refresh root) :recurring)) "Took 1 credit from The Root")
          (changes-val-macro 0 (:credit (get-corp))
                             "Used 1 credit from The Root to rez"
                             (core/rez state :corp (refresh iw))
                             (click-card state :corp root))
          (is (= 0 (get-counters (refresh root) :recurring)) "Took 1 credit from The Root"))))))

(deftest thomas-haas
  ;; Thomas Haas
  (letfn [(haas-test [number]
            (do-game
              (new-game {:corp {:deck ["Thomas Haas"]}})
              (core/gain state :corp :credit 10 :click 10)
              (play-from-hand state :corp "Thomas Haas" "New remote")
              (let [haas (get-content state :remote1 0)]
                (core/rez state :corp haas)
                (advance state (refresh haas) number)
                (core/lose state :corp :credit (:credit (get-corp)))
                (is (zero? (-> (get-corp) :discard count)) "Corp should start with 0 cards in Archives")
                (is (zero? (:credit (get-corp))) "Corp should fire ability with 0 credits")
                (is (= number (get-counters (refresh haas) :advancement))
                    (str "Thomas Haas should have " number " advancement tokens"))
                (card-ability state :corp (refresh haas) 0)
                (is (= (* 2 number) (:credit (get-corp)))
                    (str "Corp should gain " (* 2 number) " credits from Thomas Haas' ability"))
                (is (= 1 (-> (get-corp) :discard count)) "Thomas Haas should be in Archives after ability"))))]
    (doall (map haas-test [1 2 3 4 5]))))

(deftest tiered-subscription
  ;; Tiered Subscription
  (do-game
    (new-game {:corp {:hand ["Tiered Subscription" "An Offer You Can't Refuse" "Enigma"]}
               :runner {:deck ["Deuces Wild" "Dirty Laundry"]}})
    (play-from-hand state :corp "Tiered Subscription" "New remote")
    (play-from-hand state :corp "Enigma" "HQ")
    (core/rez state :corp (get-content state :remote1 0))
    ;; An offer you can't refuse run
    (play-from-hand state :corp "An Offer You Can't Refuse")
    (is (= 1 (:credit (get-corp))))
    (click-prompt state :corp "R&D")
    (click-prompt state :runner "Yes")
    (is (= 2 (:credit (get-corp))) "Gained 1 credit from the first run on this turn")
    (run-continue state)
    (run-successful state)
    (take-credits state :corp)
    ;; Normal run
    (is (= 2 (:credit (get-corp))))
    (run-empty-server state "R&D")
    (is (= 3 (:credit (get-corp))))
    (take-credits state :runner)
    (take-credits state :corp)
    ;; Run event
    (is (= 6 (:credit (get-corp))))
    (play-from-hand state :runner "Dirty Laundry")
    (click-prompt state :runner "R&D")
    (is (= 7 (:credit (get-corp))))
    (run-jack-out state)
    (run-empty-server state "R&D")
    (is (= 7 (:credit (get-corp))) "Didn't gain credit on second run this turn")
    (take-credits state :runner)
    (take-credits state :corp)
    ;; Non run event
    (play-from-hand state :runner "Deuces Wild")
    (click-prompt state :runner "Expose 1 ice and make a run")
    (click-card state :runner (get-ice state :hq 0))
    (is (= 10 (:credit (get-corp))))
    (click-prompt state :runner "R&D")
    (is (= 11 (:credit (get-corp))))))

(deftest toshiyuki-sakai
  ;; Toshiyuki Sakai - Swap with an asset/agenda from HQ; Runner can choose to access new card or not
  (do-game
    (new-game {:corp {:deck ["Toshiyuki Sakai" "Project Junebug" "Hedge Fund"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]}})
    (play-from-hand state :corp "Toshiyuki Sakai" "New remote")
    (let [toshi (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh toshi)})
      (core/advance state :corp {:card (refresh toshi)})
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh toshi) :advancement)) "Toshiyuki has 2 advancements")
      (run-empty-server state "Server 1")
      (is (= :waiting (prompt-type :runner))
          "Runner has prompt to wait for Toshiyuki")
      (click-prompt state :corp "Yes") ; choose to do a swap
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= (refresh toshi) (get-content state :remote1 0)) "Toshiyuki still in remote; can't target an operation in hand")
      (click-card state :corp (find-card "Project Junebug" (:hand (get-corp))))
      (let [june (get-content state :remote1 0)]
        (is (= "Project Junebug" (:title (refresh june))) "Project Junebug swapped into Server 1")
        (is (= 2 (get-counters (refresh june) :advancement)) "Project Junebug has 2 advancements")
        (click-prompt state :runner "Yes") ; choose to access new card
        (click-prompt state :corp "Yes") ; pay 1c to fire Junebug
        (is (= 4 (count (:discard (get-runner)))) "Runner took 4 net damage")))))

(deftest turtlebacks
  ;; Turtlebacks - Gain 1 credit for every new server created
  (do-game
    (new-game {:corp {:deck ["Turtlebacks" (qty "PAD Campaign" 2) "Wraparound"]}})
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
    (new-game {:corp {:deck ["Urban Renewal"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]}})
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
      (is (zero? (count (:discard (get-corp)))) "Nothing in Corp trash")
      (is (zero? (count (:discard (get-runner)))) "Nothing in Runner trash")
      (take-credits state :runner)
      ;; Corp turn 4 - damage fires
      (is (= 1 (count (:discard (get-corp)))) "Urban Renewal got trashed")
      (is (= 4 (count (:discard (get-runner)))) "Urban Renewal did 4 meat damage"))))

(deftest vaporframe-fabricator
  ;; Vaporframe Fabricator
  (testing "Click ability"
    (testing "Install an agenda"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "Bellona"]}})
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (card-ability state :corp (get-content state :remote1 0) 0)
        (click-card state :corp "Bellona")
        (click-prompt state :corp "New remote")
        (is (= "Bellona" (:title (get-content state :remote2 0))) "Bellona is now installed")))
    (testing "Install an asset"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "NGO Front"]}})
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (card-ability state :corp (get-content state :remote1 0) 0)
        (click-card state :corp "NGO Front")
        (click-prompt state :corp "New remote")
        (is (= "NGO Front" (:title (get-content state :remote2 0))) "NGO Front is now installed")))
    (testing "Install an ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "Ice Wall"]}})
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (card-ability state :corp (get-content state :remote1 0) 0)
        (click-card state :corp "Ice Wall")
        (is (= ["Archives" "R&D" "HQ" "Server 1" "New remote"] (prompt-buttons :corp))
            "Can install in the same server as Vaporframe Fabricator")
        (click-prompt state :corp "New remote")
        (is (= "Ice Wall" (:title (get-ice state :remote2 0))) "Ice Wall is now installed")))
    (testing "Install an ice in a server with other ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "Ice Wall" "Enigma"]}})
        (play-from-hand state :corp "Enigma" "HQ")
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (card-ability state :corp (get-content state :remote1 0) 0)
        (click-card state :corp "Ice Wall")
        (is (= ["Archives" "R&D" "HQ" "Server 1" "New remote"] (prompt-buttons :corp))
            "Can install in the same server as Vaporframe Fabricator")
        (changes-val-macro
          0 (:credit (get-corp))
          "Corp doesn't lose any credits from installing a second ice over HQ"
          (click-prompt state :corp "HQ"))
        (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall is now installed"))))
  (testing "Trash ability"
    (testing "Install an agenda"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "Bellona"]}})
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-card state :corp "Bellona")
        (click-prompt state :corp "New remote")
        (is (= "Bellona" (:title (get-content state :remote2 0))) "Bellona is now installed")))
    (testing "Install an asset"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "NGO Front"]}})
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-card state :corp "NGO Front")
        (click-prompt state :corp "New remote")
        (is (= "NGO Front" (:title (get-content state :remote2 0))) "NGO Front is now installed")))
    (testing "Install an ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "Ice Wall"]}})
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-card state :corp "Ice Wall")
        (is (= ["Archives" "R&D" "HQ" "New remote"] (prompt-buttons :corp))
            "Can't install in the same server as Vaporframe Fabricator")
        (click-prompt state :corp "New remote")
        (is (= "Ice Wall" (:title (get-ice state :remote2 0))) "Ice Wall is now installed")))
    (testing "Install an ice in a server with other ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vaporframe Fabricator" "Ice Wall" "Enigma"]}})
        (play-from-hand state :corp "Enigma" "HQ")
        (play-from-hand state :corp "Vaporframe Fabricator" "New remote")
        (core/rez state :corp (get-content state :remote1 0))
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-card state :corp "Ice Wall")
        (is (= ["Archives" "R&D" "HQ" "New remote"] (prompt-buttons :corp))
            "Can install in the same server as Vaporframe Fabricator")
        (changes-val-macro
          0 (:credit (get-corp))
          "Corp doesn't lose any credits from installing a second ice over HQ"
          (click-prompt state :corp "HQ"))
        (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall is now installed")))))

(deftest victoria-jenkins
  ;; Victoria Jenkins
  (do-game
    (new-game {:corp {:deck ["Victoria Jenkins"]}})
    (play-from-hand state :corp "Victoria Jenkins" "New remote")
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner should have 4 clicks by default")
    (let [victoria (get-content state :remote1 0)]
      (core/rez state :corp victoria)
      (is (= 4 (:click (get-runner))) "Runner should still have 4 clicks when Victoria Jenkins is rezzed")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:click (get-runner))) "Runner should lose 1 click to Victoria Jenkins")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (= 2 (:click (get-runner))) "Runner shouldn't gain a click after trashing Victoria Jenkins")
      (is (= 2 (:agenda-point (get-runner))) "Runner should gain 2 agenda points from trashing Victoria Jenkins")
      (is (= 1 (count (get-scored state :runner))) "Runner should have 1 card in score area")
      (is (zero? (-> (get-corp) :discard count)) "Victoria Jenkins shouldn't go to Archives when trashed"))))

(deftest wall-to-wall
  (testing "Basic functionality"
    (do-game
      (new-game {:corp {:deck ["Wall To Wall" (qty "Hedge Fund" 3)
                               "PAD Campaign" "Ice Wall"]}})
      (play-from-hand state :corp "Wall To Wall" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [w2w (get-content state :remote1 0)
            pad (get-content state :remote2 0)
            iw (get-ice state :hq 0)]
        (core/rez state :corp w2w)
        (take-credits state :corp)
        (dotimes [_ 3]
          (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck))
        (take-credits state :runner)
        (changes-val-macro 1 (:credit (get-corp))
                           "Gained 1 credit"
                           (click-prompt state :corp "Gain 1 [Credits]"))
        (changes-val-macro 1 (count (:hand (get-corp)))
                           "Drew 1 card"
                           (click-prompt state :corp "Draw 1 card"))
        (changes-val-macro 1 (get-counters (refresh iw) :advancement)
                           "Added 1 advancement token"
                           (click-prompt state :corp "Place 1 advancement token on a piece of ice")
                           (click-card state :corp iw))
        (core/rez state :corp pad)
        (take-credits state :corp)
        (take-credits state :runner)
        (changes-val-macro 2 (count (:hand (get-corp)))
                           "Added this asset to HQ (and took mandatory draw)"
                           (click-prompt state :corp "Add this asset to HQ"))
        (is (empty? (:prompt (get-corp))) "No further options because PAD Campaign is rezzed")))))

(deftest warden-fatuma
  ;; Warden Fatuma - rezzed bioroid ice gains an additional sub
  (do-game
    (new-game {:corp {:deck ["Warden Fatuma" "Kakugo"
                             "Eli 2.0" "Ichi 2.0"]}})
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
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 3.0 starts with 2 subs")
      (is (= 3 (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 starts with 3 subs")
      (core/rez state :corp wf)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 4 (count (:subroutines (refresh eli)))) "Eli 2.0 gains 1 sub")
      (is (= 3 (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 stays at 2 subs")
      (core/rez state :corp ichi)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 4 (count (:subroutines (refresh eli)))) "Eli 2.0 stays at 4 subs")
      (is (= 4 (count (:subroutines (refresh ichi)))) "Ichi 2.0 rezzes with 4 subs")
      (core/derez state :corp (refresh wf))
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 reverts")
      (is (= 3 (count (:subroutines (refresh ichi)))) "Ichi 2.0 reverts"))))

(deftest watchdog
  ;; Watchdog - Reduce rez cost of first ICE per turn by number of Runner tags
  (do-game
    (new-game {:corp {:deck ["Watchdog" "Architect" "Wraparound"]}})
    (play-from-hand state :corp "Watchdog" "New remote")
    (play-from-hand state :corp "Wraparound" "HQ")
    (play-from-hand state :corp "Architect" "HQ")
    (let [wd (get-content state :remote1 0)
          arch (get-ice state :hq 1)
          wrap (get-ice state :hq 0)]
      (take-credits state :corp)
      (is (= 4 (:credit (get-corp))))
      (gain-tags state :runner 2)
      (run-on state "HQ")
      (core/rez state :corp wd)
      (core/rez state :corp arch)
      (is (= 2 (:credit (get-corp))) "Only 2 credits to rez Architect")
      (core/rez state :corp wrap)
      (is (zero? (:credit (get-corp))) "No rez discount on Wraparound"))))

(deftest whampoa-reclamation
  ;; Whampoa Reclamation: Enable trashing a card from HQ to place a card in Archives on the bottom of R&D
  (do-game
    (new-game {:corp {:deck [(qty "Whampoa Reclamation" 3)
                             (qty "Global Food Initiative" 3)]}})
    (play-from-hand state :corp "Whampoa Reclamation" "New remote")
    (trash state :corp (find-card "Whampoa Reclamation" (:hand (get-corp))))
    (let [wr (get-content state :remote1 0)
          gfi (find-card "Global Food Initiative" (:hand (get-corp)))]
      (core/rez state :corp wr)
      (take-credits state :corp)
      (card-ability state :corp wr 0)
      (click-card state :corp gfi)
      (click-card state :corp (find-card "Global Food Initiative" (:discard (get-corp))))
      (is (= 1 (count (:discard (get-corp)))) "Only card in discard placed in bottom of R&D")
      (is (= "Global Food Initiative" (-> (get-corp) :deck last :title)) "GFI last card in deck"))))

(deftest worlds-plaza
  ;; Worlds Plaza
  (do-game
    (new-game {:corp {:deck ["Worlds Plaza"
                             "Personalized Portal"
                             "Dedicated Response Team"
                             "Honeyfarm"]}})
    (core/gain state :corp :credit 10 :click 10)
    (play-from-hand state :corp "Worlds Plaza" "New remote")
    (let [plaza (get-content state :remote1 0)]
      (core/rez state :corp plaza)
      (card-ability state :corp plaza 0)
      (let [credits (:credit (get-corp))]
        (card-ability state :corp plaza 0)
        (click-card state :corp (find-card "Personalized Portal" (:hand (get-corp))))
        (is (= (dec credits) (:credit (get-corp))) "Corp should only spend 1 credit to rez Personalized Portal"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp plaza 0)
        (click-card state :corp (find-card "Dedicated Response Team" (:hand (get-corp))))
        (is (= credits (:credit (get-corp))) "Corp should spend 0 credit to rez Dedicated Response Team"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp plaza 0)
        (click-card state :corp (find-card "Honeyfarm" (:hand (get-corp))))
        (is (= credits (:credit (get-corp))) "Corp should spend 0 credit to rez Honeyfarm")))))

(deftest zaibatsu-loyalty
  ;; Zaibatsu Loyalty
  (do-game
    (new-game {:corp {:deck ["Zaibatsu Loyalty" "Ice Wall"]}
               :runner {:deck ["Lemuria Codecracker"]}})
    (core/gain state :corp :click 10 :click 10)
    (play-from-hand state :corp "Zaibatsu Loyalty" "New remote")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 10 :click 10)
    (play-from-hand state :runner "Lemuria Codecracker")
    (let [code (get-hardware state 0)
          iw (get-ice state :remote2 0)
          zai (get-content state :remote1 0)]
      (run-empty-server state "HQ")
      (card-ability state :runner code 0)
      (click-card state :runner (refresh iw))
      (is (some? (prompt-map :corp)) "Corp should get the option to rez Zaibatsu Loyalty before expose")
      (click-prompt state :corp "Yes")
      (is (rezzed? (refresh zai)) "Zaibatsu Loyalty should be rezzed")
      (let [credits (:credit (get-corp))]
        (card-ability state :corp zai 0)
        (is (= (dec credits) (:credit (get-corp))) "Corp should lose 1 credit for stopping the expose")
        (click-prompt state :corp "Done"))
      (card-ability state :runner code 0)
      (click-card state :runner (refresh iw))
      (is (some? (prompt-map :corp)) "Corp should be prompted to prevent")
      (is (zero? (-> (get-corp) :discard count)) "No trashed cards")
      (card-ability state :corp zai 1)
      (is (= 1 (-> (get-corp) :discard count)) "Zaibatsu Loyalty should be in discard after using ability"))))

(deftest zealous-judge
  ;; Zealous Judge
  (do-game
    (new-game {:corp {:deck ["Zealous Judge"]}})
    (play-from-hand state :corp "Zealous Judge" "New remote")
    (let [judge (get-content state :remote1 0)]
      (core/rez state :corp judge)
      (is (not (rezzed? (refresh judge))) "Zealous Judge can't be rezzed until Runner is tagged")
      (gain-tags state :runner 1)
      (core/rez state :corp judge)
      (is (rezzed? (refresh judge)) "Zealous Judge can be rezzed while the Runner is tagged")
      (card-ability state :corp judge 0)
      (is (= 2 (count-tags state)) "Runner should gain a tag from Zealous Judge's ability"))))
