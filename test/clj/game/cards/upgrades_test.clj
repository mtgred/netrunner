(ns game.cards.upgrades-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core.eid :refer [make-eid]]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest amazon-industrial-zone
  ;; Amazon Industrial Zone - Immediately rez ICE installed over its server at 3 credit discount
  (do-game
    (new-game {:corp {:deck ["Spiderweb" "Amazon Industrial Zone"]}})
    (take-credits state :corp 1)
    (play-from-hand state :corp "Amazon Industrial Zone" "New remote")
    (let [aiz (get-content state :remote1 0)]
      (rez state :corp aiz)
      (is (= 2 (:credit (get-corp))))
      (play-from-hand state :corp "Spiderweb" "Server 1")
      (click-prompt state :corp "Yes") ; optional ability
      (let [spid (get-ice state :remote1 0)]
        (is (rezzed? (refresh spid)) "Spiderweb rezzed")
        (is (= 1 (:credit (get-corp))) "Paid only 1 credit to rez")))))

(deftest arella-salvatore
  ;; Arella Salvatore - when an agenda is scored from this server, install a card from hq w/ advancement token
  (testing "Install to server"
    (do-game
      (new-game {:corp {:deck ["Arella Salvatore" "Bryan Stinson" (qty "TGTBT" 2)]}})
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "TGTBT" "Server 1")
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [arella (get-content state :remote1 0)
            same-tg (get-content state :remote1 1)
            diff-tg (get-content state :remote2 0)]
        (rez state :corp arella)
        (score-agenda state :corp (refresh diff-tg))
        (is (empty? (:prompt (get-corp))) "Arella not triggered for different remote score")
        (is (= 1 (count (get-scored state :corp))) "1 Agenda scored")
        (score-agenda state :corp (refresh same-tg))
        (click-card state :corp (find-card "Bryan Stinson" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= 2 (count (get-scored state :corp))) "2 Agendas scored")
        (is (= 1 (count (get-content state :remote3))) "Bryan installed in new remote")
        (is (= 1 (get-counters (get-content state :remote3 0) :advancement)) "Bryan has 1 advancement counter"))))
  (testing "Interaction w/ other on-scored triggers"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Arella Salvatore" "Domestic Sleepers" "Project Vitruvius" "Hedge Fund"]}})
      (starting-hand state :corp ["Arella Salvatore" "Domestic Sleepers"])
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "Server 1")
      (let [arella (get-content state :remote1 0)
            domest (get-content state :remote1 1)]
        (rez state :corp arella)
        (score-agenda state :corp (refresh domest))
        ;; Simultaneous prompt: Sportsmetal automatically triggers, as Arella is silent because there are no installable cards in HQ
        (click-prompt state :corp "Draw 2 cards")
        ;; Arella is no longer silent and now triggers
        (click-card state :corp (find-card "Project Vitruvius" (:hand (get-corp))))
        (click-prompt state :corp "Server 1")
        (is (= 2 (count (get-content state :remote1))) "Agenda installed in server 1")
        (is (= 1 (get-counters (get-content state :remote1 1) :advancement)) "Agenda has 1 advancement counter"))))
  (testing "No cost"
    (do-game
      (new-game {:corp {:deck ["Arella Salvatore" "TGTBT" (qty "Ice Wall" 2)]}})
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "TGTBT" "Server 1")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 1 (count (get-ice state :hq))) "One ice on hq")
      (let [arella (get-content state :remote1 0)
            tg (get-content state :remote1 1)]
        (rez state :corp arella)
        (score-agenda state :corp (refresh tg))
        (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 2 (count (get-ice state :hq))) "Two ice on hq")
        (is (= 1 (get-counters (get-ice state :hq 1) :advancement)) "Ice Wall has 1 counter"))))
  (testing "Overadvanced Vitruvius"
    (do-game
      (new-game {:corp {:deck ["Arella Salvatore" "Project Vitruvius" (qty "Hedge Fund" 2)]}})
      (core/gain state :corp :click 6)
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "Project Vitruvius" "Server 1")
      (let [arella (get-content state :remote1 0)
            vit (get-content state :remote1 1)]
        (rez state :corp arella)
        (advance state vit 4)
        (is (= 4 (get-counters (refresh vit) :advancement)) "Vitruvius should have 4 advancement tokens")
        (core/score state :corp {:card (refresh vit)}))
      (let [vit-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 1 agenda counter")))))

(deftest ash-2x3zb9cy
  ;; Ash 2X3ZB9CY
  (testing "Ash 2X3ZB9CY"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Ash 2X3ZB9CY" "Ice Wall"]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (let [ash (get-content state :hq 0)]
        (rez state :corp ash)
        (run-empty-server state "HQ")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= "Ash 2X3ZB9CY" (-> (prompt-map :runner) :card :title)) "Should access Ash")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (is (not (:run @state)) "Accessing Ash then ends the run"))))
  (testing "Ash+Dirty Laundry interaction"
    (do-game
      (new-game {:corp {:deck ["Ash 2X3ZB9CY"]}
                 :runner {:deck ["Dirty Laundry"]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Dirty Laundry")
      (click-prompt state :runner "Server 1")
      (is (:credit (get-runner) 3) "Runner has 1 credit")
      (run-continue state)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (:credit (get-runner) 3) "Runner still has 3 credits")
      (is (:run @state) "Run is not over")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (:credit (get-runner) 5) "Runner got their laundry money")
      (is (not (:run @state)) "Run not over")))
  (testing "installed in archives #5015"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ash 2X3ZB9CY"]
                        :discard ["Hostile Takeover"]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "Archives")
      (take-credits state :corp)
      (let [ash (get-content state :archives 0)]
        (rez state :corp ash)
        (run-empty-server state "Archives")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (is (empty? (:prompt (get-runner))) "Runner gets no further access prompts")))))

(deftest awakening-center
  ;; Awakening Center
  (testing "Basic Operation"
    (do-game
      (new-game {:corp {:deck ["Awakening Center" "Fairchild"]}})
      (play-from-hand state :corp "Awakening Center" "New remote")
      (let [ac (get-content state :remote1 0)]
        (rez state :corp ac)
        (card-ability state :corp (refresh ac) 0)
        (click-card state :corp (find-card "Fairchild" (:hand (get-corp))))
        (let [fc (first (:hosted (refresh ac)))]
          (is (= "Fairchild" (:title (refresh fc))) "Fairchild hosted on Awakening Center")
          (is (not (rezzed? (refresh fc))) "Fairchild is not rezzed")
          (is (empty? (:hand (get-corp))) "Fairchild removed from hand")
          (take-credits state :corp)
          (run-empty-server state "Server 1")
          (card-ability state :corp (refresh ac) 1)
          (click-prompt state :corp "Fairchild")
          (is (rezzed? (refresh fc)) "Fairchild is rezzed")
          (click-prompt state :runner "Done")
          (is (not (:run @state)) "Run has ended")
          (is (= 1 (count (:discard (get-corp)))) "Fairchild in discard")
          (is (empty? (:hosted (refresh ac))) "Fairchild no longer hosted")))))
  (testing "DDoS Interaction"
    (do-game
      (new-game {:corp {:deck ["Awakening Center" "Fairchild"]}
                 :runner {:deck ["DDoS"]}})
      (play-from-hand state :corp "Awakening Center" "New remote")
      (let [ac (get-content state :remote1 0)]
        (rez state :corp ac)
        (card-ability state :corp (refresh ac) 0)
        (click-card state :corp (find-card "Fairchild" (:hand (get-corp))))
        (let [fc (first (:hosted (refresh ac)))]
          (is (= "Fairchild" (:title (refresh fc))) "Fairchild hosted on Awakening Center")
          (take-credits state :corp)
          (play-from-hand state :runner "DDoS")
          (card-ability state :runner (get-resource state 0) 0)
          (is (= 1 (count (:discard (get-runner)))) "DDoS trashed")
          (run-empty-server state "Server 1")
          (card-ability state :corp (refresh ac) 1)
          (click-prompt state :corp "Fairchild")
          (is (rezzed? (refresh fc)) "Fairchild is rezzed")
          (click-prompt state :runner "Done")
          (is (not (:run @state)) "Run has ended")
          (is (= 1 (count (:discard (get-corp)))) "Fairchild in discard")
          (is (empty? (:hosted (refresh ac))) "Fairchild no longer hosted"))))))

(deftest bamboo-dome
  ;; Bamboo Dome
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Border Control" "Fairchild" "Ice Wall"]
                        :hand ["Bamboo Dome"]}
                 :runner {:deck [(qty "Sure Gamble" 10)]}})
      (play-from-hand state :corp "Bamboo Dome" "R&D")
      (let [bd (get-content state :rd 0)]
        (rez state :corp bd)
        (card-ability state :corp (refresh bd) 0)
        (click-prompt state :corp "Border Control")
        (click-prompt state :corp "Fairchild")
        (click-prompt state :corp "Ice Wall")
        (click-prompt state :corp "Done")
        (is (= "Ice Wall" (:title (first (:deck (get-corp))))))
        (is (= "Fairchild" (:title (second (:deck (get-corp))))))
        (is (= 1 (count (:hand (get-corp)))) "Border Control in hand - hand size 1")
        (is (= "Border Control" (:title (first (:hand (get-corp))))) "Border Control in hand")))))

(deftest ben-musashi
  ;; Ben Musashi
  (testing "Basic test - pay 2 net damage to steal from this server"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "House of Knives"]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [bm (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (prompt-map :runner))))
            "Prompt to pay 2 net damage")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner did not pay 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner bm)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "on R&D access"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "House of Knives"]}})
      (starting-hand state :corp ["Ben Musashi"])
      (play-from-hand state :corp "Ben Musashi" "R&D")
      (take-credits state :corp)
      (let [bm (get-content state :rd 0)]
        (rez state :corp bm)
        (run-empty-server state "R&D")
        ;; runner now chooses which to access.
        (click-prompt state :runner "Card from deck")
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (prompt-map :runner))))
            "Prompt to pay 2 net damage")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner did not pay 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "Ben Musashi")
        (click-prompt state :runner "No action")
        (run-empty-server state "R&D")
        (click-prompt state :runner "Card from deck")
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "pay even when trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Ben Musashi" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [bm (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner bm)
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        ;; should now have prompt to pay 2 net for HoK
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Check runner chooses order of payment"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "Obokata Protocol"]}
                 :runner {:hand [(qty "Sure Gamble" 6)]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "Obokata Protocol" "Server 1")
      (take-credits state :corp)
      (let [bm (get-content state :remote1 0)
            op (get-content state :remote1 1)]
        (rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner op)
        ;; prompt should be asking for the net damage costs
        (is (= "Obokata Protocol" (:title (:card (prompt-map :runner))))
            "Prompt to pay steal costs")
        (click-prompt state :runner "Pay to steal")
        (is (= 6 (count (:discard (get-runner)))) "Runner took 4 net damage")
        (is (= 1 (count (:scored (get-runner)))) "Scored agenda"))))
  (testing "Check Fetal AI can be stolen, #2586"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "Fetal AI"]}
                 :runner {:deck [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "Fetal AI" "Server 1")
      (take-credits state :corp)
      (let [bm (get-content state :remote1 0)
            fai (get-content state :remote1 1)]
        (rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner fai)
        ;; prompt should be asking for the net damage costs
        (is (= "Fetal AI" (:title (:card (prompt-map :runner))))
            "Prompt to pay steal costs")
        (click-prompt state :runner "Pay to steal")
        (is (= 3 (:credit (get-runner))) "Runner paid 2 credits")
        (is (= 4 (count (:discard (get-runner)))) "Runner took 4 net damage - 2 from Fetal, 2 from Ben")
        (is (= 1 (count (:scored (get-runner)))) "Scored agenda")))))

(deftest bernice-mai
  ;; Bernice Mai
  (testing "Basic test - successful and unsuccessful"
    (do-game
      (new-game {:corp {:deck [(qty "Bernice Mai" 3) (qty "Hedge Fund" 3) (qty "Wall of Static" 3)]}})
      (starting-hand state :corp ["Bernice Mai" "Bernice Mai" "Bernice Mai"])
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Bernice Mai" "R&D")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 1 (count-tags state)))
      (is (= 2 (:credit (get-runner))) "Runner paid 3cr to trash Bernice")
      (rez state :corp (get-content state :remote2 0))
      (core/gain state :runner :credit 20)
      (run-empty-server state :remote2)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (not (get-content state :remote2 0)) "Bernice auto-trashed from unsuccessful trace")
      (is (not (:run @state)) "Run ended when Bernice was trashed from server")
      (rez state :corp (get-content state :rd 0))
      (run-empty-server state :rd)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (:card (prompt-map :runner)) "Accessing a card from R&D; not showing Bernice Mai as possible access")))
  (testing "interaction with Dedicated Response Team"
    (do-game
      (new-game {:corp {:deck [(qty "Bernice Mai" 3) "Dedicated Response Team"]}})
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 1 (count-tags state)))
      (is (= 2 (:credit (get-runner))) "Runner paid 3cr to trash Bernice")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 1 meat damage"))))

(deftest bio-vault
  ;; Bio Vault - 2 advancement tokens + trash to end the run
  (do-game
    (new-game {:corp {:deck ["Bio Vault"]}})
    (play-from-hand state :corp "Bio Vault" "New remote")
    (take-credits state :corp)
    (let [bv (get-content state :remote1 0)]
      (run-on state "Server 1")
      (rez state :corp (refresh bv))
      (card-ability state :corp (refresh bv) 0)
      (is (:run @state) "Bio Vault doesn't fire if less than 2 advancements")
      (run-continue state)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (advance state (refresh bv) 2)
      (take-credits state :corp)
      (run-on state "Server 1")
      (card-ability state :corp (refresh bv) 0)
      (is (not (:run @state)) "Bio Vault fires with 2 advancement tokens")
      (is (= 1 (count (:discard (get-corp)))) "Bio Vault trashed"))))

(deftest black-level-clearance
  ;;Black Level Clearance
  (testing "taking brain damage"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Black Level Clearance"]}})
      (play-from-hand state :corp "Black Level Clearance" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (changes-val-macro
        0 (:credit (get-corp))
        "Corp gains 0 credits"
        (click-prompt state :runner "Take 1 brain damage"))
      (is (get-run) "Run has ended")
      (is (get-content state :remote1) "Black Level Clearance has not been trashed")))
  (testing "Jack out"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Black Level Clearance"]}})
      (play-from-hand state :corp "Black Level Clearance" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (changes-val-macro
        5 (:credit (get-corp))
        "Corp gains 5 credits"
        (click-prompt state :runner "Jack out"))
      (is (= ["Hedge Fund"] (map :title (:hand (get-corp)))) "Corp drew 1 card")
      (is (nil? (get-run)) "Run has ended")
      (is (empty? (get-content state :remote1)) "Black Level Clearance has been trashed"))))

(deftest breaker-bay-grid
  ;; Breaker Bay Grid - Reduce rez cost of other cards in this server by 5 credits
  (do-game
    (new-game {:corp {:deck [(qty "Breaker Bay Grid" 2) "Off the Grid" "Strongbox"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Breaker Bay Grid" "New remote")
    (play-from-hand state :corp "Off the Grid" "Server 1")
    (let [bbg1 (get-content state :remote1 0)
          otg (get-content state :remote1 1)]
      (rez state :corp bbg1)
      (rez state :corp otg)
      (is (= 4 (:credit (get-corp))) "Paid only 1 to rez Off the Grid")
      (play-from-hand state :corp "Breaker Bay Grid" "R&D")
      (play-from-hand state :corp "Strongbox" "R&D")
      (let [bbg2 (get-content state :rd 0)
            sbox (get-content state :rd 1)]
        (rez state :corp bbg2)
        (rez state :corp sbox)
        (is (= 1 (:credit (get-corp))) "Paid full 3 credits to rez Strongbox")))))

(deftest bryan-stinson
  ;; Bryan Stinson - play a transaction from archives and remove from game. Ensure Currents are RFG and not trashed.
  (do-game
    (new-game {:corp {:deck ["Bryan Stinson" "Death and Taxes"
                             "Paywall Implementation" "Global Food Initiative"
                             "IPO"]}
               :runner {:deck ["Interdiction"]}})
    (trash-from-hand state :corp "Death and Taxes")
    (play-from-hand state :corp "Bryan Stinson" "New remote")
    (let [bs (get-content state :remote1 0)]
      (rez state :corp (refresh bs))
      (card-ability state :corp (refresh bs) 0)
      (click-prompt state :corp (find-card "Death and Taxes" (:discard (get-corp))))
      (is (find-card "Death and Taxes" (:current (get-corp))) "Death and Taxes is active Current")
      (take-credits state :corp)
      (play-from-hand state :runner "Interdiction")
      (is (find-card "Interdiction" (:current (get-runner))) "Interdiction is active Current")
      (is (find-card "Death and Taxes" (:rfg (get-corp))) "Death and Taxes removed from game")
      (is (not= "Death and Taxes" (:title (first (:discard (get-corp))))) "Death and Taxes not moved to trash")
      (take-credits state :runner)
      (core/lose state :runner :credit 3)
      (trash-from-hand state :corp "Paywall Implementation")
      (card-ability state :corp (refresh bs) 0)
      (click-prompt state :corp (find-card "Paywall Implementation" (:discard (get-corp))))
      (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall Implementation is active Current")
      (is (find-card "Interdiction" (:discard (get-runner))) "Interdiction is trashed")
      (trash-from-hand state :corp "IPO")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (find-card "Paywall Implementation" (:rfg (get-corp))) "Paywall Implementation removed from game")
      (is (not= "Paywall Implementation" (:title (first (:discard (get-corp))))) "Paywall Implementation not moved to trash")
      (take-credits state :runner)
      (core/lose state :runner :credit 3)
      (card-ability state :corp (refresh bs) 0)
      (click-prompt state :corp (find-card "IPO" (:discard (get-corp))))
      (is (find-card "IPO" (:rfg (get-corp))) "IPO is removed from game"))))

(deftest calibration-testing
  ;; Calibration Testing - advanceable / non-advanceable
  (do-game
    (new-game {:corp {:deck [(qty "Calibration Testing" 2) "Project Junebug" "PAD Campaign"]}})
    (core/gain state :corp :credit 10)
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Calibration Testing" "New remote")
    (play-from-hand state :corp "Project Junebug" "Server 1")
    (let [ct (get-content state :remote1 0)
          pj (get-content state :remote1 1)]
      (rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-card state :corp pj)
      (is (= 1 (get-counters (refresh pj) :advancement)) "Project Junebug advanced")
      (is (= 1 (count (:discard (get-corp)))) "Calibration Testing trashed"))
    (play-from-hand state :corp "Calibration Testing" "New remote")
    (play-from-hand state :corp "PAD Campaign" "Server 2")
    (let [ct (get-content state :remote2 0)
          pad (get-content state :remote2 1)]
      (rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-card state :corp pad)
      (is (= 1 (get-counters (refresh pad) :advancement)) "PAD Campaign advanced")
      (is (= 2 (count (:discard (get-corp)))) "Calibration Testing trashed"))))

(deftest caprice-nisei
  ;; Caprice Nisei - Psi game for ETR after runner passes last ice
  (do-game
    (new-game {:corp {:deck [(qty "Caprice Nisei" 3) (qty "Quandary" 3)]}})
    (play-from-hand state :corp "Caprice Nisei" "New remote")
    (take-credits state :corp)
    (let [caprice (get-content state :remote1 0)]
      ;; Check Caprice triggers properly on no ice (and rezzed)
      (rez state :corp caprice)
      (run-on state "Server 1")
      (is (prompt-is-card? state :corp caprice)
          "Caprice prompt even with no ice, once runner makes run")
      (is (prompt-is-card? state :runner caprice) "Runner has Caprice prompt")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (take-credits state :runner)
      (play-from-hand state :corp "Quandary" "Server 1")
      (play-from-hand state :corp "Quandary" "Server 1")
      (take-credits state :corp)
      ;; Check Caprice triggers properly on multiple ice
      (run-on state "Server 1")
      (is (empty? (:prompt (get-corp))) "Caprice not trigger on first ice")
      (run-continue state)
      (run-continue state) ; Caprice should trigger here
      (is (prompt-is-card? state :corp caprice)
          "Corp has Caprice prompt (triggered automatically as runner passed last ice)")
      (is (prompt-is-card? state :runner caprice) "Runner has Caprice prompt")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (not (:run @state)) "Run ended by Caprice")
      (is (empty? (:prompt (get-corp))) "Caprice prompted cleared")
      ;; Check Caprice does not trigger on other servers
      (run-on state "HQ")
      (is (empty? (:prompt (get-corp))) "Caprice does not trigger on other servers"))))

(deftest cayambe-grid
  ;; Cayambe Grid
  (testing "Advance ability"
    (testing "No ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid"]}})
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)]
          (rez state :corp cg))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (empty? (:prompt (get-corp))) "corp has no prompts when no ice is installed in this server")))
    (testing "1 ice in same server"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid" "Enigma"]}})
        (play-from-hand state :corp "Enigma" "HQ")
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)]
          (rez state :corp cg))
        (take-credits state :corp)
        (take-credits state :runner)
        (let [enigma (get-ice state :hq 0)]
          (is (zero? (get-counters (refresh enigma) :advancement)) "Enigma has 0 counters to start")
          (is (= "Place 1 advancement token on an ice protecting HQ" (:msg (prompt-map :corp))) "Correct server in prompt title")
          (click-card state :corp enigma)
          (is (= 1 (get-counters (refresh enigma) :advancement)) "Enigma has 1 counter"))))
    (testing "1 ice in another server"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid" "Enigma"]}})
        (play-from-hand state :corp "Enigma" "New remote")
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)]
          (rez state :corp cg))
        (take-credits state :corp)
        (take-credits state :runner)
        (let [enigma (get-ice state :remote1 0)]
          (is (zero? (get-counters (refresh enigma) :advancement)) "Enigma has 0 counters to start")
          (is (empty? (:prompt (get-corp))) "corp has no prompts when no ice is installed in this server")))))
  (testing "Payment ability"
    (testing "No ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid"]}})
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)]
          (rez state :corp cg)
          (take-credits state :corp)
          (run-on state :hq)
          (let [credits (:credit (get-runner))]
            (is (= "Pay 0 [Credits] or end the run?" (:msg (prompt-map :runner))))
            (click-prompt state :runner "Pay 0 [Credits]")
            (is (= credits (:credit (get-runner))))
            (is (:run @state) "Run hasn't ended")))))
    (testing "1 ice with no counter"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid" "Enigma"]
                          :credits 10}})
        (play-from-hand state :corp "Enigma" "HQ")
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)]
          (rez state :corp cg)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (let [credits (:credit (get-runner))]
            (is (= "Pay 0 [Credits] or end the run?" (:msg (prompt-map :runner))))
            (click-prompt state :runner "Pay 0 [Credits]")
            (is (= credits (:credit (get-runner))))
            (is (:run @state) "Run hasn't ended")))))
    (testing "1 ice with a counter"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid" "Ice Wall"]
                          :credits 10}})
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)
              iw (get-ice state :hq 0)]
          (rez state :corp cg)
          (advance state (refresh iw) 1)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (let [credits (:credit (get-runner))]
            (is (= "Pay 2 [Credits] or end the run?" (:msg (prompt-map :runner))))
            (click-prompt state :runner "Pay 2 [Credits]")
            (is (= (- credits 2) (:credit (get-runner))))
            (is (:run @state) "Run hasn't ended")))))
    (testing "2 ice with counters"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid" (qty "Ice Wall" 2)]
                          :credits 10}})
        (core/gain state :corp :click 10)
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)
              iw1 (get-ice state :hq 0)
              iw2 (get-ice state :hq 1)]
          (rez state :corp cg)
          (advance state (refresh iw1) 1)
          (advance state (refresh iw2) 1)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (run-continue state)
          (let [credits (:credit (get-runner))]
            (is (= "Pay 4 [Credits] or end the run?" (:msg (prompt-map :runner))))
            (click-prompt state :runner "Pay 4 [Credits]")
            (is (= (- credits 4) (:credit (get-runner))))
            (is (:run @state) "Run hasn't ended")))))
    (testing "3 ice with counters"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid" (qty "Ice Wall" 3)]
                          :credits 10}})
        (core/gain state :corp :click 10)
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)
              iw1 (get-ice state :hq 0)
              iw2 (get-ice state :hq 1)
              iw3 (get-ice state :hq 2)]
          (rez state :corp cg)
          (advance state (refresh iw1) 1)
          (advance state (refresh iw2) 1)
          (advance state (refresh iw3) 1)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (run-continue state)
          (run-continue state)
          (let [credits (:credit (get-runner))]
            (is (= "Pay 6 [Credits] or end the run?" (:msg (prompt-map :runner))))
            (is (= ["End the run"] (prompt-buttons :runner)))
            (click-prompt state :runner "End the run")
            (is (= credits (:credit (get-runner))))
            (is (not (:run @state)) "Run has ended")))))
  (testing "Prompt from starting ability"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Cayambe Grid" 2) (qty "Ice Wall" 2)]
                        :credits 10}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Cayambe Grid" "HQ")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Cayambe Grid" "Server 1")
      (let [cg1 (get-content state :hq 0)
            cg2 (get-content state :remote1 0)
            iw1 (get-ice state :hq 0)
            iw2 (get-ice state :remote1 0)]
          (rez state :corp cg1)
          (rez state :corp cg2)
          (take-credits state :corp)
          (take-credits state :runner)
          (click-prompt state :corp cg1)
          (is (= (:msg (prompt-map :corp)) "Place 1 advancement token on an ice protecting HQ")
              "Correct server in prompt title (HQ)")
          (click-card state :corp iw1)
          (is (= (:msg (prompt-map :corp)) "Place 1 advancement token on an ice protecting Server 1")
              "Correct server in prompt title (Server 1)")
          (click-card state :corp iw2))))))

(deftest chilo-city-grid
  ;; ChiLo City Grid - Give 1 tag for successful traces during runs on its server
  (do-game
    (new-game {:corp {:deck [(qty "Resistor" 2) "ChiLo City Grid"]}})
    (play-from-hand state :corp "ChiLo City Grid" "New remote")
    (play-from-hand state :corp "Resistor" "Server 1")
    (play-from-hand state :corp "Resistor" "R&D")
    (take-credits state :corp)
    (let [chilo (get-content state :remote1 0)
          res-r1 (get-ice state :remote1 0)
          res-rd (get-ice state :rd 0)]
      (run-on state "R&D")
      (rez state :corp res-rd)
      (rez state :corp chilo)
      (run-continue state)
      (card-subroutine state :corp res-rd 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (not (:run @state)) "Trace was successful")
      (is (zero? (count-tags state)) "No tags given for run on different server")
      (run-on state "Server 1")
      (rez state :corp res-r1)
      (run-continue state)
      (card-subroutine state :corp res-r1 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (not (:run @state)) "Trace was successful")
      (is (= 1 (count-tags state))
          "Runner took 1 tag given from successful trace during run on ChiLo server"))))

(deftest code-replicator
  ;; Code Replicator - trash to make runner approach passed (rezzed) ice again
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) "Code Replicator"]}
               :runner {:hand ["Corroder"]}})
    (core/gain state :corp :click 1)
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Code Replicator" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state "HQ")
    (is (= 3 (:position (get-in @state [:run]))) "Initial position outermost Ice Wall")
    (let [cr (get-content state :hq 0)
          i1 (get-ice state :hq 0)
          i2 (get-ice state :hq 1)
          i3 (get-ice state :hq 2)
          corr (get-program state 0)]
      (rez state :corp cr)
      (is (= 5 (:credit (get-corp))))
      (rez state :corp i3)
      (run-continue state)
      (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card corr})
      (core/continue state :corp nil)
      (is (= 2 (:position (get-in @state [:run]))) "Passed Ice Wall")
      (card-ability state :corp cr 0)
      (is (= 3 (:position (get-in @state [:run]))) "Runner approaching previous Ice Wall")
      (is (empty? (get-content state :hq))
          "Code Replicatior trashed from root of HQ"))))

(deftest cold-site-server
  ;; Cold Site Server - Increase run cost by 1 cred, 1 click per power counters
  (testing "Cost modification plays nice with derez"
    (do-game
     (new-game {:corp {:deck ["Cold Site Server" "Test Ground"]}
                :runner {:deck ["Dirty Laundry"]}})
     (core/gain state :corp :credit 10 :click 10)
     (play-from-hand state :corp "Cold Site Server" "HQ")
     (play-from-hand state :corp "Test Ground" "New remote")
     (let [css (get-content state :hq 0)
           tg (get-content state :remote1 0)]
       (rez state :corp (refresh css))
       (advance state (refresh tg) 1)
       (card-ability state :corp (refresh css) 0)
       (card-ability state :corp (refresh css) 0)
       (is (= 2 (get-counters (refresh css) :power)) "2 counters placed on Cold Site Server")
       (take-credits state :corp)
       (is (= 5 (:credit (get-runner))))
       (run-on state :hq)
       (is (:run @state) "Run initiated")
       (is (= 3 (:credit (get-runner))) "2 creds spent to run HQ")
       (is (= 1 (:click (get-runner))) "2 extra clicks spent to run HQ")
       (run-jack-out state)
       (rez state :corp (refresh tg))
       (card-ability state :corp (refresh tg) 0)
       (click-card state :corp (refresh css))
       (is (not (rezzed? (refresh css))) "CSS derezzed")
       (core/gain state :runner :click 2)
       (run-on state :hq)
       (is (= 3 (:credit (get-runner))) "0 creds spent to run HQ")
       (is (= 2 (:click (get-runner))) "Only 1 click spent to run HQ")
       (is (:run @state) "Run initiated")
       (run-jack-out state)
       (rez state :corp (refresh css))
       (is (= 2 (get-counters (refresh css) :power)) "Still 2 counters on Cold Site Server")
       (play-from-hand state :runner "Dirty Laundry")
       (is (not (some #{"HQ"} (prompt-buttons :runner)))
           "Runner should not get to choose HQ due to increased cost")
       (click-prompt state :runner "R&D")
       (run-jack-out state)
       (take-credits state :runner)
       (is (= 2 (:credit (get-runner))))
       (is (= 0 (get-counters (refresh css) :power)) "Counters cleared at start of corp turn")
       (take-credits state :corp)
       (is (= 2 (:credit (get-runner))))
       (is (= 4 (:click (get-runner))))
       (run-on state :hq)
       (is (:run @state) "Run initiated")
       (is (= 3 (:click (get-runner))) "No extra cost to run HQ")
       (is (= 2 (:credit (get-runner))) "No extra cost to run HQ")))))

(deftest corporate-troubleshooter
  ;; Corporate Troubleshooter - Pay X credits and trash to add X strength to a piece of rezzed ICE
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 2) "Corporate Troubleshooter"]}})
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Corporate Troubleshooter" "HQ")
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Quandary" "HQ")
    (let [ct (get-content state :hq 0)
          q1 (get-ice state :hq 0)
          q2 (get-ice state :hq 1)]
      (rez state :corp q1)
      (is (= 8 (:credit (get-corp))))
      (rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-card state :corp q2)
      (is (zero? (get-strength (refresh q2))) "Outer Quandary unrezzed; can't be targeted")
      (click-card state :corp q1)
      (click-prompt state :corp "5")
      (is (= 5 (get-strength (refresh q1))) "Inner Quandary boosted to 5 strength")
      (is (empty? (get-content state :hq))
          "Corporate Troubleshooter trashed from root of HQ")
      (take-credits state :corp)
      (is (zero? (get-strength (refresh q1)))
          "Inner Quandary back to default 0 strength after turn ends"))))

(deftest crisium-grid
  ;; Crisium Grid
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 2)]}
                 :runner {:deck ["Desperado" "Temüjin Contract"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (is (= 4 (:credit (get-corp))) "Corp has 4 credits")
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Desperado")
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "HQ")
      (run-empty-server state :hq)
      (is (= 2 (:credit (get-runner))) "No Desperado or Temujin credits")
      (is (not (:successful-run (:register (get-runner)))) "No successful run in register")
      (click-prompt state :runner "Crisium Grid")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (not (:run @state)) "Run ended")))
  (testing "with Gauntlet, #3082"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 2)(qty "Vanilla" 2)]}
                 :runner {:deck ["The Gauntlet" "Corroder"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (play-from-hand state :corp "Vanilla" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "The Gauntlet")
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (get-program state 0)})
      (core/continue state :corp nil)
      (run-continue state)
      (is (seq (:prompt (get-runner))) "The Gauntlet has a prompt")))
  (testing "Crisium Grid prevents first successful run abilities. Issue #5092"
    (do-game
     (new-game {:corp {:hand ["Crisium Grid"]
                       :deck [(qty "Hedge Fund" 3)]}
                :runner {:hand ["Bhagat"]
                         :credits 20}})
     (play-from-hand state :corp "Crisium Grid" "HQ")
     (rez state :corp (get-content state :hq 0))
     (take-credits state :corp)
     (play-from-hand state :runner "Bhagat")
     (run-empty-server state "HQ")
     (click-prompt state :runner "Pay 5 [Credits] to trash")
     (is (= 1 (count (:discard (get-corp)))) "Archive has 1 card (Crisium)")
     (run-empty-server state "HQ")
     (is (= 2 (count (:discard (get-corp)))) "Archive has 2 cards (Crisium and Hedge Fund)"))))

(deftest cyberdex-virus-suite
  ;; Cyberdex Virus Suite
  (testing "Purge ability"
    (do-game
      (new-game {:corp {:deck [(qty "Cyberdex Virus Suite" 3)]}
                 :runner {:deck ["Cache" "Medium"]}})
      (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state (refresh card)))
            cache (find-card "Cache" (get-program state))
            cvs (get-content state :hq 0)]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :runner "Medium")
        (take-credits state :runner 2)
        (rez state :corp cvs)
        (card-ability state :corp cvs 0)
        ;; nothing in hq content
        (is (empty? (get-content state :hq)) "CVS was trashed")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-program state))))
            "Medium has no counters"))))
  (testing "Purge on access"
    (do-game
      (new-game {:corp {:deck [(qty "Cyberdex Virus Suite" 3)]}
                 :runner {:deck ["Cache" "Medium"]}})
      (play-from-hand state :corp "Cyberdex Virus Suite" "New remote")
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state (refresh card)))
            cache (find-card "Cache" (get-program state))
            cvs (get-content state :remote1 0)]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :runner "Medium")
        (run-empty-server state "Server 1")
        ;; corp now has optional prompt to trigger virus purge
        (click-prompt state :corp "Yes")
        ;; runner has prompt to trash CVS
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-program state))))
            "Medium has no counters"))))
  (testing "Don't interrupt archives access, #1647"
    (do-game
      (new-game {:corp {:deck ["Cyberdex Virus Suite" "Braintrust"]}
                 :runner {:deck ["Cache"]}})
      (trash-from-hand state :corp "Cyberdex Virus Suite")
      (trash-from-hand state :corp "Braintrust")
      (take-credits state :corp)
      ;; runner's turn
      ;; install cache
      (play-from-hand state :runner "Cache")
      (let [cache (get-program state 0)]
        (is (= 3 (get-counters (refresh cache) :virus)))
        (run-empty-server state "Archives")
        (click-prompt state :runner "Cyberdex Virus Suite")
        (click-prompt state :corp "Yes")
        (is (pos? (count (:prompt (get-runner)))) "CVS purge did not interrupt archives access")
        ;; purged counters
        (is (zero? (get-counters (refresh cache) :virus))
            "Cache has no counters")))))

(deftest daruma
  ;; Daruma
  (testing "swapping with another installed card"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daruma" "Hostile Takeover" "Snare!"]
                        :credits 10}})
      (play-from-hand state :corp "Daruma" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "Server 1")
      (play-from-hand state :corp "Snare!" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state :remote1)
      (click-prompt state :corp "Yes")
      (click-card state :corp "Hostile Takeover")
      (click-card state :corp "Snare!")
      (is (find-card "Daruma" (:discard (get-corp))))
      (run-continue state)
      (is (= "Pay 4 [Credits] to use Snare! ability?" (:msg (prompt-map :corp))))))
  (testing "swapping with a card in HQ"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daruma" "Hostile Takeover" "Snare!"]
                        :credits 10}})
      (play-from-hand state :corp "Daruma" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "Server 1")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state :remote1)
      (click-prompt state :corp "Yes")
      (click-card state :corp "Hostile Takeover")
      (click-card state :corp "Snare!")
      (run-continue state)
      (is (= "Pay 4 [Credits] to use Snare! ability?" (:msg (prompt-map :corp)))))))

(deftest dedicated-technician-team
  ;; Dedicated Technician Team
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:hand ["Dedicated Technician Team" (qty "Enigma" 3)]}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Enigma" "New remote")
      (play-from-hand state :corp "Enigma" "Server 1")
      (play-from-hand state :corp "Dedicated Technician Team" "Server 1")
      (let [dtt (get-content state :remote1 0)]
        (rez state :corp dtt)
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 3 credits from Dedicated Technician Team"
                           (play-from-hand state :corp "Enigma" "Server 1")
                           (click-card state :corp dtt))))))

(deftest disposable-hq
  ;; Disposable HQ
  (testing "Basic test"
    (do-game
      (new-game {:corp {:hand ["Disposable HQ" "Fire Wall" "Hedge Fund" "Spiderweb"]
                        :deck ["Ice Wall"]}})
      (play-from-hand state :corp "Disposable HQ" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes")
      (click-card state :corp "Fire Wall")
      (click-card state :corp "Hedge Fund")
      (click-card state :corp "Spiderweb")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (empty? (:prompt (get-corp))) "Corp should be waiting on Runner")
      (is (empty? (:prompt (get-runner))) "Runner should be able to take actions")
      (is (= ["Ice Wall" "Fire Wall" "Hedge Fund" "Spiderweb"]
             (->> (get-corp) :deck (take 4) (map :title) (into [])))
          "Deck should be ordered top to bottom")))
  (testing "Handles eid when cancelled. Issue #4912"
    (do-game
      (new-game {:corp {:hand ["Disposable HQ" "Fire Wall" "Hedge Fund" "Spiderweb"]
                        :deck ["Ice Wall"]}})
      (play-from-hand state :corp "Disposable HQ" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (empty? (:prompt (get-corp))) "Corp should be waiting on Runner")
      (is (empty? (:prompt (get-runner))) "Runner should be able to take actions"))))

(deftest drone-screen
  ;; Drone Screen
  (do-game
    (new-game {:corp {:deck ["Drone Screen"]}})
    (play-from-hand state :corp "Drone Screen" "New remote")
    (let [drone (get-content state :remote1 0)]
      (rez state :corp drone)
      (gain-tags state :runner 1)
      (take-credits state :corp)
      (run-on state "Server 1")
      (is (zero? (-> (get-runner) :discard count)) "Heap should start empty")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should discard 1 card from meat damage from losing Drone Screen trace"))))

(deftest embolus
  ;; Embolus - 1 power token to end the run, tokens are lost on successful runs
  (do-game
    (new-game {:corp {:deck ["Embolus"]}})
    (play-from-hand state :corp "Embolus" "New remote")
    (take-credits state :corp)
    (let [em (get-content state :remote1 0)]
      (rez state :corp em)
      (take-credits state :runner)
      (let [credits (:credit (get-corp))
            powers (get-counters (refresh em) :power)]
        (is (zero? powers) "Embolus is rezzed with 0 counters")
        (click-prompt state :corp "Yes")
        (is (= (dec credits) (:credit (get-corp)))
            "Adding power counters costs a credit")
        (is (= 1 (get-counters (refresh em) :power))
            "A power counter was added"))
      (take-credits state :corp)
      (run-on state "HQ")
      (card-ability state :corp (refresh em) 1) ; try to etr
      (is (and (:run @state) (= 1 (get-counters (refresh em) :power)))
          "Embolus doesn't fire during a run on other servers")
      (run-continue state)
      (is (zero? (get-counters (refresh em) :power))
          "A successful run removes counters")
      ;; (click-prompt state :runner "No action")
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (is (= 1 (get-counters (refresh em) :power)) "A counter was added")
      (take-credits state :corp)
      (run-on state "Server 1")
      (card-ability state :corp (refresh em) 1) ; try to etr
      (is (and (not (:run @state)) (zero? (get-counters (refresh em) :power)))
          "Embolus spent a counter to ETR"))))

(deftest forced-connection
  ;; Forced Connection - ambush, trace(3) give the runner 2 tags
  (do-game
    (new-game {:corp {:deck [(qty "Forced Connection" 3)]}})
    (starting-hand state :corp ["Forced Connection" "Forced Connection"])
    (play-from-hand state :corp "Forced Connection" "New remote")
    (take-credits state :corp)
    (is (zero? (count-tags state)) "Runner starts with 0 tags")
    (run-empty-server state :remote1)
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (is (= 2 (count-tags state)) "Runner took two tags")
    (run-empty-server state "Archives")
    (is (= 2 (count-tags state)) "Runner doesn't take tags when accessed from Archives")
    (run-empty-server state "HQ")
    (click-prompt state :corp "0")
    (click-prompt state :runner "3")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (is (= 2 (count-tags state)) "Runner doesn't take tags when trace won")))

(deftest ganked
  ;; Ganked!
  (testing "Access ability and firing subs"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Ganked!"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (let [iw (get-ice state :rd 0)]
        (rez state :corp iw)
        (take-credits state :corp)
        (run-empty-server state :hq)
        (last-log-contains? state "Runner accesses Ganked!")
        (is (= "Trash Ganked! to force the Runner to encounter a piece of ice?"
               (:msg (prompt-map :corp))) "Corp has Ganked! prompt")
        (click-prompt state :corp "Yes")
        (is (= :select (prompt-type :corp)))
        (is (= :waiting (prompt-type :runner)))
        (click-card state :corp iw)
        (is (= "You are encountering Ice Wall. Allow its subroutine to fire?"
               (:msg (prompt-map :runner))) "Runner has Ice Wall fake encounter prompt")
        (is (= :waiting (prompt-type :corp)))
        (click-prompt state :runner "Yes")
        (is (not (get-run)) "Run has been ended")
        (last-log-contains? state "Corp resolves 1 unbroken subroutine on Ice Wall")
        (is (empty? (:prompt (get-corp))) "No more prompts")
        (is (empty? (:prompt (get-runner))) "No more prompts")
        (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
        (is (empty? (remove :seen (:discard (get-corp)))) "Cards in Archives are faceup"))))
  (testing "Access ability and not firing subs"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Ganked!"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (let [iw (get-ice state :rd 0)]
        (rez state :corp iw)
        (take-credits state :corp)
        (run-empty-server state :hq)
        (last-log-contains? state "Runner accesses Ganked!")
        (is (= "Trash Ganked! to force the Runner to encounter a piece of ice?"
               (:msg (prompt-map :corp))) "Corp has Ganked! prompt")
        (click-prompt state :corp "Yes")
        (is (= :select (prompt-type :corp)))
        (is (= :waiting (prompt-type :runner)))
        (click-card state :corp iw)
        (is (= "You are encountering Ice Wall. Allow its subroutine to fire?"
               (:msg (prompt-map :runner))) "Runner has Ice Wall fake encounter prompt")
        (is (= :waiting (prompt-type :corp)))
        (click-prompt state :runner "No")
        (is (not (get-run)) "Run has been ended")
        (last-log-contains? state "Corp resolves 1 unbroken subroutine on Ice Wall")
        (is (empty? (:prompt (get-corp))) "No more prompts")
        (is (empty? (:prompt (get-runner))) "No more prompts"))))
  (testing "No access ability when there are no rezzed ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Ganked!"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (run-empty-server state :hq)
      (last-log-contains? state "Runner accesses Ganked!")
      (is (= "You accessed Ganked!." (:msg (prompt-map :runner))) "Runner has normal access prompt")
      (click-prompt state :runner "No action")
      (is (not (get-run)) "Run has been ended")
      (is (empty? (:prompt (get-corp))) "No more prompts")
      (is (empty? (:prompt (get-runner))) "No more prompts"))))

(deftest georgia-emelyov
  ;; Georgia Emelyov
  (do-game
    (new-game {:corp {:deck ["Georgia Emelyov"]}})
    (play-from-hand state :corp "Georgia Emelyov" "New remote")
    (let [geo (get-content state :remote1 0)]
      (rez state :corp geo)
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-jack-out state)
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (card-ability state :corp (refresh geo) 0)
      (click-prompt state :corp "Archives")
      (let [geo (get-content state :archives 0)]
        (is geo "Georgia moved to Archives")
        (run-on state "Archives")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-runner)))) "Runner took 1 net damage")
        (run-on state "HQ")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-runner)))) "Runner did not take damage")))))

(deftest giordano-memorial-field
  ;; Giordano Memorial Field
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Giordano Memorial Field" "Hostile Takeover"]}
                 :runner {:deck [(qty "Fan Site" 3)]}})
      (play-from-hand state :corp "Giordano Memorial Field" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (let [credits (:credit (get-runner))]
        (click-prompt state :runner "Pay 6 [Credits]")
        (is (= (- credits 6) (:credit (get-runner))) "Runner pays 6 credits to not end the run"))
      (click-prompt state :runner "No action")
      (run-empty-server state "Server 1")
      (is (= ["End the run"] (prompt-buttons :runner)) "Only choice should be End the run")
      (click-prompt state :runner "End the run")
      (is (not (:run @state)) "Run should be ended from Giordano Memorial Field ability")))
  (testing "Ending the run doesn't mark the run as unsuccessful. Issue #4223"
    (do-game
      (new-game {:corp {:hand ["Giordano Memorial Field" "Hostile Takeover"]}
                 :runner {:hand [(qty "Fan Site" 2) "John Masanori"]
                          :credit 10}})
      (play-from-hand state :corp "Giordano Memorial Field" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "John Masanori")
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (zero? (count-tags state)))
      (let [credits (:credit (get-runner))]
        (click-prompt state :runner "End the run")
        (is (zero? (count-tags state)) "Don't gain a tag from John Masanori")))))

(deftest helheim-servers
  ;; Helheim Servers - Full test
  (do-game
    (new-game {:corp {:deck ["Helheim Servers" "Gutenberg" "Vanilla"
                             "Jackson Howard" "Hedge Fund"]}})
    (play-from-hand state :corp "Helheim Servers" "R&D")
    (play-from-hand state :corp "Gutenberg" "R&D")
    (play-from-hand state :corp "Vanilla" "R&D")
    (take-credits state :corp)
    (run-on state "R&D")
    (is (:run @state))
    (let [helheim (get-content state :rd 0)
          gutenberg (get-ice state :rd 0)
          vanilla (get-ice state :rd 1)]
      (rez state :corp helheim)
      (rez state :corp gutenberg)
      (rez state :corp vanilla)
      (is (= 6 (get-strength (refresh gutenberg))))
      (is (zero? (get-strength (refresh vanilla))))
      (card-ability state :corp helheim 0)
      (click-card state :corp "Jackson Howard")
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 8 (get-strength (refresh gutenberg))))
      (is (= 2 (get-strength (refresh vanilla))))
      (card-ability state :corp helheim 0)
      (click-card state :corp "Hedge Fund")
      (is (= 2 (count (:discard (get-corp)))))
      (is (= 10 (get-strength (refresh gutenberg))))
      (is (= 4 (get-strength (refresh vanilla))))
      (run-continue state)
      (run-jack-out state)
      (is (not (:run @state)))
      (is (= 6 (get-strength (refresh gutenberg))))
      (is (zero? (get-strength (refresh vanilla)))))))

(deftest henry-phillips
  ;; Henry Philips - gain 2c when runner is tagged and breaks a sub on this server
  (testing "Basic behavior"
    (do-game
      (new-game {:corp {:deck ["Henry Phillips" "Hadrian's Wall"]
                        :credits 30}
                 :runner {:deck ["Paperclip"]
                          :credits 30}})
      (play-from-hand state :corp "Henry Phillips" "New remote")
      (play-from-hand state :corp "Hadrian's Wall" "Server 1")
      (rez state :corp (get-content state :remote1 0))
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Paperclip")
      (let [pc (get-program state 0)
            corp-creds (:credit (get-corp))]
        (run-on state :remote1)
        (run-continue state)
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card pc})
        (core/continue state :corp nil)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (= corp-creds (:credit (get-corp))) "Henry doesn't gain credits if runner not tagged")
        (gain-tags state :runner 1)
        (run-on state :remote1)
        (run-continue state)
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card pc})
        (core/continue state :corp nil)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (= (+ 4 corp-creds) (:credit (get-corp))) "Henry gains 4 credits if runner tagged and breaks two subs")))))

(deftest hired-help
  ;; Hired Help - trash an agenda if you wanna run my server
  (testing "Normal usage"
    (do-game
     (new-game {:corp {:deck ["Hired Help" "Accelerated Beta Test"]}})
     (play-from-hand state :corp "Hired Help" "New remote")
     (play-from-hand state :corp "Accelerated Beta Test" "New remote")
     (take-credits state :corp)
     (core/gain state :runner :click 1)
     (rez state :corp (get-content state :remote1 0))
     (let [hh (get-content state :remote1 0)]
       (run-on state :remote1)
       (is (prompt-is-card? state :runner (refresh hh)) "Runner prompt is on Hired Help")
       (click-prompt state :runner "End the run")
       (is (not (:run @state)) "Run ended")
       (run-empty-server state :remote2)
       (click-prompt state :runner "Steal")
       (run-on state :remote1)
       (click-prompt state :runner "Trash 1 scored agenda")
       (click-card state :runner (get-scored state :runner 0))
       (is (= 1 (count (:discard (get-corp)))) "ABT trashed")
       (is (zero? (count (:scored (get-runner)))) "No stolen agendas")
       (run-jack-out state)
       (run-empty-server state :hq)
       (run-on state :remote1)
       (is (empty? (:prompt (get-runner))) "No Hired Help prompt"))))
  (testing "Crisium Grid, fake agenda interactions"
    (do-game
     (new-game {:corp {:deck ["Hired Help" "Crisium Grid"]}
                :runner {:deck ["Notoriety"]}})
     (play-from-hand state :corp "Hired Help" "New remote")
     (play-from-hand state :corp "Crisium Grid" "HQ")
     (take-credits state :corp)
     (rez state :corp (get-content state :remote1 0))
     (let [hh (get-content state :remote1 0)]
       (run-empty-server state :archives)
       (run-empty-server state :rd)
       (run-empty-server state :hq)
       (click-prompt state :runner "No action")
       (play-from-hand state :runner "Notoriety")
       (is (= 1 (count (:scored (get-runner)))) "Notoriety scored")
       (take-credits state :runner)
       (take-credits state :corp)
       (run-on state :remote1)
       (is (prompt-is-card? state :runner (refresh hh)) "Runner prompt is on Hired Help")
       (click-prompt state :runner "Trash 1 scored agenda")
       (click-card state :runner (get-scored state :runner 0))
       (is (zero? (count (:discard (get-corp)))) "Notoriety does not go in Corp discard")
       (is (= 1 (count (:discard (get-runner)))) "Notoriety is in Runner discard")
       (is (zero? (count (:scored (get-runner)))) "No stolen agendas")
       (run-jack-out state)
       (rez state :corp (get-content state :hq 0)) ;rez crisium
       (run-empty-server state :hq)
       (click-prompt state :runner "No action")
       (run-on state :remote1)
       (is (prompt-is-card? state :runner (refresh hh))
           "Runner prompt is on Hired Help despite HQ run because of Crisium")))))

(deftest hokusai-grid
  ;; Hokusai Grid - Do 1 net damage when run successful on its server
  (do-game
    (new-game {:corp {:deck ["Hokusai Grid"]}})
    (play-from-hand state :corp "Hokusai Grid" "HQ")
    (take-credits state :corp)
    (rez state :corp (get-content state :hq 0))
    (run-empty-server state :rd)
    (is (empty? (:discard (get-runner))) "No net damage done for successful run on R&D")
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-runner)))) "1 net damage done for successful run on HQ")))

(deftest increased-drop-rates
  ;; Increased Drop Rates
  (do-game
    (new-game {:corp {:deck [(qty "Increased Drop Rates" 2)]}})
    (core/gain state :corp :bad-publicity 1)
    (starting-hand state :corp ["Increased Drop Rates"])
    (play-from-hand state :corp "Increased Drop Rates" "New remote")
    (take-credits state :corp)
    (run-empty-server state "R&D")
    (is (= 0 (count-tags state)))
    (click-prompt state :runner "Yes")
    (is (= 1 (count-tags state)) "Runner takes 1 tag to prevent Corp from removing 1 BP")
    (click-prompt state :runner "Pay 2 [Credits] to trash") ; trash
    (run-empty-server state "Archives")
    (is (= 1 (count-bad-pub state)))
    (click-prompt state :runner "No")
    (is (= 0 (count-bad-pub state)) "Runner declines to take tag, Corp removes 1 BP")))

(deftest intake
  ;; Intake - Trace4, add an installed program or virtual resource to the grip
  (do-game
    (new-game {:corp {:deck [(qty "Intake" 3)]}
               :runner {:deck ["Corroder" "Fester" "Daily Casts"]}})
    (starting-hand state :corp ["Intake" "Intake"])
    (play-from-hand state :corp "Intake" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 5 :credit 10)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Fester")
    (play-from-hand state :runner "Daily Casts")
    (run-empty-server state "R&D")
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (empty? (:hand (get-runner))) "Runner starts with no cards in hand")
    (click-card state :corp (get-program state 0))
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (run-empty-server state "Archives")
    (is (empty? (:prompt (get-corp))) "No prompt from Archives access")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (run-empty-server state "Server 1")
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (click-card state :corp (get-resource state 0))
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (click-prompt state :runner "No action") ; trash
    (run-empty-server state "HQ")
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (click-prompt state :corp "Done")
    (click-prompt state :runner "No action") ; trash
    (is (empty? (:prompt (get-corp))) "Prompt closes after done")
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (run-empty-server state "HQ")
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "5")
    (is (empty? (:prompt (get-corp))) "Prompt closes after lost trace")))

(deftest jinja-city-grid
  ;; Jinja City Grid - install drawn ice, lowering install cost by 4
  (testing "Single draws"
    (do-game
      (new-game {:corp {:deck [(qty "Vanilla" 3) (qty "Ice Wall" 3)]
                        :hand ["Jinja City Grid"]}})
      (core/gain state :corp :click 6)
      (play-from-hand state :corp "Jinja City Grid" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (is (= 4 (:credit (get-corp))) "Starts with 4 credits")
      (dotimes [n 5]
        (click-draw state :corp)
        (click-prompt state :corp (first (prompt-buttons :corp)))
        (is (= 4 (:credit (get-corp))) "Not charged to install ice")
        (is (= (inc n) (count (get-in @state [:corp :servers :remote1 :ices]))) (str n " ICE protecting Remote1")))
      (click-draw state :corp)
      (click-prompt state :corp (first (prompt-buttons :corp)))
      (is (= 3 (:credit (get-corp))) "Charged to install ice")
      (is (= 6 (count (get-in @state [:corp :servers :remote1 :ices]))) "6 ICE protecting Remote1")))
  (testing "Drawing non-ice on runner's turn"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Jinja City Grid"]}
                 :runner {:id "Laramy Fisk: Savvy Investor"
                          :deck ["Eden Shard"]}})
      (play-from-hand state :corp "Jinja City Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (run-empty-server state :rd)
      (is (= "Force the Corp to draw a card?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= :waiting (prompt-type :runner)) "Runner has wait prompt")
      (is (= :bogus (prompt-type :corp)) "Corp has a bogus prompt to fake out the runner")
      (click-prompt state :corp "Carry on!"))))

(deftest keegan-lane
  ;; Keegan Lane - Trash self and remove 1 Runner tag to trash a program
  (do-game
    (new-game {:corp {:deck ["Keegan Lane"]}
               :runner {:deck ["Corroder"]}})
    (play-from-hand state :corp "Keegan Lane" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state :hq)
    (let [keeg (get-content state :hq 0)]
      (rez state :corp keeg)
      (card-ability state :corp keeg 0)
      (is (= 1 (count (get-content state :hq))) "Keegan didn't fire, Runner has no tags")
      (gain-tags state :runner 2)
      (card-ability state :corp keeg 0)
      (click-card state :corp (get-program state 0))
      (is (= 1 (count-tags state)) "1 tag removed")
      (is (= 1 (count (:discard (get-corp)))) "Keegan trashed")
      (is (= 1 (count (:discard (get-runner)))) "Corroder trashed"))))

(deftest khondi-plaza
  ;; Khondi Plaza
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:hand ["Khondi Plaza" "Ice Wall" "Enigma" (qty "PAD Campaign" 3)]}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Khondi Plaza" "New remote")
      (play-from-hand state :corp "Enigma" "Server 1")
      (dotimes [c 3] (play-from-hand state :corp "PAD Campaign" "New remote"))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [kh (get-content state :remote1 0)
            en (get-ice state :remote1 0)]
        (rez state :corp kh)
        (is (= 4 (get-counters (refresh kh) :recurring)) "4 recurring credits on Khondi")
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 3 credits from Khondi Plaza"
                           (rez state :corp en)
                           (dotimes [c 3] (click-card state :corp kh)))))))

(deftest la-costa-grid
  (testing "La Costa Grid cannot be installed in a central server"
    (do-game
      (new-game {:corp {:hand ["La Costa Grid"]}})
      (is (not (some #{"HQ" "R&D" "Archives"} (prompt-buttons :corp))) "Central servers are not listed in the install prompt")))
  (testing "At the start of their turn The Corp may place an advancement token on a card in La Costa Grid's server"
    (do-game
      (new-game {:corp {:hand ["La Costa Grid", "Breaking News"]}})
      (play-from-hand state :corp "La Costa Grid" "New remote")
      (play-from-hand state :corp "Breaking News" "Server 1")
      (let [[la-costa breaking-news] (get-content state :remote1)]
        (rez state :corp la-costa)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (empty? (:prompt (get-corp)))) "The Corp is prompted to place one advancement token on a card")
        (click-card state :corp la-costa)
        (is (= 1 (get-counters (refresh la-costa) :advancement)) "Clicking on La Costa Grid advances it")
        (take-credits state :corp)
        (take-credits state :runner)
        (click-card state :corp breaking-news)
        (is (= 1 (get-counters (refresh breaking-news) :advancement)) "Clicking on a card in La Costa Grid's server advances it"))))
  (testing "The Corp may not advance cards which are not in La Costa Grid's server"
    (do-game
      (new-game {:corp {:hand ["La Costa Grid", (qty "Mumbad Virtual Tour" 2), (qty "Vanilla" 3)]}})
      (play-from-hand state :corp "La Costa Grid" "New remote")
      (let [[la-costa] (get-content state :remote1)]
        (rez state :corp la-costa)
        (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
        (let [[remote-mvt] (get-content state :remote2)]
          (take-credits state :corp)
          (take-credits state :runner)
          (click-card state :corp remote-mvt)
          (is (not (empty? (:prompt (get-corp)))) "Clicking a card in a different remote does not clear the prompt")
          (is (zero? (get-counters (refresh remote-mvt) :advancement)) "Clicking a card in a different remote does not advance it"))
        (play-from-hand state :corp "Mumbad Virtual Tour" "HQ")
        (let [[central-mvt] (get-content state :hq)]
          (take-credits state :corp)
          (take-credits state :runner)
          (click-card state :corp central-mvt)
          (is (not (empty? (:prompt (get-corp)))) "Clicking a card in a central does not clear the prompt")
          (is (zero? (get-counters (refresh central-mvt) :advancement)) "Clicking a card in a central does not advance it"))
        (play-from-hand state :corp "Vanilla" "Server 1")
        (let [[vanilla] (get-ice state :remote1)]
          (take-credits state :corp)
          (take-credits state :runner)
          (click-card state :corp vanilla)
          (is (not (empty? (:prompt (get-corp)))) "Clicking an ice protecting La Costa Grid does not clear the prompt")
          (is (zero? (get-counters (refresh vanilla) :advancement)) "Clicking a an ice protecting La Costa Grid does not advance it"))
        (play-from-hand state :corp "Vanilla" "Server 2")
        (let [[remote-vanilla] (get-ice state :remote2)]
          (take-credits state :corp)
          (take-credits state :runner)
          (click-card state :corp remote-vanilla)
          (is (not (empty? (:prompt (get-corp)))) "Clicking an ice protecting La Costa Grid does not clear the prompt")
          (is (zero? (get-counters (refresh remote-vanilla) :advancement)) "Clicking a an ice protecting La Costa Grid does not advance it"))
        (play-from-hand state :corp "Vanilla" "HQ")
        (let [[central-vanilla] (get-ice state :hq)]
          (take-credits state :corp)
          (take-credits state :runner)
          (click-card state :corp central-vanilla)
          (is (not (empty? (:prompt (get-corp)))) "Clicking an ice protecting HQ does not clear the prompt")
          (is (zero? (get-counters (refresh central-vanilla) :advancement)) "Clicking a an ice protecting HQ does not advance it")))))
  (testing "The Corp may advance hosted cards in La Costa Grid's server"
    (do-game
      (new-game {:corp {:hand ["La Costa Grid", "Full Immersion RecStudio", "Project Beale"]}})
      (play-from-hand state :corp "La Costa Grid" "New remote")
      (play-from-hand state :corp "Full Immersion RecStudio" "Server 1")
      (let [[la-costa recstudio] (get-content state :remote1)]
        (rez state :corp recstudio)
        (card-ability state :corp recstudio 0)
        (click-card state :corp (first (:hand (get-corp))))
        (let [[beale] (:hosted (refresh recstudio))]
          (rez state :corp la-costa)
          (take-credits state :corp)
          (take-credits state :runner)
          (click-card state :corp beale)
          (is (= 1 (get-counters (refresh beale) :advancement)) "Clicking on a hosted card in the La Costa Grid server advances it")))))
  (testing "Properly async. #5049"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Ice Wall"]
                        :hand ["Daily Business Show" "La Costa Grid" "Project Beale"]
                        :credits 10}})
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (play-from-hand state :corp "La Costa Grid" "New remote")
      (rez state :corp (get-content state :remote2 0))
      (play-from-hand state :corp "Project Beale" "Server 2")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= "Select a card in Server 2" (:msg (prompt-map :corp))))
      (click-card state :corp "Project Beale")
      (last-log-contains? state "La Costa Grid to place an advancement token on a card in Server 2")
      (is (= "Select 1 card to add to the bottom of R&D" (:msg (prompt-map :corp))))
      (click-card state :corp "Ice Wall")
      (last-log-contains? state "Daily Business Show to add 1 card to the bottom of R&D")
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner)))))))

(deftest letheia-nisei
  ;; Letheia Nisei
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Letheia Nisei" "Ice Wall" "Vanilla"]
                      :credits 10}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Vanilla" "R&D")
    (play-from-hand state :corp "Letheia Nisei" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [letheia (get-content state :rd 0)
          cor (get-program state 0)]
      (rez state :corp letheia)
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 1))
      (run-continue state)
      (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card cor})
      (core/continue state :corp nil)
      (run-continue state)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (zero? (:position (:run @state))) "Runner should be approaching the server")
      (click-prompt state :corp "Yes")
      (is (= 2 (:position (:run @state))) "Runner should be approaching outermost ice")
      (is (nil? (refresh letheia)) "Letheia is trashed")
      (is (find-card "Letheia Nisei" (:discard (get-corp))) "Letheia is in Archives"))))

(deftest manta-grid
  ;; If the Runner has fewer than 6 or no unspent clicks on successful run, corp gains a click next turn.
  (do-game
    (new-game {:corp {:deck ["Manta Grid"]}})
    (starting-hand state :runner [])
    (is (= 3 (:click (get-corp))) "Corp has 3 clicks")
    (play-from-hand state :corp "Manta Grid" "HQ")
    (rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (click-draw state :runner)
    (click-draw state :runner)
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action") ; don't trash Manta Grid
    (is (= 1 (:click (get-runner))) "Running last click")
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action") ; don't trash Manta Grid
    (take-credits state :runner)
    (is (= 5 (:click (get-corp))) "Corp gained 2 clicks due to 2 runs with < 6 Runner credits")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp back to 3 clicks")
    (take-credits state :corp)
    (take-credits state :runner 3)
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action") ; don't trash Manta Grid
    (take-credits state :runner)
    (is (= 4 (:click (get-corp))) "Corp gained a click due to running last click")))

(deftest marcus-batty
  ;; Marcus Batty
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Marcus Batty" "Ice Wall"]
                        :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Marcus Batty" "HQ")
      (let [iw (get-ice state :hq 0)
            mb (get-content state :hq 0)]
        (rez state :corp mb)
        (take-credits state :corp)
        (run-on state "HQ")
        (rez state :corp iw)
        (card-ability state :corp mb 0)
        (is (= :psi (prompt-type :corp)))
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (click-card state :corp iw)
        (click-prompt state :corp "End the run")
        (is (not (:run @state)) "Run has ended")
        (is (nil? (refresh mb)) "Marcus Batty is trashed")))))

(deftest midori
  ;; Midori
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Midori" "Ice Wall" "Anansi"]}})
    (play-from-hand state :corp "Midori" "HQ")
    (rez state :corp (get-content state :hq 0))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (run-on state "HQ")
    (click-prompt state :corp "Yes")
    (click-card state :corp "Anansi")
    (let [current-ice (core/get-current-ice state)]
      (is (= "Anansi" (:title current-ice)))
      (is (not (rezzed? current-ice))))
    (is (= ["Ice Wall"] (map :title (:hand (get-corp))))
        "Ice Wall has been added to hand")))

(deftest midway-station-grid
  ;; Midway Station Grid
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Midway Station Grid" (qty "Battlement" 2)]
                      :credits 20}
               :runner {:hand ["Corroder"]
                        :credits 10}})
    (play-from-hand state :corp "Midway Station Grid" "HQ")
    (rez state :corp (get-content state :hq 0))
    (play-from-hand state :corp "Battlement" "HQ")
    (play-from-hand state :corp "Battlement" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [corroder (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (changes-val-macro
        -1 (:credit (get-runner))
        "Runner loses 1 credit only for boosting strength"
        (card-ability state :runner corroder 1))
      (changes-val-macro
        -2 (:credit (get-runner))
        "Runner should lose 2 credits, 1 for Midway Station, 1 for base ability"
        (card-ability state :runner corroder 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Done"))
      (run-jack-out state)
      (run-on state "Server 1")
      (rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (changes-val-macro
        -1 (:credit (get-runner))
        "Runner loses 1 credit only for running on a different server"
        (card-ability state :runner corroder 0)
        (click-prompt state :runner "End the run")))))

(deftest mumbad-city-grid
  ;; Mumbad City Grid - when runner passes a piece of ice, swap that ice with another from this server
  (testing "1 ice"
    (do-game
      (new-game {:corp {:deck ["Mumbad City Grid" "Quandary"]}})
      (play-from-hand state :corp "Mumbad City Grid" "New remote")
      (play-from-hand state :corp "Quandary" "Server 1")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state "Server 1")
      (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "1 ice on server")
      (run-continue state)
      (is (empty? (:prompt (get-corp))))
      (run-jack-out state)
      (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "Still 1 ice on server")))
  (testing "fire before pass"
    (do-game
      (new-game {:corp {:deck ["Mumbad City Grid" "Enigma" "Quandary" "Wall of Static" "Ice Wall"]
                        :credits 10}})
      (play-from-hand state :corp "Mumbad City Grid" "New remote")
      (play-from-hand state :corp "Quandary" "Server 1")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state "Server 1")
      (is (= 2 (:position (:run @state))) "Runner at position 2")
      (is (= 2 (count (get-in @state [:corp :servers :remote1 :ices]))) "2 ice on server")
      (is (= "Quandary" (:title (first (get-in @state [:corp :servers :remote1 :ices])))) "Quandary inner ice")
      (is (= "Ice Wall" (:title (second (get-in @state [:corp :servers :remote1 :ices])))) "Ice Wall outer ice")
      (run-continue state)
      (click-card state :corp "Quandary")
      (is (= "Quandary" (:title (second (get-in @state [:corp :servers :remote1 :ices])))) "Quandary outer ice")
      (is (= "Ice Wall" (:title (first (get-in @state [:corp :servers :remote1 :ices])))) "Ice Wall inner ice")
      (run-jack-out state)
      (is (= 2 (count (get-in @state [:corp :servers :remote1 :ices]))) "Still 2 ice on server")
      (take-credits state :runner)
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Wall of Static" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (empty? (:prompt (get-corp))) "Mumbad doesn't trigger on other servers"))))

(deftest mumbad-virtual-tour
  ;; Tests that Mumbad Virtual Tour forces trash
  (do-game
    (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 2)]}})
    (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    ;; MVT does not force trash when not installed
    (click-prompt state :runner "No action")
    (is (= 5 (:credit (get-runner))) "Runner not forced to trash MVT in HQ")
    (is (empty? (:discard (get-corp))) "MVT in HQ is not trashed")
    (run-empty-server state "Server 1")
    (is (= ["Pay 5 [Credits] to trash"] (prompt-buttons :runner)) "Should only have a single option")
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (is (zero? (:credit (get-runner))) "Runner forced to trash MVT")
    (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed"))
  (testing "interaction with Imp"
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 2)]}
                 :runner {:deck ["Imp"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      ;; Reset credits to 5
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 1")
      ;; Runner not force to trash since Imp is installed
      (is (= 2 (count (prompt-buttons :runner))) "Runner has 2 choices when Imp is installed")
      (is (= 5 (:credit (get-runner))) "Runner not forced to trash MVT when Imp installed")
      (is (empty? (:discard (get-corp))) "MVT is not force-trashed when Imp installed")
      (let [imp (get-program state 0)]
        (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
        (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed with Imp"))))
  (testing "interactions with Imp and various amounts of money"
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 3)]}
                 :runner {:deck ["Imp"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (is (= 3 (:credit (get-runner))) "Runner paid install costs")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 1")
      (is (= #{"[Imp] Hosted virus counter: Trash card" "Pay 5 [Credits] to trash"}
             (into #{} (prompt-buttons :runner))) "Should have Imp and MVT options")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 2")
      (is (= ["[Imp] Hosted virus counter: Trash card"] (prompt-buttons :runner)) "Should only have Imp option")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 3")
      (is (= ["No action"] (prompt-buttons :runner)) "Should only have no action option")
      (click-prompt state :runner "No action")
      (is (= 2 (->> (get-corp) :discard count)) "Runner was not forced to trash MVT")))
  (testing "not forced to trash when credits below 5"
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 3)]}
                 :runner {:deck ["Daily Casts"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Daily Casts")
      (is (= 2 (:credit (get-runner))) "Runner paid install costs")
      (run-empty-server state "Server 1")
      (is (= ["No action"] (prompt-buttons :runner)) "Runner is not given the choice")))
  (testing "forced to trash when playing as Khumalo"
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 3)]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Daily Casts"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Daily Casts")
      (is (= 2 (:credit (get-runner))) "Runner paid install costs")
      (run-empty-server state "Server 1")
      (is (= ["[Freedom Khumalo] Trash card"] (prompt-buttons :runner)) "Runner is not given the choice")))
  (testing "forced to trash after playing Demolition Run"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Mumbad Virtual Tour"]}
                 :runner {:hand ["Demolition Run"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Demolition Run")
      (is (= 3 (:credit (get-runner))) "Runner paid play costs")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (click-prompt state :runner "Unrezzed upgrade")
      (is (= ["[Demolition Run] Trash card"] (prompt-buttons :runner)) "Runner is not given the choice")))
  (testing "not to trash after installing Salsette Slums"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Mumbad Virtual Tour"]}
                 :runner {:hand ["Salsette Slums"]
                          :credits 10}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Salsette Slums")
      (is (= 8 (:credit (get-runner))) "Runner paid install costs")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Unrezzed upgrade")
      (is (= ["Pay 5 [Credits] to trash"] (prompt-buttons :runner)) "Runner is not given the choice"))))

(deftest mwanza-city-grid
  ;; Mwanza City Grid - runner accesses 3 additional cards, gain 2C for each card accessed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Mwanza City Grid")
      (is (= ["R&D" "HQ"] (prompt-buttons :corp)) "Mwanza can only be installed in root of HQ or R&D")
      (click-prompt state :corp "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (rez state :corp mcg)
        (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
        (run-continue state)
        (click-prompt state :runner "Mwanza City Grid")
        (click-prompt state :runner "No action")
        (dotimes [c 4]
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))
  (testing "effect persists through current run after trash"
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (rez state :corp mcg)
        (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
        (run-continue state)
        (click-prompt state :runner "Mwanza City Grid")
        (click-prompt state :runner "Pay 5 [Credits] to trash")
        (dotimes [c 4]
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))
  (testing "works well with replacement effects"
    ;; Regression test for #3456
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" "Hedge Fund"]}
                 :runner {:deck ["Embezzle"]}})
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (take-credits state :corp)
      (rez state :corp (get-content state :hq 0))
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (play-run-event state "Embezzle" :hq)
      (click-prompt state :runner "ICE")
      (is (zero? (count (:discard (get-corp)))) "No cards trashed from HQ")
      (is (not (:run @state)) "Run ended after Embezzle completed - no accesses from Mwanza")
      (is (= 7 (:credit (get-corp))) "Corp did not gain any money from Mwanza")))
  (testing "interaction with Kitsune"
    ;; Regression test for #3469
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" "Breached Dome"
                               (qty "Kitsune" 2) (qty "Hedge Fund" 3)]}})
      (core/draw state :corp 1) ; Draw last card of deck
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (play-from-hand state :corp "Kitsune" "HQ")
      (play-from-hand state :corp "Kitsune" "R&D")
      (take-credits state :corp)
      (let [mwanza (get-content state :hq 0)
            k-hq (get-ice state :hq 0)
            k-rd (get-ice state :rd 0)]
        (rez state :corp mwanza)
        (run-on state "HQ")
        (rez state :corp k-hq)
        (run-continue state)
        (card-subroutine state :corp k-hq 0)
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Breached Dome" (:hand (get-corp))))
        (is (= 2 (-> (get-runner) :hand count)) "Runner took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :runner "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :runner "No action"))
        (run-jack-out state)
        (run-on state "R&D")
        (rez state :corp k-rd)
        (run-continue state)
        (card-subroutine state :corp k-rd 0)
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Breached Dome" (:hand (get-corp))))
        (is (= 1 (-> (get-runner) :hand count)) "Runner took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :runner "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :runner "No action"))
        (run-jack-out state)
        (is (= 2 (-> (get-corp) :discard count)) "Two Kitsunes trashed after resolving their subroutines")))))

(deftest navi-mumbai-city-grid
  ;; Navi Mumbai City Grid
  (testing "Basic test"
    (do-game
      (new-game {:corp {:hand ["Navi Mumbai City Grid" "Fire Wall"]
                        :credits 7}
                 :runner {:hand ["D4v1d" "Corroder" "DDoS"]
                          :deck [(qty "Sure Gamble" 5)]
                          :credits 12}})
      (play-from-hand state :corp "Navi Mumbai City Grid" "HQ")
      (play-from-hand state :corp "Fire Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "D4v1d")
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "DDoS")
      (let [nmcg (get-content state :hq 0)
            fire (get-ice state :hq 0)
            d4 (get-program state 0)
            cor (get-program state 1)
            ddos (get-resource state 0)]
        (run-on state :hq)
        (rez state :corp nmcg)
        (rez state :corp fire)
        (run-continue state)
        (card-ability state :runner d4 0)
        (is (empty? (:prompt (get-runner))) "Could not use D4v1d")
        (card-ability state :runner ddos 0)
        (is (empty? (:discard (get-runner))) "DDoS not trashed")
        (changes-val-macro -4 (:credit (get-runner))
                           "Paid 3+1 to break Fire Wall with Corroder"
                           (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh cor)})))))
  (testing "Navi Mumbai City Grid blocks only runner abilities test"
    (do-game
      (new-game {:corp {:hand ["Navi Mumbai City Grid" "NGO Front"]
                        :credits 5}
                 :runner {:deck [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Navi Mumbai City Grid" "New remote")
      (play-from-hand state :corp "NGO Front" "Server 1")
      (let [navi (get-content state :remote1 0)
            ngo  (get-content state :remote1 1)]
        (core/add-counter state :corp ngo :advancement 2)
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp navi)
        (rez state :corp ngo)
        (changes-val-macro 5 (:credit (get-corp))
                           "+5 credits from NGO"
                           (card-ability state :corp (refresh ngo) 0))))))

(deftest neotokyo-grid
  ;; NeoTokyo Grid - Gain 1c the first time per turn a card in this server gets an advancement
  (do-game
    (new-game {:corp {:deck ["NeoTokyo Grid" "Nisei MK II"
                             "Shipment from SanSan" "Ice Wall"]}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "NeoTokyo Grid" "New remote")
    (play-from-hand state :corp "Nisei MK II" "Server 1")
    (rez state :corp (get-content state :remote1 0))
    (let [nis (get-content state :remote1 1)]
      (play-from-hand state :corp "Shipment from SanSan")
      (click-prompt state :corp "2")
      (click-card state :corp nis)
      (is (= 2 (get-counters (refresh nis) :advancement)) "2 advancements on agenda")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit")
      (core/advance state :corp {:card (refresh nis)})
      (is (= 3 (get-counters (refresh nis) :advancement)) "3 advancements on agenda")
      (is (= 3 (:credit (get-corp))) "No credit gained")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (core/advance state :corp {:card (refresh (get-ice state :remote1 0))})
      (is (= 2 (:credit (get-corp))) "No credit gained from advancing ICE"))))

(deftest nihongai-grid
  ;; Nihongai Grid
  (testing "Basic test. #5013"
    (do-game
      (new-game {:corp {:deck []
                        :hand ["Nihongai Grid" "Beanstalk Royalties"
                                "Accelerated Beta Test" "Brainstorm" "Chiyashi" "DNA Tracker" "Enigma" "Fire Wall"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Nihongai Grid" "R&D")
      (rez state :corp (get-content state :rd 0))
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (= ["Yes" "No"] (prompt-buttons :corp)) "Has Nihongai Grid prompt")
      (click-prompt state :corp "Yes")
      (is (= ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "DNA Tracker" "Enigma"]
             (map :title (prompt-buttons :corp)))
          "Corp sees top 5 cards")
      (click-prompt state :corp "Accelerated Beta Test")
      (click-card state :corp "Beanstalk Royalties")
      (click-prompt state :runner "Card from deck")
      (is (= "You accessed Beanstalk Royalties." (:msg (prompt-map :runner)))
          "Runner accesses switched card")
      (click-prompt state :runner "No action")
      (is (find-card "Accelerated Beta Test" (:hand (get-corp))))
      (is (find-card "Beanstalk Royalties" (:deck (get-corp))))
      (take-credits state :runner)
      (is (find-card "Beanstalk Royalties" (:hand (get-corp))))))
  (testing "Interaction with RNG Key. #5046"
    (do-game
      (new-game {:corp {:deck []
                        :hand ["Nihongai Grid" "Beanstalk Royalties"
                                "Accelerated Beta Test" "Brainstorm" "Chiyashi" "DNA Tracker" "Enigma" "Fire Wall"]}
                 :runner {:hand ["RNG Key"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Nihongai Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "RNG Key")
      (run-empty-server state "HQ")
      (is (= "Fire RNG Key?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= "Guess a number" (:msg (prompt-map :runner))))
      (click-prompt state :runner "3")
      (is (= "Look at the top 5 cards of R&D and swap one with a card from HQ?" (:msg (prompt-map :corp))))
      (click-prompt state :corp "Yes")
      (is (= "Choose a card in R&D" (:msg (prompt-map :corp))))
      (is (= ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "DNA Tracker" "Enigma"]
             (map :title (prompt-buttons :corp))))
      (click-prompt state :corp "Accelerated Beta Test")
      (is (= "Choose a card in HQ" (:msg (prompt-map :corp))))
      (click-card state :corp "Beanstalk Royalties")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Gain 3 [Credits]")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Nihongai Grid")
      (click-prompt state :runner "No action"))))

(deftest oberth-protocol
  ;; Oberth Protocol
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Oberth Protocol" "Oaktown Renovation"]}})
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Oberth Protocol" "Server 1")
    (play-from-hand state :corp "Oaktown Renovation" "Server 1")
    (take-credits state :corp)
    (take-credits state :runner)
    (let [oberth (get-content state :remote1 0)
          oak (get-content state :remote1 1) ]
      (rez state :corp (refresh oberth))
      (click-card state :corp (get-scored state :corp 0))
      (advance state oak)
      (is (= 2 (get-counters (refresh oak) :advancement)) "Oaktown should have 2 advancement tokens on it"))))

(deftest off-the-grid
  ;; Off the Grid run restriction - and interaction with RP
  (do-game
    (new-game {:corp {:id "Jinteki: Replicating Perfection"
                      :deck [(qty "Off the Grid" 3)
                             (qty "Mental Health Clinic" 3)]}})
    (play-from-hand state :corp "Off the Grid" "New remote")
    (play-from-hand state :corp "Mental Health Clinic" "Server 1")
    (let [otg (get-content state :remote1 0)]
      (take-credits state :corp)
      (rez state :corp (refresh otg))
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (run-empty-server state "R&D")
      (is (not (core/can-run-server? state "Server 1")) "Runner cannot run on Off the Grid")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Off the Grid prevention persisted")
      (run-empty-server state "HQ")
      (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on Server 1")
      (is (nil? (refresh otg)) "Off the Grid trashed"))))

(deftest old-hollywood-grid
  ;; Old Hollywood Grid
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [ohg (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (run-on state "Server 1")
        (rez state :corp ohg)
        (run-continue state)
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "No stolen agendas")
        (click-card state :runner ohg)
        (click-prompt state :runner "No action")
        (core/steal state :runner (make-eid state) (find-card "House of Knives" (:hand (get-corp))))
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Steal")
        (is (= 2 (count (:scored (get-runner)))) "2 stolen agendas"))))
  (testing "Central server"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Old Hollywood Grid" "HQ")
      (take-credits state :corp 2)
      (let [ohg (get-content state :hq 0)]
        (run-on state "HQ")
        (rez state :corp ohg)
        (run-continue state)
        ;; runner now chooses which to access.
        (is (= ["Card from hand" "Old Hollywood Grid"] (prompt-buttons :runner)))
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "No stolen agendas")
        (click-prompt state :runner "Old Hollywood Grid")
        (click-prompt state :runner "Pay 4 [Credits] to trash") ;; trash OHG
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))
  (testing "Gang Sign interaction. Prevent the steal outside of a run. #2169"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "Project Beale" 2)]}
                 :runner {:deck ["Gang Sign"]}})
      (play-from-hand state :corp "Old Hollywood Grid" "HQ")
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (rez state :corp (get-content state :hq 0))
      (score-agenda state :corp (get-content state :remote1 0))
      ;; Gang sign fires
      (is (= "You accessed Project Beale." (:msg (prompt-map :runner))))
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (zero? (count (:scored (get-runner)))) "No stolen agendas")))
  (testing "Trash order"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" "Project Beale"]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)
            pb (get-content state :remote1 1)]
        (run-on state "Server 1")
        (rez state :corp ohg)
        (run-continue state)
        (is (empty? (:scored (get-runner))) "Start with no stolen agendas")
        ;; runner now chooses which to access.
        (click-card state :runner (refresh ohg))
        (click-prompt state :runner "Pay 4 [Credits] to trash") ;; trash OHG
        (click-card state :runner (refresh pb))
        (click-prompt state :runner "No action")
        (is (empty? (:scored (get-runner))) "End with no stolen agendas")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))
  (testing "Steal other agendas"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "Project Beale" 2)]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)
            pb (get-content state :remote1 1)]
        (rez state :corp ohg)
        (run-empty-server state "Server 2")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))
  (testing "Stops protecting Archives after being trashed #4501"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Old Hollywood Grid"]
                        :discard ["Hostile Takeover" "Merger" "Vanity Project"]}
                 :runner {:hand ["Apocalypse"]}})
      (play-from-hand state :corp "Old Hollywood Grid" "Archives")
      (rez state :corp (get-content state :archives 0))
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (click-prompt state :runner "Hostile Takeover")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Merger")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Vanity Project")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Old Hollywood Grid")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Apocalypse")
      (core/gain state :runner :click 1)
      (run-empty-server state "Archives")
      (click-prompt state :runner "Hostile Takeover")
      (is (= ["Steal"] (prompt-buttons :runner)))
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Merger")
      (is (= ["Steal"] (prompt-buttons :runner)))
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Vanity Project")
      (is (= ["Steal"] (prompt-buttons :runner)))
      (click-prompt state :runner "Steal"))))

(deftest overseer-matrix
  ;; Overseer Matrix - corp takes a tag when trashing a card in this server
  (testing "Basic functionality"
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" "Red Herrings"]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (rez state :corp om)
        (run-continue state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 2 (count-tags state)) "Runner takes a tag"))))
  (testing "Effect persists after trash"
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" (qty "Red Herrings" 3)]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (rez state :corp om)
        (run-continue state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 2 (count-tags state)) "Runner takes a tag"))))
  (testing "Effect ends after current run"
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" (qty "Red Herrings" 3)]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (rez state :corp om)
        (run-continue state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-card state :runner rh)
        (click-prompt state :runner "No action")
        (is (= 1 (count-tags state)) "Runner doesn't take a tag")
        (run-on state "Server 1")
        (run-continue state)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (is (empty? (:prompt (get-corp))) "No prompt for Overseer Matrix")
        (is (= 1 (count-tags state)) "Runner doesn't take a tag"))))
  (testing "Takes into account apocalypse-like full-server trashes"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Overseer Matrix" "Bio Vault" "Red Herrings"]}
                 :runner {:hand ["Apocalypse"]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Bio Vault" "Server 1")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (rez state :corp (get-content state :remote1 0))
      (play-from-hand state :runner "Apocalypse")
      (is (= {:number 3 :default 0} (:choices (prompt-map :corp))))
      (click-prompt state :corp "3")
      (is (= 3 (count-tags state)) "Overseer Matrix gives the runner 3 tags")))
  (testing "Only works on Corp card trashes. Issue #4739"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Overseer Matrix" "PAD Campaign"]}
                 :runner {:hand ["Spy Camera"]
                          :credits 10}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Spy Camera")
      (run-on state :remote1)
      (changes-val-macro
        0 (count-tags state)
        "Runner should not gain a tag from trashing Spy Camera"
        (card-ability state :runner (get-hardware state 0) 1))
      (click-prompt state :runner "OK")
      (run-jack-out state)
      (changes-val-macro
        0 (count-tags state)
        "Runner should not gain a tag from trashing a card in another server"
        (run-empty-server state :remote2)
        (click-prompt state :runner "Pay 4 [Credits] to trash"))))
  (testing "Doesn't trigger on MaxX ability #5129"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Overseer Matrix" "PAD Campaign"]}
                 :runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Sure Gamble" 10)]
                          :hand ["Spy Camera"]
                          :credits 10}})
      (play-from-hand state :corp "Overseer Matrix" "R&D")
      (rez state :corp (get-content state :rd 0))
      (take-credits state :corp)
      (is (empty? (:prompt (get-corp)))))))

(deftest port-anson-grid
  ;; Port Anson Grid - Prevent the Runner from jacking out until they trash a program
  (do-game
    (new-game {:corp {:deck ["Port Anson Grid"]}
               :runner {:deck ["Faerie"]}})
    (play-from-hand state :corp "Port Anson Grid" "New remote")
    (take-credits state :corp)
    (let [pag (get-content state :remote1 0)]
      (run-on state "Server 1")
      (rez state :corp pag)
      (run-jack-out state)
      (is (get-run) "Can't jack out when no programs are installed")
      (run-continue state)
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Faerie")
      (run-on state "Server 1")
      (run-jack-out state)
      (click-card state :runner "Faerie")
      (is (find-card "Faerie" (:discard (get-runner))) "Faerie has been trashed")
      (is (not (get-run)) "Run has ended"))))

(deftest prisec
  ;; Prisec
  (testing "Basic test - Pay 2 credits to give runner 1 tag and do 1 meat damage, only when installed"
    (do-game
      (new-game {:corp {:deck [(qty "Prisec" 2)]}})
      (play-from-hand state :corp "Prisec" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (let [pre-creds (:credit (get-corp))]
        (click-prompt state :corp "Yes")
        (is (= (- pre-creds 2) (:credit (get-corp))) "Pay 2 [Credits] to pay for Prisec"))
      (is (= 1 (count-tags state)) "Give runner 1 tag")
      (is (= 1 (count (:discard (get-runner)))) "Prisec does 1 damage")
      ;; Runner trashes Prisec
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (run-empty-server state "HQ")
      (is (empty? (:prompt (get-corp))) "Prisec does not trigger from HQ")))
  (testing "Multiple unrezzed upgrades in Archives interaction with DRT"
    (do-game
      (new-game {:corp {:deck [(qty "Prisec" 2) "Dedicated Response Team"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (play-from-hand state :corp "Prisec" "Archives")
      (play-from-hand state :corp "Prisec" "Archives")
      (core/gain state :corp :click 1 :credit 14)
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :archives)
      (is (:run @state) "Run still active")
      (click-card state :runner (get-content state :archives 0))
      (click-prompt state :corp "Yes") ; corp pay for PriSec
      (click-prompt state :runner "No action") ; runner doesn't pay to trash
      (is (:run @state) "Run still active")
      (click-prompt state :corp "Yes") ; corp pay for PriSec
      (click-prompt state :runner "No action") ; runner doesn't pay to trash
      (is (not (:run @state)) "Run ended")
      (is (= 4 (count (:discard (get-runner)))) "Runner took 4 meat damage"))))

(deftest product-placement
  ;; Product Placement - Gain 2 credits when Runner accesses it
  (do-game
    (new-game {:corp {:deck ["Product Placement"]}})
    (play-from-hand state :corp "Product Placement" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (let [pp (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (is (= 9 (:credit (get-corp))) "Gained 2 credits from Runner accessing Product Placement")
      (click-prompt state :runner "Pay 2 [Credits] to trash") ; Runner trashes PP
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-corp)))
          "No credits gained when Product Placement accessed in Archives"))))

(deftest red-herrings
  ;; Red Herrings
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Red Herrings" "House of Knives"]}})
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [rh (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        ;; prompt should be asking for the 5cr cost
        (is (= "House of Knives" (:title (:card (prompt-map :runner))))
            "Prompt to pay 5cr")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner rh)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Cost increase even when trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Red Herrings" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [rh (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        ;; should now have prompt to pay 5cr for HoK
        (click-prompt state :runner "Pay to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Trashed from HQ"
    (do-game
      (new-game {:corp {:deck ["Red Herrings" "House of Knives"]}})
      (trash-from-hand state :corp "Red Herrings")
      (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      ;; prompt should be asking to steal HoK
      (is (= ["Steal"] (prompt-buttons :runner)) "Runner being asked to Steal")))
  (testing "Don't affect runs on other servers"
    (do-game
      (new-game {:corp {:deck ["Red Herrings" "House of Knives"]}})
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "New remote")
      (take-credits state :corp 1)
      (let [rh (get-content state :remote1 0)]
        (rez state :corp rh)
        (run-empty-server state "Server 2")
        ;; access is automatic
        (click-prompt state :runner "Steal")
        (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda")))))

(deftest reduced-service
  ;; Reduced Service - Increase run cost by 2x number of power counters
  (testing "Basic test"
    (do-game
     (new-game {:corp {:deck ["Reduced Service"]}
                :runner {:deck ["Dirty Laundry"]}})
     (play-from-hand state :corp "Reduced Service" "HQ")
     (take-credits state :corp)
     (let [rs (get-content state :hq 0)]
       (rez state :corp rs)
       (is (changes-credits (get-corp) -4
                            (click-prompt state :corp "4")))
       (is (= 4 (get-counters (refresh rs) :power)) "4 counters placed on Reduced Service")
       (play-from-hand state :runner "Dirty Laundry")
       (is (not (some #{"HQ"} (prompt-buttons :runner)))
           "Runner should not get to choose HQ due to increased cost")
       (click-prompt state :runner "Archives")
       (is (= 4 (get-counters (refresh rs) :power)) "No counter removed by only making a run")
       (run-continue state)
       (is (= 3 (get-counters (refresh rs) :power)) "1 counters removed from Reduced Service by successful run")
       (is (changes-credits (get-runner) -6
                            (run-on state :hq)))
       (run-continue state)
       (is (= 2 (get-counters (refresh rs) :power)) "1 counters removed from Reduced Service by successful run")
       (click-prompt state :runner "Pay 2 [Credits] to trash")
       (is (= 1 (count (:discard (get-corp)))) "Reduced Service trashed")
       (is (changes-credits (get-runner) 0
                            (run-on state :hq)))
       (is (:run @state) "Runner got to run without paying anything after trashing reduced service")))))

(deftest ruhr-valley
  ;; Ruhr Valley
  (testing "Basic test - As an additional cost to make a run on this server, the Runner must spend a click."
    (do-game
     (new-game {:corp {:deck ["Ruhr Valley"]}})
     (play-from-hand state :corp "Ruhr Valley" "HQ")
     (take-credits state :corp)
     (let [ruhr (get-content state :hq 0)]
       (rez state :corp ruhr)
       (is (= 4 (:click (get-runner))))
       (run-on state :hq)
       (run-jack-out state)
       (is (= 2 (:click (get-runner))))
       (take-credits state :runner 1)
       (is (= 1 (:click (get-runner))))
       (take-credits state :runner)
       (take-credits state :corp)
       (is (= 4 (:click (get-runner))))
       (is (= 7 (:credit (get-runner))))
       (run-on state :hq)
       (run-continue state)
       (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash / 7 cr - 4 cr
       (is (= 2 (:click (get-runner))))
       (is (= 3 (:credit (get-runner))))
       (run-on state :hq)
       (run-jack-out state)
       (is (= 1 (:click (get-runner)))))))
  (testing "If the runner trashes with one click left, the ability to run is enabled"
    (do-game
     (new-game {:corp {:deck ["Ruhr Valley"]}})
     (play-from-hand state :corp "Ruhr Valley" "HQ")
     (take-credits state :corp)
     (let [ruhr (get-content state :hq 0)]
       (rez state :corp ruhr)
       (is (= 4 (:click (get-runner))))
       (run-on state :rd)
       (run-jack-out state)
       (is (= 3 (:click (get-runner))))
       (run-on state :hq)
       (run-continue state)
       (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash / 6 cr - 4 cr
       (is (= 1 (:click (get-runner))))
       (run-on state :hq)
       (is (:run @state) "Runner got to run")))))

(deftest ryon-knight
  ;; Ryon Knight - Trash during run to do 1 brain damage if Runner has no clicks remaining
  (do-game
    (new-game {:corp {:deck ["Ryon Knight"]}})
    (play-from-hand state :corp "Ryon Knight" "HQ")
    (take-credits state :corp)
    (let [ryon (get-content state :hq 0)]
      (run-on state :hq)
      (rez state :corp ryon)
      (card-ability state :corp ryon 0)
      (is (= 3 (:click (get-runner))))
      (is (zero? (:brain-damage (get-runner))))
      (is (= 1 (count (get-content state :hq))) "Ryon ability didn't fire with 3 Runner clicks left")
      (run-jack-out state)
      (take-credits state :runner 2)
      (run-on state :hq)
      (card-ability state :corp ryon 0)
      (is (zero? (:click (get-runner))))
      (is (= 1 (:brain-damage (get-runner))) "Did 1 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "Ryon trashed"))))

(deftest sansan-city-grid
  ;; SanSan City Grid
  (testing "Basic test"
    (do-game
      (new-game {:corp {:hand ["Merger" "SanSan City Grid"]
                        :credits 10}})
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (play-from-hand state :corp "Merger" "Server 1")
      (is (= 3 (core/get-advancement-requirement (get-content state :remote1 1))))
      (rez state :corp (get-content state :remote1 0))
      (is (= 2 (core/get-advancement-requirement (get-content state :remote1 1)))))))

(deftest satellite-grid
  ;; Satellite Grid - Add 1 fake advancement on all ICE protecting server
  (do-game
    (new-game {:corp {:deck ["Satellite Grid" (qty "Ice Wall" 2)]}})
    (play-from-hand state :corp "Satellite Grid" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)
          sg (get-content state :hq 0)]
      (core/gain state :corp :click 1)
      (advance state iw1)
      (rez state :corp sg)
      (rez state :corp (refresh iw1))
      (is (= 1 (:extra-advance-counter (refresh iw1))) "1 fake advancement token")
      (is (= 1 (:advance-counter (refresh iw1))) "Only 1 real advancement token")
      (is (= 3 (get-strength (refresh iw1))) "Satellite Grid counter boosting strength by 1")
      (rez state :corp (refresh iw2))
      (is (= 1 (get-strength (refresh iw2))) "Satellite Grid not impacting ICE elsewhere")
      (derez state :corp sg)
      (is (= 2 (get-strength (refresh iw1))) "Ice Wall strength boost only from real advancement"))))

(deftest self-destruct
  ;; Self-destruct
  (do-game
    (new-game {:corp {:deck ["Self-destruct" "Dedicated Response Team" "Ice Wall"]}})
    (core/gain state :corp :credit 100 :click 4)
    (play-from-hand state :corp "Self-destruct" "New remote")
    (play-from-hand state :corp "Dedicated Response Team" "Server 1")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (let [self (get-content state :remote1 0)]
      (take-credits state :corp)
      (run-on state "Server 1")
      (rez state :corp self)
      (card-ability state :corp (refresh self) 0)
      (is (= 3 (-> (get-corp) :discard count)) "All 3 cards from Server 1 should be in discard")
      (is (= 2 (:base (prompt-map :corp))) "Self-destruct base trace should start at 2")
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in heap")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 3 (-> (get-runner) :discard count)) "Runner should take 3 net damage from losing Self-destruct trace")
      (is (not (:run @state)) "Run has ended because the server disappeared"))))

(deftest signal-jamming
  ;; Trash to stop installs for the rest of the run
  (do-game
    (new-game {:corp {:deck [(qty "Signal Jamming" 3)]}
               :runner {:deck [(qty "Self-modifying Code" 3) "Reaver"]}})
    (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
    (play-from-hand state :corp "Signal Jamming" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Self-modifying Code")
    (let [smc1 (get-program state 0)
          smc2 (get-program state 1)
          sj (get-content state :hq 0)]
      (rez state :corp sj)
      (run-on state "HQ")
      (card-ability state :corp sj 0)
      (card-ability state :runner smc1 0)
      (is (empty? (:prompt (get-runner))) "SJ blocking SMC")
      (run-jack-out state)
      (card-ability state :runner smc2 0)
      (click-prompt state :runner "Reaver"))))

(deftest simone-diego
  ;; Simone Diego
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:hand ["Simone Diego" "Ice Wall" "Project Junebug"]}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Simone Diego" "New remote")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (play-from-hand state :corp "Project Junebug" "Server 1")
      (let [sd (get-content state :remote1 0)
            pj (get-content state :remote1 1)
            iw (get-ice state :remote1 0)]
        (rez state :corp sd)
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 1 credit from Simone Diego to advance Ice Wall"
                           (core/advance state :corp {:card (refresh iw)})
                           (click-card state :corp sd))
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 1 credit from Simone Diego to advance Project Junebug"
                           (core/advance state :corp {:card (refresh pj)})
                           (click-card state :corp sd))))))

(deftest strongbox
  ;; Strongbox
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Strongbox" "House of Knives"]}})
      (play-from-hand state :corp "Strongbox" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [sb (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (rez state :corp sb)
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (is (= "House of Knives" (:title (:card (prompt-map :runner))))
            "Prompt to pay 5cr")
        (click-prompt state :runner "No action")
        (is (= 3 (:click (get-runner))) "Runner was not charged 1click")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner sb)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (= 1 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Click cost even when trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Strongbox" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Strongbox" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [sb (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (rez state :corp sb)
        (run-empty-server state "Server 1")
        (click-card state :runner sb)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda")))))

(deftest surat-city-grid
  ;; Surat City Grid - Trigger on rez of a card in/protecting same server to rez another card at 2c discount
  (do-game
    (new-game {:corp {:deck [(qty "Surat City Grid" 2) (qty "Cyberdex Virus Suite" 2)
                             "Enigma" "Wraparound"]}})
    (core/gain state :corp :credit 15 :click 8)
    (play-from-hand state :corp "Surat City Grid" "New remote")
    (play-from-hand state :corp "Wraparound" "Server 1")
    (play-from-hand state :corp "Cyberdex Virus Suite" "Server 1")
    (let [scg1 (get-content state :remote1 0)
          cvs1 (get-content state :remote1 1)
          wrap (get-ice state :remote1 0)]
      (rez state :corp scg1)
      (rez state :corp cvs1)
      (is (= 15 (:credit (get-corp))))
      (is (= (:cid scg1) (-> (prompt-map :corp) :card :cid)) "Surat City Grid triggered from upgrade in same remote")
      (click-prompt state :corp "Yes")
      (click-card state :corp wrap)
      (is (rezzed? (refresh wrap)) "Wraparound is rezzed")
      (is (= 15 (:credit (get-corp))) "Wraparound rezzed for free with 2c discount from SCG")
      (play-from-hand state :corp "Surat City Grid" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
      (let [scg2 (get-content state :hq 0)
            cvs2 (get-content state :hq 1)
            enig (get-ice state :hq 0)]
        (rez state :corp scg2)
        (rez state :corp cvs2)
        (is (empty? (:prompt (get-corp))) "SCG didn't trigger, upgrades in root of same central aren't considered in server")
        (derez state :corp (refresh wrap))
        (rez state :corp enig)
        (is (= (:cid scg2) (-> (prompt-map :corp) :card :cid)) "SCG did trigger for ICE protecting HQ")))))

(deftest tranquility-home-grid
  ;; Tranquility Home Grid
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 5)]
                        :hand ["Tranquility Home Grid" "PAD Campaign"]}})
      (play-from-hand state :corp "Tranquility Home Grid" "New remote")
      (let [thg (get-content state :remote1 0)]
        (rez state :corp thg)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (is (empty? (:prompt (get-corp))) "THG didn't trigger on the PAD Campaign install, because its own install was the first install this turn")
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (click-prompt state :corp "OK") ; Trash existing PAD Campaign
        (changes-val-macro 2 (:credit (get-corp))
                           "Gained 2 credits from THG"
                           (click-prompt state :corp "Gain 2 [Credits]"))
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (click-prompt state :corp "OK") ; Trash existing PAD Campaign
        (changes-val-macro 1 (count (:hand (get-corp)))
                           "Drew 1 card from THG"
                           (click-prompt state :corp "Draw 1 card")))))
  (testing "Not installable on centrals"
    (do-game
      (new-game {:corp {:hand ["Tranquility Home Grid"]}})
      (play-from-hand state :corp "Tranquility Home Grid")
      (is (= ["New remote"] (prompt-buttons :corp)) "Only installable in a remote server")))
  (testing "Restore interaction"
    (do-game
      (new-game {:corp {:hand ["Tranquility Home Grid" "Restore"]}})
      (core/move state :corp (find-card "Tranquility Home Grid" (:hand (get-corp))) :discard)
      (play-from-hand state :corp "Restore")
      (click-card state :corp (find-card "Tranquility Home Grid" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (empty? (:prompt (get-corp))) "No prompt from THG on its own install, because it was inactive at the point of install triggers")))
  (testing "THG interaction with ICE install"
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 5)]
                        :hand ["Tranquility Home Grid" "PAD Campaign" "Ice Wall"]}})
      (play-from-hand state :corp "Tranquility Home Grid" "New remote")
      (let [thg (get-content state :remote1 0)]
        (rez state :corp thg)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (is (empty? (:prompt (get-corp))) "THG didn't trigger on the PAD Campaign install, because its own install was the first install this turn")
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "Ice Wall" "Server 1")
        (is (empty? (:prompt (get-corp))) "No prompt from Ice Wall install")
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (click-prompt state :corp "OK") ; Trash existing PAD Campaign
        (changes-val-macro 2 (:credit (get-corp))
                           "Gained 2 credits from THG"
                           (click-prompt state :corp "Gain 2 [Credits]")))))
  (testing "Interaction with PAD Tap. Issue #4835"
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 5)]
                        :hand ["Tranquility Home Grid" "PAD Campaign"]}
                 :runner {:hand ["PAD Tap"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "PAD Tap")
      (take-credits state :runner)
      (play-from-hand state :corp "Tranquility Home Grid" "New remote")
      (let [thg (get-content state :remote1 0)]
        (rez state :corp thg)
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (changes-val-macro
          1 (:credit (get-runner))
          "Runner gained 1 credit from PAD Tap"
          (click-prompt state :corp "Gain 2 [Credits]"))))))

(deftest tempus
  ;; Tempus - Trace^3, the runner chooses to lose 2 clicks or take 1 brain damage
  (do-game
    (new-game {:corp {:deck [(qty "Tempus" 3)]}
               :runner {:deck [(qty "Sure Gamble" 3)]}})
    (starting-hand state :corp ["Tempus"])
    (play-from-hand state :corp "Tempus" "New remote")
    (take-credits state :corp)
    (run-on state "R&D")
    (run-continue state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
    (click-prompt state :runner "Lose [Click][Click]")
    (is (= 1 (:click (get-runner))) "Runner loses 2 clicks")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (run-on state "Server 1")
    (run-continue state)
    (click-prompt state :corp "0") ; trace
    (is (zero? (:brain-damage (get-runner))) "Runner starts with 0 brain damage")
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (take-credits state :runner)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (is (= 1 (:brain-damage (get-runner))) "Runner takes no brain damage")
    (is (= 3 (:click (get-runner))) "Runner loses no clicks")
    (run-on state "HQ")
    (run-continue state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner starts with 1 brain damage")
    (click-prompt state :runner "Take 1 brain damage")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 1 brain damage")
    (click-prompt state :runner "No action") ; don't trash
    (run-on state "HQ")
    (run-continue state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "4")
    (click-prompt state :runner "Pay 0 [Credits] to trash")))

(deftest the-twins
  ;; The Twins
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["The Twins" (qty "Quicksand" 2)]}
               :runner {:deck ["Corroder"]}})
    (play-from-hand state :corp "The Twins" "New remote")
    (play-from-hand state :corp "Quicksand" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [twins (get-content state :remote1 0)
          quicksand (get-ice state :remote1 0)
          cor (get-program state 0)]
      (rez state :corp twins)
      (run-on state "Server 1")
      (rez state :corp quicksand)
      (run-continue state)
      (card-ability state :runner cor 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Quicksand" (:hand (get-corp))))
      (is (= 1 (:position (get-run))) "Run should be moved back to position 1")
      (is (utils/same-card? quicksand (core/get-current-ice state)))
      (is (= 2 (get-counters (get-ice state :remote1 0) :power))
          "Encounter abilities resolve a second time"))))

(deftest tori-hanzo
  ;; Tori Hanzō - Pay to do 1 brain damage instead of net damage
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Pup" "Tori Hanzō"]}
                 :runner {:deck [(qty "Sure Gamble" 3) "Net Shield"]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Pup" "HQ")
      (play-from-hand state :corp "Tori Hanzō" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Net Shield")
      (run-on state "HQ")
      (let [pup (get-ice state :hq 0)
            tori (get-content state :hq 0)
            nshld (get-program state 0)]
        (rez state :corp pup)
        (rez state :corp tori)
        (run-continue state)
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Suffer 1 net damage")
        (card-ability state :runner nshld 0)
        (is (empty? (:discard (get-runner))) "1 net damage prevented")
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Suffer 1 net damage")
        (click-prompt state :runner "Done") ; decline to prevent
        (is (= 1 (count (:discard (get-runner)))) "1 net damage; previous prevention stopped Tori ability")
        (run-jack-out state)
        (run-on state "HQ")
        (run-continue state)
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Suffer 1 net damage")
        (click-prompt state :runner "Done")
        (click-prompt state :corp "Yes")
        (is (= 2 (count (:discard (get-runner)))) "1 brain damage suffered")
        (is (= 1 (:brain-damage (get-runner)))))))
  (testing "with Hokusai Grid: Issue #2702"
    (do-game
      (new-game {:corp {:deck ["Tori Hanzō" "Hokusai Grid"]}})
      (core/gain state :corp :credit 5)
      (play-from-hand state :corp "Hokusai Grid" "Archives")
      (play-from-hand state :corp "Tori Hanzō" "Archives")
      (take-credits state :corp)
      (run-on state "Archives")
      (let [hg (get-content state :archives 0)
            tori (get-content state :archives 1)]
        (rez state :corp hg)
        (rez state :corp tori)
        (run-continue state)
        (click-prompt state :corp "No") ; Tori prompt to pay 2c to replace 1 net with 1 brain
        (is (= 1 (count (:discard (get-runner)))) "1 net damage suffered")
        (click-prompt state :runner "Hokusai Grid")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Tori Hanzō")
        (click-prompt state :runner "No action")
        (is (and (empty (:prompt (get-runner))) (not (:run @state))) "No prompts, run ended")
        (run-empty-server state "Archives")
        (click-prompt state :corp "Yes") ; Tori prompt to pay 2c to replace 1 net with 1 brain
        (is (= 2 (count (:discard (get-runner)))))
        (is (= 1 (:brain-damage (get-runner))) "1 brain damage suffered")
        (click-prompt state :runner "Hokusai Grid")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Tori Hanzō")
        (click-prompt state :runner "No action")
        (is (and (empty (:prompt (get-runner))) (not (:run @state))) "No prompts, run ended"))))
  (testing "breaking subsequent net damage: Issue #3176"
    (do-game
      (new-game {:corp {:deck ["Tori Hanzō" (qty "Pup" 2) (qty "Neural EMP" 2)]}})
      (core/gain state :corp :credit 8)
      (play-from-hand state :corp "Tori Hanzō" "New remote")
      (play-from-hand state :corp "Pup" "Server 1")
      (take-credits state :corp)
      (run-on state "Server 1")
      (let [tori (get-content state :remote1 0)
            pup (get-ice state :remote1 0)]
        (rez state :corp pup)
        (rez state :corp tori)
        (run-continue state)
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Suffer 1 net damage")
        (click-prompt state :corp "Yes") ; pay 2c to replace 1 net with 1 brain
        (is (= 1 (count (:discard (get-runner)))) "1 brain damage suffered")
        (is (= 1 (:brain-damage (get-runner))))
        (run-jack-out state)
        (take-credits state :runner)
        (play-from-hand state :corp "Neural EMP")
        (is (= 2 (count (:discard (get-runner)))) "Net damage processed correctly")))))

(deftest underway-grid
  ;; Underway Grid - prevent expose of cards in server
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"
                             "Underway Grid"]}
               :runner {:deck ["Drive By"]}})
    (play-from-hand state :corp "Underway Grid" "New remote")
    (play-from-hand state :corp "Eve Campaign" "Server 1")
    (take-credits state :corp)
    (rez state :corp (get-content state :remote1 0))
    (let [eve1 (get-content state :remote1 1)]
      (play-from-hand state :runner "Drive By")
      (click-card state :runner eve1)
      (is (empty? (:discard (get-corp))) "Expose and trash prevented"))))

(deftest valley-grid
  ;; Valley Grid
  (testing "Reduce Runner max hand size and restore it even if trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Valley Grid" 3) (qty "Ice Wall" 3)]
                        :credits 10}
                 :runner {:hand ["Corroder"]
                          :credits 10}})
      (play-from-hand state :corp "Valley Grid" "New remote")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "Server 1")
      (let [vg (get-content state :remote1 0)
            iw (get-ice state :remote1 0)
            cor (get-program state 0)]
        (rez state :corp vg)
        (rez state :corp iw)
        (run-continue state)
        (card-ability state :runner (refresh cor) 0)
        (click-prompt state :runner "End the run")
        (is (= 4 (hand-size :runner)) "Runner max hand size reduced by 1")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; pay to trash
        (is (= 4 (hand-size :runner)) "Valley Grids effect persists through trash")
        (take-credits state :runner)
        (is (= 5 (hand-size :runner)) "Runner max hand size back to normal")))))

(deftest warroid-tracker
  ;; Warroid Tracker
  (testing "Trashing Warroid starts trace"
    (do-game
      (new-game {:corp {:deck ["Warroid Tracker"]}
                 :runner {:deck ["Corroder" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Dyson Mem Chip")
      (let [war (get-content state :remote1 0)
            cor (get-program state 0)
            mem (get-hardware state 0)]
        (rez state :corp war)
        (run-on state "Server 1")
        (run-continue state)
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
        (click-card state :runner cor)
        (click-card state :runner mem)
        (is (= 2 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards"))))
  (testing "Trashing from central triggers Warroid in root. Issue #3725"
    (do-game
      (new-game {:corp {:deck ["Warroid Tracker" (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Clan Vengeance" "Corroder" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Warroid Tracker" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Dyson Mem Chip")
      (play-from-hand state :runner "Clan Vengeance")
      (let [war (get-content state :hq 0)
            clv (get-resource state 0)
            cor (get-program state 0)
            mem (get-hardware state 0)]
        (rez state :corp war)
        (core/add-counter state :runner clv :power 2)
        (card-ability state :runner (refresh clv) 0)
        ;; Prompt choice checks there is a trace prompt from Warroid
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (-> (get-runner) :discard count)) "Runner should start with 1 card in heap (Clan Vengeance)")
        (click-card state :runner cor)
        (click-card state :runner mem)
        (is (= 3 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards (and CV already in heap)")
        (is (= 2 (count (:discard (get-corp)))) "Two cards trashed from HQ by Clan Vengeance"))))
  (testing "Doesn't trigger from Maxx. Issue #4329"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Warroid Tracker"]}
                 :runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Sure Gamble" 10)]
                          :hand ["Corroder" "Dyson Mem Chip"]}})
      (play-from-hand state :corp "Warroid Tracker" "HQ")
      (let [war (get-content state :hq 0)]
        (rez state :corp war)
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Dyson Mem Chip")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (count (:hand (get-runner)))) "MaxX draws 2 cards")
        (is (empty? (:prompt (get-corp))) "Corp has no prompt")
        (is (empty? (:prompt (get-runner))) "Runner has no prompt"))))
  (testing "Interactions with IG. Issue #4329"
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck [(qty "Hedge Fund" 3)]
                        :hand ["Warroid Tracker" "Launch Campaign" (qty "Hedge Fund" 2)]
                        :credits 100}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Corroder" "Dyson Mem Chip"]
                          :credits 100}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (play-from-hand state :corp "Launch Campaign" "Remote 1")
      (let [war (get-content state :remote1 0)]
        (rez state :corp war)
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Dyson Mem Chip")
        (run-on state "Server 1")
        (run-continue state)
        (click-card state :runner "Launch Campaign")
        (is (zero? (count (:discard (get-runner)))) "Before trashing anything")
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (click-card state :runner "Corroder")
        (click-card state :runner "Dyson Mem Chip")
        (click-prompt state :runner "Done")
        (is (= 2 (count (:discard (get-runner)))) "Runner trashes 2 cards to Warriod Tracker"))))
  (testing "Trashing Warroid starts trace"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Warroid Tracker" "PAD Campaign"]
                        :credits 15}
                 :runner {:hand ["Singularity" (qty "Self-modifying Code" 5)]
                          :credits 15}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (play-from-hand state :corp "PAD Campaign" "Remote 1")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (dotimes [_ 5]
        (play-from-hand state :runner "Self-modifying Code"))
      (rez state :corp (get-content state :remote1 0))
      (play-from-hand state :runner "Singularity")
      (click-prompt state :runner "Server 1")
      (run-continue state)
      (is (= 2 (-> (get-corp) :discard count)) "Corp has both cards in discard")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0") ; Corp wins trace
      (click-card state :runner (get-program state 0))
      (click-card state :runner (get-program state 1))
      (is (empty? (:prompt (get-corp))) "Warroid Tracker can't trash anything else")
      (is (= 3 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards")))
  (testing "Shouldn't trigger from self-trash in root of central server. Issue #4813"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Warroid Tracker"]}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Corroder" "Dyson Mem Chip"]
                          :credits 20}})
      (play-from-hand state :corp "Warroid Tracker" "HQ")
      (let [war (get-content state :hq 0)]
        (rez state :corp war)
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Dyson Mem Chip")
        (run-empty-server state :hq)
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (is (empty? (:prompt (get-corp))) "Corp has no prompt")
        (is (empty? (:prompt (get-runner))) "Runner has no prompt"))))
  (testing "Shouldn't trigger when trashed card is in root of central server."
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Warroid Tracker" "Crisium Grid"]
                        :credits 20}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Corroder" "Dyson Mem Chip"]
                          :credits 20}})
      (play-from-hand state :corp "Warroid Tracker" "HQ")
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (let [war (get-content state :hq 0)
            cg (get-content state :hq 1)]
        (rez state :corp war)
        (rez state :corp cg)
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Dyson Mem Chip")
        (run-empty-server state :hq)
        (is (= ["Warroid Tracker" "Crisium Grid"] (prompt-buttons :runner)))
        (click-prompt state :runner "Crisium Grid")
        (click-prompt state :runner "Pay 5 [Credits] to trash")
        (click-prompt state :runner "Warroid Tracker")
        (click-prompt state :runner "No action")
        (is (empty? (:prompt (get-corp))) "Corp has no prompt")
        (is (empty? (:prompt (get-runner))) "Runner has no prompt"))))
  (testing "Shouldn't trigger when trashed by corp (via Hellion Beta Test). Issue #4941"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Warroid Tracker" (qty "Marilyn Campaign" 3) "Hellion Beta Test"]
                        :credits 20}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Corroder" "Dyson Mem Chip"]
                          :credits 20}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (play-from-hand state :corp "Marilyn Campaign" "Remote 1")
      (play-from-hand state :corp "Marilyn Campaign" "New remote")
      (take-credits state :corp)
      (let [war (get-content state :remote1 0)
            mar1 (get-content state :remote1 1)]
        (rez state :corp war)
        (run-empty-server state :remote2)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (take-credits state :runner)
        (play-from-hand state :corp "Hellion Beta Test")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (click-card state :corp mar1)
        (click-card state :corp war)
        (is (empty? (:prompt (get-corp))) "Corp has no prompt")))))

(deftest will-o-the-wisp
  ;; Will-o'-the-Wisp
  (testing "Basic test"
    (do-game
     (new-game {:corp {:deck ["Will-o'-the-Wisp"
                              "Vanilla"]}
                :runner {:deck ["Corroder"]}})
     (play-from-hand state :corp "Will-o'-the-Wisp" "New remote")
     (rez state :corp (get-content state :remote1 0))
     (play-from-hand state :corp "Vanilla" "Server 1")
     (rez state :corp (get-ice state :remote1 0))
     (take-credits state :corp)
     (play-from-hand state :runner "Corroder")
     (let [cor (get-program state 0)]
       (run-on state "Server 1")
       (run-continue state)
       (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh cor)})
       (core/continue state :corp nil)
       (run-continue state)
       (click-prompt state :corp "Yes")
       (click-card state :corp (refresh cor))
       (is (empty? (get-program state)) "Corroder uninstalled")
       (is (= "Corroder" (:title (last (:deck (get-runner))))) "GoCorroderrdian on bottom of Stack")))))

(deftest oaktown-grid
  (do-game
    (new-game {:corp {:hand ["Oaktown Grid" "PAD Campaign"]}
               :runner {:credits 15}})
    (play-from-hand state :corp "Oaktown Grid" "New remote")
    (play-from-hand state :corp "PAD Campaign" "Remote 1")
    (let [og (get-content state :remote1 0)
          pad (get-content state :remote1 1)]
      (rez state :corp og)
      (rez state :corp pad)
    (take-credits state :corp)
    (run-on state "Server 1")
    (run-continue state)
    (click-card state :runner pad)
    (click-prompt state :runner "Pay 7 [Credits] to trash")
    (is (= "PAD Campaign" (:title (first (:discard (get-corp))))) "PAD Campaign trashed for 7c (3 more than normal cost thanks to Oaktown Grid"))))

(deftest rutherford-grid
  (do-game
    (new-game {:corp {:hand ["Rutherford Grid" "Caduceus"]}})
    (play-from-hand state :corp "Rutherford Grid" "New remote")
    (play-from-hand state :corp "Caduceus" "Remote 1")
    (let [rg (get-content state :remote1 0)
          caduceus (get-ice state :remote1 0)]
      (rez state :corp rg)
      (rez state :corp caduceus)
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-continue state)
      (fire-subs state caduceus)
      (click-prompt state :corp "3")                        ;;Boost trace by 3
      (is (= 8 (:strength (get-prompt state :runner))) "3 base, +2 of upgrade +3 boost"))))

(deftest akitaro-watanabe
  (do-game
    (new-game {:corp {:hand ["Akitaro Watanabe" "Ice Wall" "Enigma"]}})
    (play-from-hand state :corp "Akitaro Watanabe" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "HQ")              ;;-1c
    (let [akitaro (get-content state :hq 0)
          iw (get-ice state :hq 0)
          enigma (get-ice state :hq 1)]
      (rez state :corp akitaro)
      (rez state :corp iw)
      (rez state :corp enigma)
      (is (= 2 (:credit (get-corp))) "2c left after installing and rezzing with AW bonus"))))

(deftest research-station
  (do-game
    (new-game {:corp {:id "NBN: The World is Yours*"
                      :hand [(qty "Research Station" 9)] :deck ["Research Station"]}})
    (play-from-hand state :corp "Research Station" "HQ")
    (rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (is (= 8 (count (:hand (get-corp)))) "+3 cards, 2 of Research Station and +1 of ID")
    (is (= nil (get-prompt state :corp)) "No prompt asking to discard cards from hand")
    (take-credits state :runner)
    (is (= 9 (count (:hand (get-corp)))) "Double check that you start next turn with 8 cards")))

(deftest traffic-analyzer
  (do-game
    (new-game {:corp   {:hand ["Traffic Analyzer" "Ice Wall"]}
               :runner {:tag 2 :hand [(qty "Corroder" 2)]}})
    (play-from-hand state :corp "Traffic Analyzer" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-content state :hq 0))
    (rez state :corp (get-ice state :hq 0))
    (click-prompt state :corp "4")
    (click-prompt state :runner "0")
    (is (= 1 (:credit (get-corp))) "After using all credits to boost trace, Corp gains 1c thanks to Traffic Analyzer")
    ))

(deftest defense-construct
  (do-game
    (new-game {:corp {:hand ["Defense Construct" (qty "PAD Campaign" 3)]}})
    ;; Play 3 PAD Campaigns on the same server so 2 of them are trashed facedown
    (play-from-hand state :corp "Defense Construct" "Archives")
    (play-from-hand state :corp "PAD Campaign" "HQ")
    (play-from-hand state :corp "PAD Campaign" "HQ")
    (click-prompt state :corp "OK")
    (take-credits state :runner)
    (play-from-hand state :corp "PAD Campaign" "HQ")
    (click-prompt state :corp "OK")
    ;; Advance Defense Construct twice to recover both PAD Campaigns from discard
    (let [dc (get-content state :archives 0)
          advance-tokens 2]
      (rez state :corp dc)
      (advance state dc advance-tokens)
      (take-credits state :corp)
      (run-on state :archives)
      (card-ability state :corp dc 0)
      (click-card state :corp (get (:discard (get-corp)) 0))
      (click-card state :corp (get (:discard (get-corp)) 1))
      (is (= advance-tokens (count (filter #(= (:title %) "PAD Campaign") (:hand (get-corp))))) "2 advance tokens of DC so 2 PAD Campaign back to corp")
      (is (= "Defense Construct" (:title (first (:discard (get-corp)))))))) "Defense Construct is trashed after using it")

(deftest k-p-lynn
  (do-game
    (new-game {:corp {:hand ["K. P. Lynn"]}})
    (play-from-hand state :corp "K. P. Lynn" "New remote")
    ))

(deftest fractal-threat-matrix
  (do-game
    (new-game {:corp {:hand ["Fractal Threat Matrix" "Najja 1.0"]}
               :runner {:deck [(qty "Acacia" 7)]}})
    (play-from-hand state :corp "Fractal Threat Matrix" "New remote")
    (play-from-hand state :corp "Najja 1.0" "Remote 1")
    (take-credits state :corp)
    (is (= 2 (count (:deck (get-runner)))))
    (run-on state :remote1)
    (let [najja (get-ice state :remote1 0)
          ftm (get-content state :remote1 0)]
      (rez state :corp najja)
      (rez state :corp ftm)
      (run-continue state)
      (card-side-ability state :runner najja 0)
      (click-prompt state :runner "End the run")
      (card-side-ability state :runner najja 0)
      (click-prompt state :runner "End the run")
      ;; All subroutines broken. Manually trigger Fractal Thread Matrix
      (card-ability state :corp ftm 0)
      (is (zero? (count (:deck (get-runner)))))
      (is (= 2 (count (:discard (get-runner)))))
      )))
>>>>>>> 6937396a1 (Add tests to fractal-threat-matrix)
