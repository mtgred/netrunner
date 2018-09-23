(ns game-test.cards.upgrades
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [jinteki.utils :refer [count-tags]]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "upgrades"))

(deftest amazon-industrial-zone
  ;; Amazon Industrial Zone - Immediately rez ICE installed over its server at 3 credit discount
  (do-game
    (new-game (default-corp ["Spiderweb" "Amazon Industrial Zone"])
              (default-runner))
    (take-credits state :corp 1)
    (play-from-hand state :corp "Amazon Industrial Zone" "New remote")
    (let [aiz (get-content state :remote1 0)]
      (core/rez state :corp aiz)
      (is (= 2 (:credit (get-corp))))
      (play-from-hand state :corp "Spiderweb" "Server 1")
      (click-prompt state :corp "Yes") ; optional ability
      (let [spid (get-ice state :remote1 0)]
        (is (:rezzed (refresh spid)) "Spiderweb rezzed")
        (is (= 1 (:credit (get-corp))) "Paid only 1 credit to rez")))))

(deftest arella-salvatore
  ;; Arella Salvatore - when an agenda is scored from this server, install a card from hq w/ advancement token
  (testing "Install to server"
    (do-game
      (new-game (default-corp ["Arella Salvatore" "Bryan Stinson" (qty "TGTBT" 2)])
                (default-runner))
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "TGTBT" "Server 1")
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [arella (get-content state :remote1 0)
            same-tg (get-content state :remote1 1)
            diff-tg (get-content state :remote2 0)]
        (core/rez state :corp arella)
        (score-agenda state :corp (refresh diff-tg))
        (is (empty? (get-in @state [:corp :prompt])) "Arella not triggered for different remote score")
        (is (= 1 (count (get-scored state :corp))) "1 Agenda scored")
        (score-agenda state :corp (refresh same-tg))
        (click-card state :corp (find-card "Bryan Stinson" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= 2 (count (get-scored state :corp))) "2 Agendas scored")
        (is (= 1 (count (get-content state :remote3))) "Bryan installed in new remote")
        (is (= 1 (get-counters (get-content state :remote3 0) :advancement)) "Bryan has 1 advancement counter"))))
  (testing "Interaction w/ other on-scored triggers"
    (do-game
      (new-game (make-deck "Sportsmetal: Go Big or Go Home" ["Arella Salvatore" "Domestic Sleepers" "Project Vitruvius" "Hedge Fund"])
                (default-runner))
      (starting-hand state :corp ["Arella Salvatore" "Domestic Sleepers"])
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "Server 1")
      (let [arella (get-content state :remote1 0)
            domest (get-content state :remote1 1)]
        (core/rez state :corp arella)
        (score-agenda state :corp (refresh domest))
        ;; Simultaneous prompt: Sportsmetal automatically triggers, as Arella is silent because there are no installable cards in HQ
        (click-prompt state :corp "2 cards")
        ;; Arella is no longer silent and now triggers
        (click-card state :corp (find-card "Project Vitruvius" (:hand (get-corp))))
        (click-prompt state :corp "Server 1")
        (is (= 2 (count (get-content state :remote1))) "Agenda installed in server 1")
        (is (= 1 (get-counters (get-content state :remote1 1) :advancement)) "Agenda has 1 advancement counter"))))
  (testing "No cost"
    (do-game
      (new-game (default-corp ["Arella Salvatore" "TGTBT" (qty "Ice Wall" 2)])
                (default-runner))
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "TGTBT" "Server 1")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 1 (count (get-ice state :hq))) "One ice on hq")
      (let [arella (get-content state :remote1 0)
            tg (get-content state :remote1 1)]
        (core/rez state :corp arella)
        (score-agenda state :corp (refresh tg))
        (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 2 (count (get-ice state :hq))) "Two ice on hq")
        (is (= 1 (get-counters (get-ice state :hq 1) :advancement)) "Ice Wall has 1 counter")))))

(deftest ash-2x3zb9cy
  ;; Ash 2X3ZB9CY
  (do-game
    (new-game (default-corp ["Ash 2X3ZB9CY" (qty "Ice Wall" 10)])
              (default-runner))
    (starting-hand state :corp ["Ash 2X3ZB9CY" "Ice Wall"])
    (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
    (take-credits state :corp)
    (let [ash (get-content state :hq 0)]
      (core/rez state :corp ash)
      (run-empty-server state "HQ")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= "Ash 2X3ZB9CY" (-> (get-runner) :prompt first :card :title)) "Should access Ash")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (not (:run @state)) "Accessing Ash then ends the run"))))

(deftest awakening-center
  ;; Awakening Center
  (testing "Basic Operation"
    (do-game
      (new-game (default-corp ["Awakening Center" "Fairchild"])
                (default-runner))
      (play-from-hand state :corp "Awakening Center" "New remote")
      (let [ac (get-content state :remote1 0)]
        (core/rez state :corp ac)
        (card-ability state :corp (refresh ac) 0)
        (click-card state :corp (find-card "Fairchild" (:hand (get-corp))))
        (let [fc (first (:hosted (refresh ac)))]
          (is (= "Fairchild" (:title (refresh fc))) "Fairchild hosted on Awakening Center")
          (is (not (:rezzed (refresh fc))) "Fairchild is not rezzed")
          (is (empty? (:hand (get-corp))) "Fairchild removed from hand")
          (take-credits state :corp)
          (run-empty-server state "Server 1")
          (card-ability state :corp (refresh ac) 1)
          (click-prompt state :corp "Fairchild")
          (is (:rezzed (refresh fc)) "Fairchild is rezzed")
          (click-prompt state :runner "Done")
          (is (not (:run @state)) "Run has ended")
          (is (= 1 (count (:discard (get-corp)))) "Fairchild in discard")
          (is (empty? (:hosted (refresh ac))) "Fairchild no longer hosted")))))
  (testing "DDoS Interaction"
    (do-game
      (new-game (default-corp ["Awakening Center" "Fairchild"])
                (default-runner ["DDoS"]))
      (play-from-hand state :corp "Awakening Center" "New remote")
      (let [ac (get-content state :remote1 0)]
        (core/rez state :corp ac)
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
          (is (:rezzed (refresh fc)) "Fairchild is rezzed")
          (click-prompt state :runner "Done")
          (is (not (:run @state)) "Run has ended")
          (is (= 1 (count (:discard (get-corp)))) "Fairchild in discard")
          (is (empty? (:hosted (refresh ac))) "Fairchild no longer hosted"))))))

(deftest ben-musashi
  ;; Ben Musashi
  (testing "Basic test - pay 2 net damage to steal from this server"
    (do-game
      (new-game (default-corp ["Ben Musashi" "House of Knives"])
                (default-runner))
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [bm (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 2 net damage")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner did not pay 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner bm)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "on R&D access"
    (do-game
      (new-game (default-corp ["Ben Musashi" "House of Knives"])
                (default-runner))
      (starting-hand state :corp ["Ben Musashi"])
      (play-from-hand state :corp "Ben Musashi" "R&D")
      (take-credits state :corp)
      (let [bm (get-content state :rd 0)]
        (core/rez state :corp bm)
        (run-empty-server state "R&D")
        ;; runner now chooses which to access.
        (click-prompt state :runner "Card from deck")
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 2 net damage")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner did not pay 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "Ben Musashi")
        (click-prompt state :runner "No action")
        (run-empty-server state "R&D")
        (click-prompt state :runner "Card from deck")
        (click-prompt state :runner "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "pay even when trashed"
    (do-game
      (new-game (default-corp [(qty "Ben Musashi" 3) (qty "House of Knives" 3)])
                (default-runner))
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [bm (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner bm)
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        ;; should now have prompt to pay 2 net for HoK
        (click-prompt state :runner "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Check runner chooses order of payment"
    (do-game
      (new-game (default-corp ["Ben Musashi" "Obokata Protocol"])
                (default-runner [(qty "Sure Gamble" 6)]))
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "Obokata Protocol" "Server 1")
      (take-credits state :corp)
      (let [bm (get-content state :remote1 0)
            op (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner op)
        ;; prompt should be asking for the net damage costs
        (is (= "Obokata Protocol" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay steal costs")
        (click-prompt state :runner "Pay to steal")
        (click-prompt state :runner "2 net damage")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "4 net damage")
        (is (= 5 (count (:discard (get-runner)))) "Runner took 4 net damage")
        (is (= 1 (count (:scored (get-runner)))) "Scored agenda"))))
  (testing "Check Fetal AI can be stolen, #2586"
    (do-game
      (new-game (default-corp ["Ben Musashi" "Fetal AI"])
                (default-runner [(qty "Sure Gamble" 5)]))
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "Fetal AI" "Server 1")
      (take-credits state :corp)
      (let [bm (get-content state :remote1 0)
            fai (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner fai)
        ;; prompt should be asking for the net damage costs
        (is (= "Fetal AI" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay steal costs")
        (click-prompt state :runner "Pay to steal")
        (click-prompt state :runner "2 [Credits]")
        (is (= 3 (:credit (get-runner))) "Runner paid 2 credits")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "2 net damage")
        (is (= 4 (count (:discard (get-runner)))) "Runner took 4 net damage - 2 from Fetal, 2 from Ben")
        (is (= 1 (count (:scored (get-runner)))) "Scored agenda")))))

(deftest bernice-mai
  ;; Bernice Mai
  (testing "Basic test - successful and unsuccessful"
    (do-game
      (new-game (default-corp [(qty "Bernice Mai" 3) (qty "Hedge Fund" 3) (qty "Wall of Static" 3)])
                (default-runner))
      (starting-hand state :corp ["Bernice Mai" "Bernice Mai" "Bernice Mai"])
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Bernice Mai" "R&D")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 1 (count-tags state)))
      (is (= 2 (:credit (get-runner))) "Runner paid 3cr to trash Bernice")
      (core/rez state :corp (get-content state :remote2 0))
      (core/gain state :runner :credit 20)
      (run-empty-server state :remote2)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (not (get-content state :remote2 0)) "Bernice auto-trashed from unsuccessful trace")
      (is (not (:run @state)) "Run ended when Bernice was trashed from server")
      (core/rez state :corp (get-content state :rd 0))
      (run-empty-server state :rd)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (:card (first (:prompt (get-runner)))) "Accessing a card from R&D; not showing Bernice Mai as possible access")))
  (testing "interaction with Dedicated Response Team"
    (do-game
      (new-game (default-corp [(qty "Bernice Mai" 3) "Dedicated Response Team"])
                (default-runner))
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
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
    (new-game (default-corp ["Bio Vault"])
              (default-runner))
    (play-from-hand state :corp "Bio Vault" "New remote")
    (take-credits state :corp)
    (let [bv (get-content state :remote1 0)]
      (run-on state "Server 1")
      (core/rez state :corp (refresh bv))
      (card-ability state :corp (refresh bv) 0)
      (is (:run @state) "Bio Vault doesn't fire if less than 2 advancements")
      (run-successful state)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (advance state (refresh bv) 2)
      (take-credits state :corp)
      (run-on state "Server 1")
      (card-ability state :corp (refresh bv) 0)
      (is (not (:run @state)) "Bio Vault fires with 2 advancement tokens")
      (is (= 1 (count (:discard (get-corp)))) "Bio Vault trashed"))))

(deftest breaker-bay-grid
  ;; Breaker Bay Grid - Reduce rez cost of other cards in this server by 5 credits
  (do-game
   (new-game (default-corp [(qty "Breaker Bay Grid" 2) "The Root" "Strongbox"])
             (default-runner))
   (core/gain state :corp :click 1)
   (play-from-hand state :corp "Breaker Bay Grid" "New remote")
   (play-from-hand state :corp "The Root" "Server 1")
   (let [bbg1 (get-content state :remote1 0)
         root (get-content state :remote1 1)]
     (core/rez state :corp bbg1)
     (core/rez state :corp root)
     (is (= 4 (:credit (get-corp))) "Paid only 1 to rez The Root")
     (play-from-hand state :corp "Breaker Bay Grid" "R&D")
     (play-from-hand state :corp "Strongbox" "R&D")
     (let [bbg2 (get-content state :rd 0)
           sbox (get-content state :rd 1)]
       (core/rez state :corp bbg2)
       (core/rez state :corp sbox)
       (is (= 1 (:credit (get-corp))) "Paid full 3 credits to rez Strongbox")))))

(deftest bryan-stinson
  ;; Bryan Stinson - play a transaction from archives and remove from game. Ensure Currents are RFG and not trashed.
  (do-game
   (new-game (default-corp ["Bryan Stinson" "Death and Taxes"
                            "Paywall Implementation" "Global Food Initiative"
                            "IPO"])
             (default-runner ["Interdiction"]))
    (trash-from-hand state :corp "Death and Taxes")
    (play-from-hand state :corp "Bryan Stinson" "New remote")
    (let [bs (get-content state :remote1 0)]
      (core/rez state :corp (refresh bs))
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
      (run-on state "HQ")
      (run-successful state)
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
    (new-game (default-corp [(qty "Calibration Testing" 2) "Project Junebug" "PAD Campaign"])
              (default-runner))
    (core/gain state :corp :credit 10)
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Calibration Testing" "New remote")
    (play-from-hand state :corp "Project Junebug" "Server 1")
    (let [ct (get-content state :remote1 0)
          pj (get-content state :remote1 1)]
      (core/rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-card state :corp pj)
      (is (= 1 (get-counters (refresh pj) :advancement)) "Project Junebug advanced")
      (is (= 1 (count (:discard (get-corp)))) "Calibration Testing trashed"))
    (play-from-hand state :corp "Calibration Testing" "New remote")
    (play-from-hand state :corp "PAD Campaign" "Server 2")
    (let [ct (get-content state :remote2 0)
          pad (get-content state :remote2 1)]
      (core/rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-card state :corp pad)
      (is (= 1 (get-counters (refresh pad) :advancement)) "PAD Campaign advanced")
      (is (= 2 (count (:discard (get-corp)))) "Calibration Testing trashed"))))

(deftest caprice-nisei
  ;; Caprice Nisei - Psi game for ETR after runner passes last ice
  (do-game
   (new-game (default-corp [(qty "Caprice Nisei" 3) (qty "Quandary" 3)])
             (default-runner))
   (play-from-hand state :corp "Caprice Nisei" "New remote")
   (take-credits state :corp)
   (let [caprice (get-content state :remote1 0)]
     ;; Check Caprice triggers properly on no ice (and rezzed)
     (core/rez state :corp caprice)
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
     (run-continue state)
     (is (empty? (get-in @state [:corp :prompt])) "Caprice not trigger on first ice")
     (run-continue state) ; Caprice prompt after this
     (is (prompt-is-card? state :corp caprice)
         "Corp has Caprice prompt (triggered automatically as runner passed last ice)")
     (is (prompt-is-card? state :runner caprice) "Runner has Caprice prompt")
     (click-prompt state :corp "0 [Credits]")
     (click-prompt state :runner "1 [Credits]")
     (is (not (:run @state)) "Run ended by Caprice")
     (is (empty? (get-in @state [:corp :prompt])) "Caprice prompted cleared")
     ;; Check Caprice does not trigger on other servers
     (run-on state "HQ")
     (is (empty? (get-in @state [:corp :prompt])) "Caprice does not trigger on other servers"))))

(deftest chilo-city-grid
  ;; ChiLo City Grid - Give 1 tag for successful traces during runs on its server
  (do-game
    (new-game (default-corp [(qty "Caduceus" 2) "ChiLo City Grid"])
              (default-runner))
    (play-from-hand state :corp "ChiLo City Grid" "New remote")
    (play-from-hand state :corp "Caduceus" "Server 1")
    (take-credits state :corp)
    (let [chilo (get-content state :remote1 0)
          cad (get-ice state :remote1 0)]
      (run-on state "R&D")
      (core/rez state :corp cad)
      (core/rez state :corp chilo)
      (card-subroutine state :corp cad 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 3 (:credit (get-corp))) "Trace was successful")
      (is (zero? (count-tags state)) "No tags given for run on different server")
      (run-successful state)
      (run-on state "Server 1")
      (card-subroutine state :corp cad 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 6 (:credit (get-corp))) "Trace was successful")
      (is (= 1 (count-tags state))
          "Runner took 1 tag given from successful trace during run on ChiLo server"))))

(deftest code-replicator
  ;; Code Replicator - trash to make runner approach passed (rezzed) ice again
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) "Code Replicator"])
              (default-runner))
    (core/gain state :corp :click 1)
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Code Replicator" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (is (= 3 (:position (get-in @state [:run]))) "Initial position outermost Ice Wall")
    (let [cr (get-content state :hq 0)
          i1 (get-ice state :hq 0)
          i2 (get-ice state :hq 1)
          i3 (get-ice state :hq 2)]
      (core/rez state :corp cr)
      (is (= 5 (:credit (get-corp))))
      (core/rez state :corp i3)
      (run-continue state)
      (is (= 2 (:position (get-in @state [:run]))) "Passed Ice Wall")
      (card-ability state :corp cr 0)
      (is (= 3 (:position (get-in @state [:run]))) "Runner approaching previous Ice Wall")
      (is (empty? (get-content state :hq))
          "Code Replicatior trashed from root of HQ"))))

(deftest corporate-troubleshooter
  ;; Corporate Troubleshooter - Pay X credits and trash to add X strength to a piece of rezzed ICE
  (do-game
    (new-game (default-corp [(qty "Quandary" 2) "Corporate Troubleshooter"])
              (default-runner))
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Corporate Troubleshooter" "HQ")
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Quandary" "HQ")
    (let [ct (get-content state :hq 0)
          q1 (get-ice state :hq 0)
          q2 (get-ice state :hq 1)]
      (core/rez state :corp q1)
      (is (= 8 (:credit (get-corp))))
      (core/rez state :corp ct)
      (card-ability state :corp ct 0)
      (click-prompt state :corp "5")
      (click-card state :corp q2)
      (is (nil? (:current-strength (refresh q2))) "Outer Quandary unrezzed; can't be targeted")
      (click-card state :corp q1)
      (is (= 5 (:current-strength (refresh q1))) "Inner Quandary boosted to 5 strength")
      (is (empty? (get-content state :hq))
          "Corporate Troubleshooter trashed from root of HQ")
      (take-credits state :corp)
      (is (zero? (:current-strength (refresh q1)))
          "Inner Quandary back to default 0 strength after turn ends"))))

(deftest crisium-grid
  ;; Crisium Grid
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Crisium Grid" 2)])
                (default-runner ["Desperado" "Temüjin Contract"]))
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (core/rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (is (= 4 (:credit (get-corp))) "Corp has 4 credits")
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Desperado")
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "HQ")
      (run-empty-server state "HQ")
      (is (= 2 (:credit (get-runner))) "No Desperado or Temujin credits")
      (is (not (:successful-run (:register (get-runner)))) "No successful run in register")))
  (testing "with Gauntlet, #3082"
    (do-game
      (new-game (default-corp [(qty "Crisium Grid" 2)(qty "Vanilla" 2)])
                (default-runner ["The Gauntlet" "Temüjin Contract"]))
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (play-from-hand state :corp "Vanilla" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (core/rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "The Gauntlet")
      (run-on state "HQ")
      (run-successful state)
      (is (seq (:prompt (get-runner))) "The Gauntlet has a prompt"))))

(deftest cyberdex-virus-suite
  ;; Cyberdex Virus Suite
  (testing "Purge ability"
    (do-game
      (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
                (default-runner ["Cache" "Medium"]))
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
        (core/rez state :corp cvs)
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
      (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
                (default-runner ["Cache" "Medium"]))
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
      (new-game (default-corp ["Cyberdex Virus Suite" "Braintrust"])
                (default-runner ["Cache"]))
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

(deftest drone-screen
  ;; Drone Screen
  (do-game
    (new-game (default-corp ["Drone Screen"])
              (default-runner))
    (play-from-hand state :corp "Drone Screen" "New remote")
    (let [drone (get-content state :remote1 0)]
      (core/rez state :corp drone)
      (core/gain-tags state :runner 1)
      (take-credits state :corp)
      (run-on state "Server 1")
      (is (zero? (-> (get-runner) :discard count)) "Heap should start empty")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should discard 1 card from meat damage from losing Drone Screen trace"))))

(deftest forced-connection
  ;; Forced Connection - ambush, trace(3) give the runner 2 tags
  (do-game
    (new-game (default-corp [(qty "Forced Connection" 3)])
              (default-runner))
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

(deftest georgia-emelyov
  ;; Georgia Emelyov
  (do-game
    (new-game (default-corp ["Georgia Emelyov"])
              (default-runner))
    (play-from-hand state :corp "Georgia Emelyov" "New remote")
    (let [geo (get-content state :remote1 0)]
      (core/rez state :corp geo)
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
  (do-game
    (new-game (default-corp ["Giordano Memorial Field" "Hostile Takeover"])
              (default-corp [(qty "Fan Site" 3)]))
    (play-from-hand state :corp "Giordano Memorial Field" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
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
    (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner should only get 1 choice")
    (is (= "End the run" (-> (get-runner) :prompt first :choices first)) "Only choice should be End the run")
    (click-prompt state :runner "End the run")
    (is (not (:run @state)) "Run should be ended from Giordano Memorial Field ability")))

(deftest helheim-servers
  ;; Helheim Servers - Full test
  (do-game
    (new-game (default-corp ["Helheim Servers" "Gutenberg" "Vanilla"
                             "Jackson Howard" "Hedge Fund"])
              (default-runner))
    (play-from-hand state :corp "Helheim Servers" "R&D")
    (play-from-hand state :corp "Gutenberg" "R&D")
    (play-from-hand state :corp "Vanilla" "R&D")
    (take-credits state :corp)
    (run-on state "R&D")
    (is (:run @state))
    (let [helheim (get-content state :rd 0)
          gutenberg (get-ice state :rd 0)
          vanilla (get-ice state :rd 1)]
      (core/rez state :corp helheim)
      (core/rez state :corp gutenberg)
      (core/rez state :corp vanilla)
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (zero? (:current-strength (refresh vanilla))))
      (card-ability state :corp helheim 0)
      (click-card state :corp (find-card "Jackson Howard" (:hand (get-corp))))
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 8 (:current-strength (refresh gutenberg))))
      (is (= 2 (:current-strength (refresh vanilla))))
      (card-ability state :corp helheim 0)
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= 2 (count (:discard (get-corp)))))
      (is (= 10 (:current-strength (refresh gutenberg))))
      (is (= 4 (:current-strength (refresh vanilla))))
      (run-jack-out state)
      (is (not (:run @state)))
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (zero? (:current-strength (refresh vanilla)))))))

(deftest hokusai-grid
  ;; Hokusai Grid - Do 1 net damage when run successful on its server
  (do-game
    (new-game (default-corp ["Hokusai Grid"])
              (default-runner))
    (play-from-hand state :corp "Hokusai Grid" "HQ")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :hq 0))
    (run-empty-server state :rd)
    (is (empty? (:discard (get-runner))) "No net damage done for successful run on R&D")
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-runner)))) "1 net damage done for successful run on HQ")))

(deftest intake
  ;; Intake - Trace4, add an installed program or virtual resource to the grip
  (do-game
    (new-game (default-corp [(qty "Intake" 3)])
              (default-runner ["Corroder" "Fester" "Daily Casts"]))
    (starting-hand state :corp ["Intake" "Intake"])
    (play-from-hand state :corp "Intake" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 5 :credit 10)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Fester")
    (play-from-hand state :runner "Daily Casts")
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (empty? (:hand (get-runner))) "Runner starts with no cards in hand")
    (click-card state :corp (get-program state 0))
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (run-on state "Archives")
    (run-successful state)
    (is (empty? (:prompt (get-corp))) "No prompt from Archives access")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (run-on state "Server 1")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
    (click-card state :corp (get-resource state 0))
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (click-prompt state :runner "No action") ; trash
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (click-prompt state :corp "Done")
    (click-prompt state :runner "No action") ; trash
    (is (empty? (:prompt (get-corp))) "Prompt closes after done")
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "5")
    (is (empty? (:prompt (get-corp))) "Prompt closes after lost trace")))

(deftest jinja-city-grid
  ;; Jinja City Grid - install drawn ice, lowering install cost by 4
  (testing "Single draws"
    (do-game
    (new-game (default-corp ["Jinja City Grid" (qty "Vanilla" 3) (qty "Ice Wall" 3)])
              (default-runner))
    (starting-hand state :corp ["Jinja City Grid"])
    (core/gain state :corp :click 6)
    (play-from-hand state :corp "Jinja City Grid" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (dotimes [n 5]
      (core/click-draw state :corp 1)
      (click-prompt state :corp (-> (get-corp) :prompt first :choices first))
      (is (= 4 (:credit (get-corp))) "Not charged to install ice")
      (is (= (inc n) (count (get-in @state [:corp :servers :remote1 :ices]))) (str n " ICE protecting Remote1")))
    (core/click-draw state :corp 1)
    (click-prompt state :corp (-> (get-corp) :prompt first :choices first))
    (is (= 3 (:credit (get-corp))) "Charged to install ice")
    (is (= 6 (count (get-in @state [:corp :servers :remote1 :ices]))) "6 ICE protecting Remote1")))
  (testing "Drawing non-ice on runner's turn"
    (do-game
      (new-game
        (default-corp ["Jinja City Grid" (qty "Hedge Fund" 3)])
        (make-deck "Laramy Fisk: Savvy Investor" ["Eden Shard"]))
      (starting-hand state :corp ["Jinja City Grid"])
      (play-from-hand state :corp "Jinja City Grid" "HQ")
      (core/rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Yes")
      (is (= :bogus (-> (get-corp) :prompt first :prompt-type)) "Corp has a bogus prompt to fake out the runner")
      (click-prompt state :corp "Carry on!")
      (click-prompt state :runner "No action"))))

(deftest keegan-lane
  ;; Keegan Lane - Trash self and remove 1 Runner tag to trash a program
  (do-game
    (new-game (default-corp ["Keegan Lane"])
              (default-runner ["Corroder"]))
    (play-from-hand state :corp "Keegan Lane" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state :hq)
    (let [keeg (get-content state :hq 0)]
      (core/rez state :corp keeg)
      (card-ability state :corp keeg 0)
      (is (= 1 (count (get-content state :hq))) "Keegan didn't fire, Runner has no tags")
      (core/gain-tags state :runner 2)
      (card-ability state :corp keeg 0)
      (click-card state :corp (get-program state 0))
      (is (= 1 (count-tags state)) "1 tag removed")
      (is (= 1 (count (:discard (get-corp)))) "Keegan trashed")
      (is (= 1 (count (:discard (get-runner)))) "Corroder trashed"))))

(deftest manta-grid
  ;; If the Runner has fewer than 6 or no unspent clicks on successful run, corp gains a click next turn.
  (do-game
    (new-game (default-corp ["Manta Grid"])
              (default-runner))
    (starting-hand state :runner [])
    (is (= 3 (:click (get-corp))) "Corp has 3 clicks")
    (play-from-hand state :corp "Manta Grid" "HQ")
    (core/rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (core/click-draw state :runner nil)
    (core/click-draw state :runner nil)
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
  (testing "Simultaneous Interaction with Security Nexus"
    (do-game
      (new-game (default-corp ["Marcus Batty" "Enigma"])
                (default-runner ["Security Nexus"]))
      (play-from-hand state :corp "Marcus Batty" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 8)
      (play-from-hand state :runner "Security Nexus")
      (let [mb (get-content state :hq 0)
            en (get-ice state :hq 0)
            sn (-> @state :runner :rig :hardware first)]
        (run-on state "HQ")
        (core/rez state :corp mb)
        (core/rez state :corp en)
        (card-ability state :corp mb 0)
        (card-ability state :runner sn 0)
        ;; both prompts should be on Batty
        (is (prompt-is-card? state :corp mb) "Corp prompt is on Marcus Batty")
        (is (prompt-is-card? state :runner mb) "Runner prompt is on Marcus Batty")
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (prompt-is-card? state :corp sn) "Corp prompt is on Security Nexus")
        (is (prompt-is-type? state :runner :waiting) "Runner prompt is waiting for Corp")))))

(deftest mumbad-city-grid
  ;; Mumbad City Grid - when runner passes a piece of ice, swap that ice with another from this server
  (testing "1 ice"
    (do-game
      (new-game (default-corp ["Mumbad City Grid" "Quandary"])
                (default-runner))
      (play-from-hand state :corp "Mumbad City Grid" "New remote")
      (play-from-hand state :corp "Quandary" "Server 1")
      (let [mcg (get-content state :remote1 0)]
        (core/rez state :corp mcg)
        (take-credits state :corp)
        (run-on state "Server 1")
        (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "1 ice on server")
        (card-ability state :corp (refresh mcg) 0)
        (run-continue state)
        (card-ability state :corp (refresh mcg) 0)
        (run-jack-out state)
        (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "Still 1 ice on server"))))
  (testing "fire before pass"
    (do-game
      (new-game (default-corp ["Mumbad City Grid" "Quandary" "Ice Wall"])
                (default-runner))
      (play-from-hand state :corp "Mumbad City Grid" "New remote")
      (play-from-hand state :corp "Quandary" "Server 1")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (let [mcg (get-content state :remote1 0)]
        (core/rez state :corp mcg)
        (take-credits state :corp)
        (run-on state "Server 1")
        (is (= 2 (:position (:run @state))) "Runner at position 2")
        (is (= 2 (count (get-in @state [:corp :servers :remote1 :ices]))) "2 ice on server")
        (is (= "Quandary" (:title (first (get-in @state [:corp :servers :remote1 :ices])))) "Quandary inner ice")
        (is (= "Ice Wall" (:title (second (get-in @state [:corp :servers :remote1 :ices])))) "Ice Wall outer ice")
        (card-ability state :corp (refresh mcg) 0)
        (run-continue state)
        (is (= 1 (:position (:run @state))) "Runner at position 1")
        (card-ability state :corp (refresh mcg) 0)
        (click-card state :corp (get-ice state :remote1 0))
        (is (= 1 (:position (:run @state))) "Runner at position 1")
        (is (= "Quandary" (:title (second (get-in @state [:corp :servers :remote1 :ices])))) "Quandary outer ice")
        (is (= "Ice Wall" (:title (first (get-in @state [:corp :servers :remote1 :ices])))) "Ice Wall inner ice")
        (run-jack-out state)
        (is (= 2 (count (get-in @state [:corp :servers :remote1 :ices]))) "Still 2 ice on server")))))

(deftest mumbad-virtual-tour
  ;; Tests that Mumbad Virtual Tour forces trash when no :slow-trash
  (do-game
    (new-game (default-corp [(qty "Mumbad Virtual Tour" 2)])
              (default-runner))
    (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    ;; MVT does not force trash when not installed
    (click-prompt state :runner "No action")
    (is (= 5 (:credit (get-runner))) "Runner not forced to trash MVT in HQ")
    (is (empty? (:discard (get-corp))) "MVT in HQ is not trashed")
    (run-empty-server state "Server 1")
    (is (= 1 (-> @state :runner :prompt first :choices count)) "Should only have a single option")
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (is (zero? (:credit (get-runner))) "Runner forced to trash MVT")
    (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed"))
  (testing "interaction with Imp"
    (do-game
      (new-game (default-corp [(qty "Mumbad Virtual Tour" 2)])
                (default-runner ["Imp"]))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      ;; Reset credits to 5
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 1")
      ;; Runner not force to trash since Imp is installed
      (is (= 2 (-> @state :runner :prompt first :choices count)) "Runner has 2 choices when Imp is installed")
      (is (= 5 (:credit (get-runner))) "Runner not forced to trash MVT when Imp installed")
      (is (empty? (:discard (get-corp))) "MVT is not force-trashed when Imp installed")
      (let [imp (get-program state 0)]
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed with Imp")
        ;; Trash Imp to reset :slow-trash flag
        (core/move state :runner (refresh imp) :discard)
        (is (not (core/any-flag-fn? state :runner :slow-trash true))))))
  (testing "interactions with Imp and various amounts of money"
    (do-game
      (new-game (default-corp [(qty "Mumbad Virtual Tour" 3)])
                (default-runner ["Imp"]))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (is (= 3 (:credit (get-runner))) "Runner paid install costs")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 1")
      (is (= #{"[Imp]: Trash card" "Pay 5 [Credits] to trash"}
             (->> (get-runner) :prompt first :choices (into #{}))) "Should have Imp and MVT options")
      (click-prompt state :runner "[Imp]: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 2")
      (is (= ["[Imp]: Trash card"] (-> (get-runner) :prompt first :choices)) "Should only have Imp option")
      (click-prompt state :runner "[Imp]: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 3")
      (is (= ["No action"] (-> (get-runner) :prompt first :choices)) "Should only have no action option")
      (click-prompt state :runner "No action")
      (is (= 2 (->> (get-corp) :discard count)) "Runner was not forced to trash MVT")))
  (testing "not forced to trash when credits below 5"
    (do-game
      (new-game (default-corp [(qty "Mumbad Virtual Tour" 3)])
                (default-runner ["Cache"]))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (is (= 4 (:credit (get-runner))) "Runner paid install costs")
      (run-empty-server state "Server 1")
      (is (= ["No action"] (-> (get-runner) :prompt first :choices)) "Can't trash"))))

(deftest mwanza-city-grid
  ;; Mwanza City Grid - runner accesses 3 additional cards, gain 2C for each card accessed
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Mwanza City Grid" (qty "Hedge Fund" 5)])
                (default-runner))
      (play-from-hand state :corp "Mwanza City Grid")
      (is (= #{"R&D" "HQ"} (-> (get-corp) :prompt first :choices set)) "Mwanza can only be installed in root of HQ or R&D")
      (click-prompt state :corp "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (core/rez state :corp mcg)
        (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
        (run-successful state)
        (click-prompt state :runner "Mwanza City Grid")
        (click-prompt state :runner "No action")
        (dotimes [c 4]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))
  (testing "effect persists through current run after trash"
    (do-game
      (new-game (default-corp ["Mwanza City Grid" (qty "Hedge Fund" 5)])
                (default-runner))
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (core/rez state :corp mcg)
        (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
        (run-successful state)
        (click-prompt state :runner "Mwanza City Grid")
        (click-prompt state :runner "Pay 5 [Credits] to trash")
        (dotimes [c 4]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))
  (testing "works well with replacement effects"
    ;; Regression test for #3456
    (do-game
      (new-game (default-corp ["Mwanza City Grid" "Hedge Fund"])
                (default-runner ["Embezzle"]))
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (take-credits state :corp)
      (core/rez state :corp (get-content state :hq 0))
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (play-run-event state (first (:hand (get-runner))) :hq)
      (click-prompt state :runner "ICE")
      (is (zero? (count (:discard (get-corp)))) "No cards trashed from HQ")
      (is (not (:run @state)) "Run ended after Embezzle completed - no accesses from Mwanza")
      (is (= 7 (:credit (get-corp))) "Corp did not gain any money from Mwanza")))
  (testing "interaction with Kitsune"
    ;; Regression test for #3469
    (do-game
      (new-game (default-corp ["Mwanza City Grid" "Breached Dome"
                               (qty "Kitsune" 2) (qty "Hedge Fund" 3)])
                (default-runner))
      (core/draw state :corp 1) ; Draw last card of deck
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (play-from-hand state :corp "Kitsune" "HQ")
      (play-from-hand state :corp "Kitsune" "R&D")
      (take-credits state :corp)
      (let [mwanza (get-content state :hq 0)
            k-hq (get-ice state :hq 0)
            k-rd (get-ice state :rd 0)]
        (core/rez state :corp mwanza)
        (core/rez state :corp k-hq)
        (core/rez state :corp k-rd)
        (run-on state "HQ")
        (card-subroutine state :corp k-hq 0)
        (click-card state :corp (find-card "Breached Dome" (:hand (get-corp))))
        (is (= 2 (-> (get-runner) :hand count)) "Runner took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :runner "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (run-jack-out state)
        (run-on state "R&D")
        (card-subroutine state :corp k-rd 0)
        (click-card state :corp (find-card "Breached Dome" (:hand (get-corp))))
        (is (= 1 (-> (get-runner) :hand count)) "Runner took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :runner "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner "No action"))
        (run-jack-out state)
        (is (= 2 (-> (get-corp) :discard count)) "Two Kitsunes trashed after resolving their subroutines")))))

(deftest neotokyo-grid
  ;; NeoTokyo Grid - Gain 1c the first time per turn a card in this server gets an advancement
  (do-game
    (new-game (default-corp ["NeoTokyo Grid" "Nisei MK II"
                             "Shipment from SanSan" "Ice Wall"])
              (default-runner))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "NeoTokyo Grid" "New remote")
    (play-from-hand state :corp "Nisei MK II" "Server 1")
    (core/rez state :corp (get-content state :remote1 0))
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

(deftest oberth-protocol
  ;; Oberth Protocol
  (do-game
    (new-game (default-corp ["Hostile Takeover" "Oberth Protocol" "Oaktown Renovation"])
              (default-runner))
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Oberth Protocol" "Server 1")
    (play-from-hand state :corp "Oaktown Renovation" "Server 1")
    (take-credits state :corp)
    (take-credits state :runner)
    (let [oberth (get-content state :remote1 0)
          oak (get-content state :remote1 1) ]
      (core/rez state :corp (refresh oberth))
      (click-card state :corp (get-scored state :corp 0))
      (advance state oak)
      (is (= 2 (get-counters (refresh oak) :advancement)) "Oaktown should have 2 advancement tokens on it"))))

(deftest off-the-grid
  ;; Off the Grid run restriction - and interaction with RP
  (do-game
   (new-game
    (make-deck "Jinteki: Replicating Perfection" [(qty "Off the Grid" 3)
                                                  (qty "Mental Health Clinic" 3)])
    (default-runner))
   (play-from-hand state :corp "Off the Grid" "New remote")
   (play-from-hand state :corp "Mental Health Clinic" "Server 1")
   (let [otg (get-content state :remote1 0)]
     (take-credits state :corp)
     (core/rez state :corp (refresh otg))
     (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
     (run-empty-server state "R&D")
     (is (not (core/can-run-server? state "Server 1")) "Runner cannot run on Off the Grid")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (not (core/can-run-server? state "Server 1")) "Off the Grid prevention persisted")
     (run-empty-server state "HQ")
     (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on Server 1")
     (is (= nil (refresh otg)) "Off the Grid trashed"))))

(deftest old-hollywood-grid
  ;; Old Hollywood Grid
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Old Hollywood Grid" (qty "House of Knives" 3)])
                (default-runner))
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [ohg (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp ohg)
        (run-successful state)
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "No stolen agendas")
        (click-card state :runner ohg)
        (click-prompt state :runner "No action")
        (core/steal state :runner (find-card "House of Knives" (:hand (get-corp))))
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Steal")
        (is (= 2 (count (:scored (get-runner)))) "2 stolen agendas"))))
  (testing "Central server"
    (do-game
      (new-game (default-corp ["Old Hollywood Grid" (qty "House of Knives" 3)])
                (default-runner))
      (play-from-hand state :corp "Old Hollywood Grid" "HQ")
      (take-credits state :corp 2)
      (let [ohg (get-content state :hq 0)]
        (run-on state "HQ")
        (core/rez state :corp ohg)
        (run-successful state)
        ;; runner now chooses which to access.
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
      (new-game (default-corp ["Old Hollywood Grid" (qty "Project Beale" 2)])
                (default-runner ["Gang Sign"]))
      (play-from-hand state :corp "Old Hollywood Grid" "HQ")
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (core/rez state :corp (get-content state :hq 0))
      (score-agenda state :corp (get-content state :remote1 0))
      ;; Gang sign fires
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "No action")
      (is (zero? (count (:scored (get-runner)))) "No stolen agendas")))
  (testing "Trash order"
    (do-game
      (new-game (default-corp ["Old Hollywood Grid" "Project Beale"])
                (default-runner))
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)
            pb (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp ohg)
        (run-successful state)
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
      (new-game (default-corp ["Old Hollywood Grid" (qty "Project Beale" 2)])
                (default-runner))
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)
            pb (get-content state :remote1 1)]
        (core/rez state :corp ohg)
        (run-empty-server state "Server 2")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda")))))

(deftest overseer-matrix
  ;; Overseer Matrix - corp takes a tag when trashing a card in this server
  (testing "Basic functionality"
    (do-game
      (new-game (default-corp ["Overseer Matrix" "Red Herrings"])
                (default-runner))
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp om)
        (run-successful state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 2 (count-tags state)) "Runner takes a tag"))))
  (testing "Effect persists after trash"
    (do-game
      (new-game (default-corp ["Overseer Matrix" (qty "Red Herrings" 3)])
                (default-runner))
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp om)
        (run-successful state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 2 (count-tags state)) "Runner takes a tag"))))
  (testing "Effect ends after current run"
    (do-game
      (new-game (default-corp ["Overseer Matrix" (qty "Red Herrings" 3)])
                (default-runner))
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)
            rh (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp om)
        (run-successful state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-card state :runner rh)
        (click-prompt state :runner "No action")
        (is (= 1 (count-tags state)) "Runner doesn't take a tag")
        (run-on state "Server 1")
        (run-successful state)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (is (empty? (:prompt (get-corp))) "No prompt for Overseer Matrix")
        (is (= 1 (count-tags state)) "Runner doesn't take a tag")))))

(deftest port-anson-grid
  ;; Port Anson Grid - Prevent the Runner from jacking out until they trash a program
  (do-game
    (new-game (default-corp ["Port Anson Grid" "Data Raven"])
              (default-runner ["Faerie" "Technical Writer"]))
    (play-from-hand state :corp "Port Anson Grid" "New remote")
    (play-from-hand state :corp "Data Raven" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Technical Writer")
    (play-from-hand state :runner "Faerie")
    (let [pag (get-content state :remote1 0)
          fae (get-program state 0)
          tw (get-resource state 0)]
      (run-on state "Server 1")
      (core/rez state :corp pag)
      (is (:cannot-jack-out (get-in @state [:run])) "Jack out disabled for Runner") ; UI button greyed out
      (core/trash state :runner tw)
      (is (:cannot-jack-out (get-in @state [:run])) "Resource trash didn't disable jack out prevention")
      (core/trash state :runner fae)
      (is (nil? (:cannot-jack-out (get-in @state [:run]))) "Jack out enabled by program trash")
      (run-on state "Server 1")
      (is (:cannot-jack-out (get-in @state [:run])) "Prevents jack out when upgrade is rezzed prior to run"))))

(deftest prisec
  ;; Prisec
  (testing "Basic test - Pay 2 credits to give runner 1 tag and do 1 meat damage, only when installed"
    (do-game
      (new-game (default-corp [(qty "Prisec" 2)])
                (default-runner))
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
      (is (not (:prompt @state)) "Prisec does not trigger from HQ")))
  (testing "Multiple unrezzed upgrades in Archives interaction with DRT"
    (do-game
      (new-game (default-corp [(qty "Prisec" 2) "Dedicated Response Team"])
                (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (play-from-hand state :corp "Prisec" "Archives")
      (play-from-hand state :corp "Prisec" "Archives")
      (core/gain state :corp :click 1 :credit 14)
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :archives)
      (is (:run @state) "Run still active")
      (click-prompt state :runner "Unrezzed upgrade in Archives")
      (click-card state :runner (get-content state :archives 0))
      (click-prompt state :corp "Yes") ; corp pay for PriSec
      (click-prompt state :runner "No action") ; runner don't pay to trash
      (is (:run @state) "Run still active")
      (click-prompt state :runner "Unrezzed upgrade in Archives")
      (click-prompt state :corp "Yes") ; corp pay for PriSec
      (click-prompt state :runner "No action") ; runner don't pay to trash
      (is (not (:run @state)) "Run ended")
      (is (= 4 (count (:discard (get-runner)))) "Runner took 4 meat damage"))))

(deftest product-placement
  ;; Product Placement - Gain 2 credits when Runner accesses it
  (do-game
    (new-game (default-corp ["Product Placement"])
              (default-runner))
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
      (new-game (default-corp ["Red Herrings" "House of Knives"])
                (default-runner))
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [rh (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        ;; prompt should be asking for the 5cr cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 5cr")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner rh)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay 5 [Credits] to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Cost increase even when trashed"
    (do-game
      (new-game (default-corp [(qty "Red Herrings" 3) (qty "House of Knives" 3)])
                (default-runner))
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [rh (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        ;; should now have prompt to pay 5cr for HoK
        (click-prompt state :runner "Pay 5 [Credits] to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Trashed from HQ"
    (do-game
      (new-game (default-corp ["Red Herrings" "House of Knives"])
                (default-runner))
      (trash-from-hand state :corp "Red Herrings")
      (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      ;; prompt should be asking to steal HoK
      (is (= "Steal" (first (:choices (first (:prompt (get-runner))))))
          "Runner being asked to Steal")))
  (testing "Don't affect runs on other servers"
    (do-game
      (new-game (default-corp ["Red Herrings" "House of Knives"])
                (default-runner))
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "New remote")
      (take-credits state :corp 1)
      (let [rh (get-content state :remote1 0)]
        (core/rez state :corp rh)
        (run-empty-server state "Server 2")
        ;; access is automatic
        (click-prompt state :runner "Steal")
        (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda")))))

(deftest ruhr-valley
  ;; Ruhr Valley
  (testing "Basic test - As an additional cost to make a run on this server, the Runner must spend a click."
    (do-game
      (new-game (default-corp ["Ruhr Valley"])
                (default-runner))
      (play-from-hand state :corp "Ruhr Valley" "HQ")
      (take-credits state :corp)
      (let [ruhr (get-content state :hq 0)]
        (core/rez state :corp ruhr)
        (is (= 4 (:click (get-runner))))
        (run-on state :hq)
        (run-jack-out state)
        (is (= 2 (:click (get-runner))))
        (take-credits state :runner 1)
        (is (= 1 (:click (get-runner))))
        (is (not (core/can-run-server? state "HQ")) "Runner can't run - no additional clicks")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 4 (:click (get-runner))))
        (is (= 7 (:credit (get-runner))))
        (run-on state :hq)
        (run-successful state)
        (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash / 7 cr - 4 cr
        (is (= 2 (:click (get-runner))))
        (is (= 3 (:credit (get-runner))))
        (run-on state :hq)
        (run-jack-out state)
        (is (= 1 (:click (get-runner)))))))
  (testing "If the runner trashes with one click left, the ability to run is enabled"
    (do-game
      (new-game (default-corp ["Ruhr Valley"])
                (default-runner))
      (play-from-hand state :corp "Ruhr Valley" "HQ")
      (take-credits state :corp)
      (let [ruhr (get-content state :hq 0)]
        (core/rez state :corp ruhr)
        (is (= 4 (:click (get-runner))))
        (run-on state :rd)
        (run-jack-out state)
        (is (= 3 (:click (get-runner))))
        (run-on state :hq)
        (run-successful state)
        (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash / 6 cr - 4 cr
        (is (= 1 (:click (get-runner))))
        (run-on state :hq)))))

(deftest ryon-knight
  ;; Ryon Knight - Trash during run to do 1 brain damage if Runner has no clicks remaining
  (do-game
    (new-game (default-corp ["Ryon Knight"])
              (default-runner))
    (play-from-hand state :corp "Ryon Knight" "HQ")
    (take-credits state :corp)
    (let [ryon (get-content state :hq 0)]
      (run-on state :hq)
      (core/rez state :corp ryon)
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

(deftest satellite-grid
  ;; Satellite Grid - Add 1 fake advancement on all ICE protecting server
  (do-game
    (new-game (default-corp ["Satellite Grid" (qty "Ice Wall" 2)])
              (default-runner))
    (play-from-hand state :corp "Satellite Grid" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)
          sg (get-content state :hq 0)]
      (core/gain state :corp :click 1)
      (advance state iw1)
      (core/rez state :corp sg)
      (core/rez state :corp (refresh iw1))
      (is (= 1 (:extra-advance-counter (refresh iw1))) "1 fake advancement token")
      (is (= 1 (get-counters (refresh iw1) :advancement)) "Only 1 real advancement token")
      (is (= 3 (:current-strength (refresh iw1))) "Satellite Grid counter boosting strength by 1")
      (core/rez state :corp (refresh iw2))
      (is (= 1 (:current-strength (refresh iw2))) "Satellite Grid not impacting ICE elsewhere")
      (core/derez state :corp sg)
      (is (= 2 (:current-strength (refresh iw1))) "Ice Wall strength boost only from real advancement"))))

(deftest self-destruct
  ;; Self-destruct
  (do-game
    (new-game (default-corp ["Self-destruct" "Dedicated Response Team" "Ice Wall"])
              (default-runner))
    (core/gain state :corp :credit 100 :click 4)
    (play-from-hand state :corp "Self-destruct" "New remote")
    (play-from-hand state :corp "Dedicated Response Team" "Server 1")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (let [self (get-content state :remote1 0)]
      (take-credits state :corp)
      (run-on state "Server 1")
      (card-ability state :corp self 0)
      (is (= 3 (-> (get-corp) :discard count)) "All 3 cards from Server 1 should be in discard")
      (is (= 2 (-> (get-corp) :prompt first :base)) "Self-destruct base trace should start at 2")
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in heap")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 3 (-> (get-runner) :discard count)) "Runner should take 3 net damage from losing Self-destruct trace"))))

(deftest signal-jamming
  ;; Trash to stop installs for the rest of the run
  (do-game
    (new-game (default-corp [(qty "Signal Jamming" 3)])
              (default-runner [(qty "Self-modifying Code" 3) "Reaver"]))
    (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
    (play-from-hand state :corp "Signal Jamming" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Self-modifying Code")
    (let [smc1 (get-program state 0)
          smc2 (get-program state 1)
          sj (get-content state :hq 0)]
      (core/rez state :corp sj)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :corp sj 0)
      (card-ability state :runner smc1 0)
      (is (empty? (:prompt (get-runner))) "SJ blocking SMC")
      (run-jack-out state)
      (card-ability state :runner smc2 0)
      (click-prompt state :runner "Reaver"))))

(deftest strongbox
  ;; Strongbox
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Strongbox" "House of Knives"])
                (default-runner))
      (play-from-hand state :corp "Strongbox" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [sb (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp sb)
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 5cr")
        (click-prompt state :runner "No action")
        (is (= 3 (:click (get-runner))) "Runner was not charged 1click")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner sb)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay [Click] to steal")
        (is (= 1 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Click cost even when trashed"
    (do-game
      (new-game (default-corp [(qty "Strongbox" 3) (qty "House of Knives" 3)])
                (default-runner))
      (play-from-hand state :corp "Strongbox" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [sb (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp sb)
        (run-empty-server state "Server 1")
        (click-card state :runner sb)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        (click-prompt state :runner "Pay [Click] to steal")
        (is (= 2 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda")))))

(deftest surat-city-grid
  ;; Surat City Grid - Trigger on rez of a card in/protecting same server to rez another card at 2c discount
  (do-game
    (new-game (default-corp [(qty "Surat City Grid" 2) (qty "Cyberdex Virus Suite" 2)
                             "Enigma" "Wraparound"])
              (default-runner))
    (core/gain state :corp :credit 15 :click 8)
    (play-from-hand state :corp "Surat City Grid" "New remote")
    (play-from-hand state :corp "Wraparound" "Server 1")
    (play-from-hand state :corp "Cyberdex Virus Suite" "Server 1")
    (let [scg1 (get-content state :remote1 0)
          cvs1 (get-content state :remote1 1)
          wrap (get-ice state :remote1 0)]
      (core/rez state :corp scg1)
      (core/rez state :corp cvs1)
      (is (= 15 (:credit (get-corp))))
      (is (= (:cid scg1) (-> (get-corp) :prompt first :card :cid)) "Surat City Grid triggered from upgrade in same remote")
      (click-prompt state :corp "Yes")
      (click-card state :corp wrap)
      (is (:rezzed (refresh wrap)) "Wraparound is rezzed")
      (is (= 15 (:credit (get-corp))) "Wraparound rezzed for free with 2c discount from SCG")
      (play-from-hand state :corp "Surat City Grid" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
      (let [scg2 (get-content state :hq 0)
            cvs2 (get-content state :hq 1)
            enig (get-ice state :hq 0)]
        (core/rez state :corp scg2)
        (core/rez state :corp cvs2)
        (is (empty? (:prompt (get-corp))) "SCG didn't trigger, upgrades in root of same central aren't considered in server")
        (core/derez state :corp (refresh wrap))
        (core/rez state :corp enig)
        (is (= (:cid scg2) (-> (get-corp) :prompt first :card :cid)) "SCG did trigger for ICE protecting HQ")))))

(deftest tempus
  ;; Tempus - Trace^3, the runner chooses to lose 2 clicks or take 1 brain damage
  (do-game
    (new-game (default-corp [(qty "Tempus" 3)])
              (default-runner [(qty "Sure Gamble" 3)]))
    (starting-hand state :corp ["Tempus"])
    (play-from-hand state :corp "Tempus" "New remote")
    (take-credits state :corp)
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
    (click-prompt state :runner "Lose [Click][Click]")
    (is (= 1 (:click (get-runner))) "Runner loses 2 clicks")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (run-on state "Server 1")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (is (zero? (:brain-damage (get-runner))) "Runner starts with 0 brain damage")
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (take-credits state :runner)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-successful state)
    (is (= 1 (:brain-damage (get-runner))) "Runner takes no brain damage")
    (is (= 3 (:click (get-runner))) "Runner loses no clicks")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner starts with 1 brain damage")
    (click-prompt state :runner "Take 1 brain damage")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 1 brain damage")
    (click-prompt state :runner "No action") ; don't trash
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "4")
    (click-prompt state :runner "Pay 0 [Credits] to trash")))

(deftest the-twins
  ;; The Twins
  (do-game
    (new-game (default-corp ["The Twins" (qty "Ice Wall" 10)])
              (default-runner ["Corroder"]))
    (starting-hand state :corp ["The Twins" "Ice Wall" "Ice Wall"])
    (play-from-hand state :corp "The Twins" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (let [twins (get-content state :remote1 0)
          iw (get-ice state :remote1 0)]
      (core/rez state :corp twins)
      (core/rez state :corp iw)
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [cor (get-program state 0)]
        (run-on state "Server 1")
        (card-ability state :runner cor 0)
        (run-continue state)
        (is (zero? (-> @state :run :position)) "Run should be at position 0")
        (card-ability state :corp twins 0)
        (click-card state :corp (-> (get-corp) :hand first))
        (is (= 1 (-> @state :run :position)) "Run should be moved back to position 1")))))

(deftest tori-hanzo
  ;; Tori Hanzō - Pay to do 1 brain damage instead of net damage
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Pup" "Tori Hanzō"])
                (default-runner [(qty "Sure Gamble" 3) "Net Shield"]))
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Pup" "HQ")
      (play-from-hand state :corp "Tori Hanzō" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Net Shield")
      (run-on state "HQ")
      (let [pup (get-ice state :hq 0)
            tori (get-content state :hq 0)
            nshld (get-program state 0)]
        (core/rez state :corp pup)
        (core/rez state :corp tori)
        (card-subroutine state :corp pup 0)
        (card-ability state :runner nshld 0)
        (click-prompt state :runner "Done")
        (is (empty? (:discard (get-runner))) "1 net damage prevented")
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Done") ; decline to prevent
        (is (= 1 (count (:discard (get-runner)))) "1 net damage; previous prevention stopped Tori ability")
        (run-jack-out state)
        (run-on state "HQ")
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Done")
        (click-prompt state :corp "Yes")
        (is (= 2 (count (:discard (get-runner)))) "1 brain damage suffered")
        (is (= 1 (:brain-damage (get-runner)))))))
  (testing "with Hokusai Grid: Issue #2702"
    (do-game
      (new-game (default-corp ["Tori Hanzō" "Hokusai Grid"])
                (default-runner))
      (core/gain state :corp :credit 5)
      (play-from-hand state :corp "Hokusai Grid" "Archives")
      (play-from-hand state :corp "Tori Hanzō" "Archives")
      (take-credits state :corp)
      (run-on state "Archives")
      (let [hg (get-content state :archives 0)
            tori (get-content state :archives 1)]
        (core/rez state :corp hg)
        (core/rez state :corp tori)
        (run-successful state)
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
      (new-game (default-corp ["Tori Hanzō" (qty "Pup" 2) (qty "Neural EMP" 2)])
                (default-runner))
      (core/gain state :corp :credit 8)
      (play-from-hand state :corp "Tori Hanzō" "New remote")
      (play-from-hand state :corp "Pup" "Server 1")
      (take-credits state :corp)
      (run-on state "Server 1")
      (let [tori (get-content state :remote1 0)
            pup (get-ice state :remote1 0)]
        (core/rez state :corp pup)
        (core/rez state :corp tori)
        (card-subroutine state :corp pup 0)
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
    (new-game (default-corp ["Eve Campaign"
                             "Underway Grid"])
              (default-runner ["Drive By"]))
    (play-from-hand state :corp "Underway Grid" "New remote")
    (play-from-hand state :corp "Eve Campaign" "Server 1")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :remote1 0))
    (let [eve1 (get-content state :remote1 1)]
      (play-from-hand state :runner "Drive By")
      (click-card state :runner eve1)
      (is (empty? (:discard (get-corp))) "Expose and trash prevented"))))

(deftest valley-grid
  ;; Valley Grid
  (testing "Reduce Runner max hand size and restore it even if trashed"
    (do-game
      (new-game (default-corp [(qty "Valley Grid" 3) (qty "Ice Wall" 3)])
                (default-runner))
      (play-from-hand state :corp "Valley Grid" "New remote")
      (take-credits state :corp 2)
      (run-on state "Server 1")
      (let [vg (get-content state :remote1 0)]
        (core/rez state :corp vg)
        (card-ability state :corp vg 0)
        (card-ability state :corp vg 0) ; only need the run to exist for test, just pretending the Runner has broken all subs on 2 ice
        (is (= 3 (core/hand-size state :runner)) "Runner max hand size reduced by 2")
        (is (= 2 (get-in (refresh vg) [:times-used])) "Saved number of times Valley Grid used")
        (run-successful state)
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; pay to trash
        (take-credits state :runner 3)
        (is (= 5 (core/hand-size state :runner)) "Runner max hand size increased by 2 at start of Corp turn")))))

(deftest warroid-tracker
  ;; Warroid Tracker
  (testing "Trashing Warroid starts trace"
    (do-game
      (new-game (default-corp ["Warroid Tracker"])
                (default-runner ["Corroder" "Dyson Mem Chip"]))
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Dyson Mem Chip")
      (let [war (get-content state :remote1 0)
            cor (get-program state 0)
            mem (get-hardware state 0)]
        (core/rez state :corp war)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
        (click-card state :runner cor)
        (click-card state :runner mem)
        (is (= 2 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards"))))
  (testing "Trashing from central triggers Warroid in root"
    ;; Regression test for #3725
    (do-game
      (new-game (default-corp ["Warroid Tracker" (qty "Hedge Fund" 3)])
                (default-runner ["Clan Vengeance" "Corroder" "Dyson Mem Chip"]))
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
        (core/rez state :corp war)
        (core/add-counter state :runner clv :power 2)
        (card-ability state :runner (refresh clv) 0)
        ;; Prompt choice checks there is a trace prompt from Warroid
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (-> (get-runner) :discard count)) "Runner should start with 1 card in heap (Clan Vengeance)")
        (click-card state :runner cor)
        (click-card state :runner mem)
        (is (= 3 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards (and CV already in heap)")
        (is (= 2 (count (:discard (get-corp)))) "Two cards trashed from HQ by Clan Vengeance")))))
