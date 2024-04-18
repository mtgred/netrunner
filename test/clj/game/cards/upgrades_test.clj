(ns game.cards.upgrades-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.eid :refer [make-eid]]
   [game.test-framework :refer :all]
   [game.utils :as utils]))

(deftest adrian-seis
  (do-game
    (new-game {:corp {:hand ["Adrian Seis" "Hedge Fund"]}})
    (play-from-hand state :corp "Adrian Seis" "R&D")
    (rez state :corp (get-content state :rd 0))
    (end-turn state :corp)
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "HQ")
    (is (= "Adrian Seis" (:title (get-content state :hq 0))))
    (start-turn state :runner)
    (run-empty-server state :hq)
    (click-prompt state :corp "0 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (click-prompt state :runner "No action")
    (is (no-prompt? state :runner) "Runner cannot access Adrian Seis")
    (run-empty-server state :hq)
    (click-prompt state :corp "0 [Credits]")
    (click-prompt state :runner "1 [Credits]")
    (click-prompt state :runner "Pay 2 [Credits] to trash")
    (is (no-prompt? state :runner) "Runner cannot access HQ")))

(deftest akitaro-watanabe
  (do-game
    (new-game {:corp {:hand ["Akitaro Watanabe" (qty "Fire Wall" 2)]
                      :credits 100}})
    (play-from-hand state :corp "Akitaro Watanabe" "HQ")
    (play-from-hand state :corp "Fire Wall" "HQ")
    (play-from-hand state :corp "Fire Wall" "R&D")
    (let [akitaro (get-content state :hq 0)
          fw-hq (get-ice state :hq 0)
          fw-rd (get-ice state :rd 0)]
      (rez state :corp akitaro)
      (is (changed? [(:credit (get-corp)) -3]
            (rez state :corp fw-hq))
          "Only spends 3 to rez Fire Wall protecting same server")
      (is (changed? [(:credit (get-corp)) -5]
            (rez state :corp fw-rd))
          "Spends full 5 to rez Fire Wall protecting another server"))))

(deftest amaze-amusements
  ;; AMAZE Amusements
  (do-game
     (new-game {:corp {:deck ["AMAZE Amusements" "Project Atlas"]}})
     (play-from-hand state :corp "AMAZE Amusements" "New remote")
     (rez state :corp (get-content state :remote1 0))
     (play-from-hand state :corp "Project Atlas" "Server 1")
     (take-credits state :corp)
     (run-empty-server state :remote1)
     (let [atlas (get-content state :remote1 1)]
       (click-card state :runner atlas)
       (click-prompt state :runner "Steal")
       (click-prompt state :runner "No action")
       (is (last-log-contains? state "give the Runner 2 tags"))
       (is (= 2 (count-tags state)) "Runner has 2 tags")))
  (testing "Basic test - trash"
    (do-game
     (new-game {:corp {:deck ["AMAZE Amusements" "Project Atlas"]}})
     (play-from-hand state :corp "AMAZE Amusements" "New remote")
     (rez state :corp (get-content state :remote1 0))
     (play-from-hand state :corp "Project Atlas" "Server 1")
     (take-credits state :corp)
     (run-empty-server state :remote1)
     (let [exchange (get-content state :remote1 0)]
       (click-card state :runner exchange)
       (click-prompt state :runner "Pay 3 [Credits] to trash")
       (click-prompt state :runner "Steal")
       (is (= 2 (count-tags state)) "Runner has 2 tags")))))

(deftest amazon-industrial-zone
  ;; Amazon Industrial Zone - Immediately rez ice installed over its server at 3 credit discount
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

(deftest angelique-garza-correa
  (do-game
    (new-game {:corp {:hand ["Angelique Garza Correa"]}
               :runner {:hand [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Angelique Garza Correa" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "No action")
    (rez state :corp (get-content state :remote1 0))
    (run-empty-server state "Server 1")
    (is (changed? [(:credit (get-corp)) -2]
          (click-prompt state :corp "Yes"))
        "Corp paid 2 credits")
    (is (= 2 (count (:discard (get-runner)))) "Runner got 2 damage")))

(deftest angelique-garza-correa-expend-ability
  (do-game
    (new-game {:corp {:hand ["Angelique Garza Correa" "City Works Project"]}
               :runner {:hand [(qty "Sure Gamble" 2)]}})
    (play-and-score state "City Works Project")
    (let [ang (first (:hand (get-corp)))]
      (is (changed? [(:credit (get-corp)) -1]
            (expend state :corp ang))
          "Corp paid 1 credit")
      (is (= 1 (count (:discard (get-runner)))) "Runner got 1 damage")
      (is (= 1 (count (:discard (get-corp)))) "Angelique Garza Correa got discarded"))))

(deftest anoetic-void
  ;; Anoetic Void
  (do-game
     (new-game {:corp {:hand ["Anoetic Void" "Ice Wall" "Fire Wall"]}})
     (play-from-hand state :corp "Anoetic Void" "New remote")
     (let [av (get-content state :remote1 0)]
       (rez state :corp av)
       (take-credits state :corp)
       (run-empty-server state "Server 1")
       (click-prompt state :corp "Yes")
       (click-card state :corp "Ice Wall")
       (click-card state :corp "Fire Wall")
       (is (not (:run @state)) "Run ended by Anoetic Void"))))

(deftest arella-salvatore-install-to-server
    ;; Install to server
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
        (is (no-prompt? state :corp) "Arella not triggered for different remote score")
        (is (= 1 (count (get-scored state :corp))) "1 Agenda scored")
        (score-agenda state :corp (refresh same-tg))
        (click-card state :corp (find-card "Bryan Stinson" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= 2 (count (get-scored state :corp))) "2 Agendas scored")
        (is (= 1 (count (get-content state :remote3))) "Bryan installed in new remote")
        (is (= 1 (get-counters (get-content state :remote3 0) :advancement)) "Bryan has 1 advancement counter"))))

(deftest arella-salvatore-interaction-w-other-on-scored-triggers
    ;; Interaction w/ other on-scored triggers
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

(deftest arella-salvatore-no-cost
    ;; No cost
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

(deftest arella-salvatore-overadvanced-vitruvius
    ;; Overadvanced Vitruvius
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
        (score state :corp (refresh vit)))
      (let [vit-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 1 agenda counter"))))

(deftest ash-2x3zb9cy-ash-2x3zb9cy
    ;; Ash 2X3ZB9CY
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

(deftest ash-2x3zb9cy-ash-dirty-laundry-interaction
    ;; Ash+Dirty Laundry interaction
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

(deftest ash-2x3zb9cy-installed-in-archives-5015
    ;; installed in archives #5015
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
        (is (no-prompt? state :runner) "Runner gets no further access prompts"))))

(deftest awakening-center-basic-operation
    ;; Basic Operation
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
          (run-on state "Server 1")
          (is (= "Rez and force the Runner to encounter a hosted piece of ice?" (:msg (prompt-map :corp))) "Awakening Center activates")
          (click-prompt state :corp "Yes")
          (is (= "Choose a hosted piece of Bioroid ice to rez" (:msg (prompt-map :corp))) "Choose a piece of ice to rez")
          (click-prompt state :corp "Fairchild")
          (is (rezzed? (refresh fc)) "Fairchild is rezzed")
          (is (= "Fairchild" (:title (core/get-current-ice state))) "Runner is encountering Fairchild")
          (encounter-continue state)
          (run-continue state)
          (click-prompt state :runner "Done")
          (is (not (:run @state)) "Run has ended")
          (is (= 1 (count (:discard (get-corp)))) "Fairchild in discard")
          (is (empty? (:hosted (refresh ac))) "Fairchild no longer hosted")))))

(deftest awakening-center-ddos-interaction
    ;; DDoS Interaction
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
          (run-on state "Server 1")
          (is (= "Rez and force the Runner to encounter a hosted piece of ice?" (:msg (prompt-map :corp))) "Awakening Center activates")
          (click-prompt state :corp "Yes")
          (is (= "Choose a hosted piece of Bioroid ice to rez" (:msg (prompt-map :corp))) "Awakening Center activates")
          (click-prompt state :corp "Fairchild")
          (is (rezzed? (refresh fc)) "Fairchild is rezzed")
          (is (= "Fairchild" (:title (core/get-current-ice state))) "Runner is encountering Fairchild")
          (encounter-continue state)
          (run-continue state)
          (click-prompt state :runner "Done")
          (is (not (:run @state)) "Run has ended")
          (is (= 1 (count (:discard (get-corp)))) "Fairchild in discard")
          (is (empty? (:hosted (refresh ac))) "Fairchild no longer hosted")))))

(deftest bamboo-dome
  ;; Bamboo Dome
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
        (is (= "Border Control" (:title (first (:hand (get-corp))))) "Border Control in hand"))))

(deftest ben-musashi
  ;; Ben Musashi
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
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest ben-musashi-on-r-d-access
    ;; on R&D access
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
        (click-prompt state :runner "No action")
        (run-empty-server state "R&D")
        (click-prompt state :runner "Card from deck")
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest ben-musashi-pay-even-when-trashed
    ;; pay even when trashed
    (do-game
      (new-game {:corp {:deck [(qty "Ben Musashi" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [bm (get-content state :remote1 0)]
        (rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner bm)
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; pay to trash
        ;; should now have prompt to pay 2 net for HoK
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest ben-musashi-check-runner-chooses-order-of-payment
    ;; Check runner chooses order of payment
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

(deftest ben-musashi-check-fetal-ai-can-be-stolen-2586
    ;; Check Fetal AI can be stolen, #2586
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
        (is (= 1 (count (:scored (get-runner)))) "Scored agenda"))))

(deftest bernice-mai
  ;; Bernice Mai
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

(deftest bernice-mai-interaction-with-dedicated-response-team
    ;; interaction with Dedicated Response Team
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
      (is (= 2 (count (:discard (get-runner)))) "Runner took 1 meat damage")))

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

(deftest black-level-clearance-taking-brain-damage
    ;; taking brain damage
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Black Level Clearance"]}})
      (play-from-hand state :corp "Black Level Clearance" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (changed? [(:credit (get-corp)) 0]
            (click-prompt state :runner "Take 1 core damage"))
          "Corp gains 0 credits")
      (is (get-run) "Run has ended")
      (is (get-content state :remote1) "Black Level Clearance has not been trashed")))

(deftest black-level-clearance-jack-out
    ;; Jack out
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Black Level Clearance"]}})
      (play-from-hand state :corp "Black Level Clearance" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (changed? [(:credit (get-corp)) 5]
            (click-prompt state :runner "Jack out"))
          "Corp gains 5 credits")
      (is (= ["Hedge Fund"] (map :title (:hand (get-corp)))) "Corp drew 1 card")
      (is (nil? (get-run)) "Run has ended")
      (is (empty? (get-content state :remote1)) "Black Level Clearance has been trashed")))

(deftest brasilia-government-grid
  (do-game
    (new-game {:corp {:hand ["Brasília Government Grid" "Rime" (qty "Vanilla" 2)]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Brasília Government Grid" "New remote")
    (play-from-hand state :corp "Rime" "Server 1")
    (play-from-hand state :corp "Vanilla" "Server 1")
    (play-from-hand state :corp "Vanilla" "HQ")
    (take-credits state :corp)
    (let [rime (get-ice state :remote1 0)
          van1 (get-ice state :remote1 1)
          van2 (get-ice state :hq 0)]
      (rez state :corp van2)
      (run-on state "Server 1")
      (rez state :corp (get-content state :remote1 0))
      (rez state :corp van1)
      (click-prompt state :corp "Yes")
      (is (changed? [(get-strength (refresh van1)) 3]
                    (click-card state :corp van2))
          "Vanilla protecting Server 1 got +3 strength")
      (is (not (rezzed? (refresh van2))) "Vanilla on HQ was derezzed")
      (run-continue state)
      (rez state :corp van2)
      (is (no-prompt? state :corp) "No prompt when rezzing ice protecting other servers")
      (run-continue state)
      (rez state :corp rime)
      (is (no-prompt? state :corp) "Brasília Government Grid ability is once per turn")
  )))

(deftest buneaker-bay-grid
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
        (is (= 4 (:credit (get-corp))) "Paid 0 credits to rez Strongbox")))))

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
      ; (is (not= "Death and Taxes" (:title (first (:discard (get-corp))))) "Death and Taxes not moved to trash")
      ; (take-credits state :runner)
      ; (core/lose state :runner :credit 3)
      ; (trash-from-hand state :corp "Paywall Implementation")
      ; (card-ability state :corp (refresh bs) 0)
      ; (click-prompt state :corp (find-card "Paywall Implementation" (:discard (get-corp))))
      ; (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall Implementation is active Current")
      ; (is (find-card "Interdiction" (:discard (get-runner))) "Interdiction is trashed")
      ; (trash-from-hand state :corp "IPO")
      ; (take-credits state :corp)
      ; (run-empty-server state "HQ")
      ; (click-prompt state :runner "Steal")
      ; (is (find-card "Paywall Implementation" (:rfg (get-corp))) "Paywall Implementation removed from game")
      ; (is (not= "Paywall Implementation" (:title (first (:discard (get-corp))))) "Paywall Implementation not moved to trash")
      ; (take-credits state :runner)
      ; (core/lose state :runner :credit 3)
      ; (card-ability state :corp (refresh bs) 0)
      ; (click-prompt state :corp (find-card "IPO" (:discard (get-corp))))
      ; (is (find-card "IPO" (:rfg (get-corp))) "IPO is removed from game")
      )))

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
      (is (no-prompt? state :corp) "Caprice not trigger on first ice")
      (run-continue-until state :movement) ; pass first Quandary
      (run-continue-until state :movement) ; Caprice should trigger here
      (is (prompt-is-card? state :corp caprice)
          "Corp has Caprice prompt (triggered automatically as runner passed last ice)")
      (is (prompt-is-card? state :runner caprice) "Runner has Caprice prompt")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (not (:run @state)) "Run ended by Caprice")
      (is (no-prompt? state :corp) "Caprice prompted cleared")
      ;; Check Caprice does not trigger on other servers
      (run-on state "HQ")
      (is (no-prompt? state :corp) "Caprice does not trigger on other servers"))))

(deftest cayambe-grid-advance-ability
    ;; Advance ability
    (testing "No ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid"]}})
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)]
          (rez state :corp cg))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (no-prompt? state :corp) "corp has no prompts when no ice is installed in this server")))
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
          (is (= "Place 1 advancement counter on an ice protecting HQ" (:msg (prompt-map :corp))) "Correct server in prompt title")
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
          (is (no-prompt? state :corp) "corp has no prompts when no ice is installed in this server")))))

(deftest cayambe-grid-payment-ability
    ;; Payment ability
    (testing "No ice"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Cayambe Grid"]}})
        (play-from-hand state :corp "Cayambe Grid" "HQ")
        (let [cg (get-content state :hq 0)]
          (rez state :corp cg)
          (take-credits state :corp)
          (run-empty-server state :hq)
          (let [credits (:credit (get-runner))]
            (is (= "Choose one" (:msg (prompt-map :runner))))
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
          (run-continue state)
          (let [credits (:credit (get-runner))]
            (is (= "Choose one" (:msg (prompt-map :runner))))
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
          (run-continue state)
          (let [credits (:credit (get-runner))]
            (is (= "Choose one" (:msg (prompt-map :runner))))
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
          (run-continue-until state :success)
          (let [credits (:credit (get-runner))]
            (is (= "Choose one" (:msg (prompt-map :runner))))
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
          (run-continue-until state :success)
          (let [credits (:credit (get-runner))]
            (is (= "Choose one" (:msg (prompt-map :runner))))
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
          (is (= (:msg (prompt-map :corp)) "Place 1 advancement counter on an ice protecting HQ")
              "Correct server in prompt title (HQ)")
          (click-card state :corp iw1)
          (is (= (:msg (prompt-map :corp)) "Place 1 advancement counter on an ice protecting Server 1")
              "Correct server in prompt title (Server 1)")
          (click-card state :corp iw2)))))

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
           i3 (get-ice state :hq 2)
           corr (get-program state 0)]
       (rez state :corp cr)
       (is (= 5 (:credit (get-corp))))
       (rez state :corp i3)
       (run-continue state)
       (auto-pump-and-break state corr)
       (core/continue state :corp nil)
       (is (= 2 (:position (get-in @state [:run]))) "Passed Ice Wall")
       (card-ability state :corp cr 0)
       (is (= 3 (:position (get-in @state [:run]))) "Runner approaching previous Ice Wall")
       (is (empty? (get-content state :hq))
           "Code Replicatior trashed from root of HQ")
       (click-prompt state :runner "No")
       (is (last-log-contains? state "Runner approaches Ice Wall protecting HQ at position 2") "Approach ice phase begins"))))

(deftest code-replicator-jack-out
    ;; jack out
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
           i3 (get-ice state :hq 2)
           corr (get-program state 0)]
       (rez state :corp cr)
       (is (= 5 (:credit (get-corp))))
       (rez state :corp i3)
       (run-continue state :encounter-ice)
       (auto-pump-and-break state corr)
       (core/continue state :corp nil)
       (is (= 2 (:position (get-in @state [:run]))) "Passed Ice Wall")
       (card-ability state :corp cr 0)
       (is (= 3 (:position (get-in @state [:run]))) "Runner approaching previous Ice Wall")
       (is (empty? (get-content state :hq))
           "Code Replicatior trashed from root of HQ")
       (click-prompt state :runner "Yes")
       (is (empty? (:run @state)) "Run has ended")
       (is (not (last-log-contains? state "Runner approaches Ice Wall protecting HQ at position 2")) "Run has ended so no approach"))))

(deftest cold-site-server-cost-modification-plays-nice-with-derez
    ;; Cost modification plays nice with derez
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
       (is (= 2 (:credit (get-runner))) "No extra cost to run HQ"))))

(deftest cold-site-server-run-event
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Cold Site Server"]}
               :runner {:deck ["Dirty Laundry"]}})
     (core/gain state :corp :credit 10 :click 10)
     (play-from-hand state :corp "Cold Site Server" "HQ")
     (let [css (get-content state :hq 0)]
       (rez state :corp (refresh css))
       (card-ability state :corp css 0))
     (take-credits state :corp)
     (play-from-hand state :runner "Dirty Laundry")
     (click-prompt state :runner "HQ")
     (is (second-last-log-contains? state "Runner spends [Click] and pays 2 [Credits] to play Dirty Laundry."))
     (is (last-log-contains? state "Runner spends [Click] and pays 1 [Credits] to make a run on HQ."))))

(deftest corporate-troubleshooter
  ;; Corporate Troubleshooter - Pay X credits and trash to add X strength to a piece of rezzed ice
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

(deftest crisium-grid-with-gauntlet-3082
    ;; with Gauntlet, #3082
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
      (auto-pump-and-break state (get-program state 0))
      (core/continue state :corp nil)
      (run-continue state)
      (is (not (no-prompt? state :runner)) "The Gauntlet has a prompt")))

(deftest crisium-grid-crisium-grid-prevents-first-successful-run-abilities-issue-5092
    ;; Crisium Grid prevents first successful run abilities. Issue #5092
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
     (is (= 2 (count (:discard (get-corp)))) "Archive has 2 cards (Crisium and Hedge Fund)")))

(deftest cyberdex-virus-suite-purge-ability
    ;; Purge ability
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

(deftest cyberdex-virus-suite-purge-on-access
    ;; Purge on access
    (do-game
      (new-game {:corp {:deck [(qty "Cyberdex Virus Suite" 3)]}
                 :runner {:deck ["Cache" "Medium"]}})
      (play-from-hand state :corp "Cyberdex Virus Suite" "New remote")
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state (refresh card)))
            cache (find-card "Cache" (get-program state))]
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

(deftest cyberdex-virus-suite-don-t-interrupt-archives-access-1647
    ;; Don't interrupt archives access, #1647
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
        (is (not (no-prompt? state :runner)) "CVS purge did not interrupt archives access")
        ;; purged counters
        (is (zero? (get-counters (refresh cache) :virus))
            "Cache has no counters"))))

(deftest daniela-jorge-inacio
  (do-game
      (new-game {:corp {:hand ["Daniela Jorge Inácio" "House of Knives"]}
                 :runner {:hand [(qty "Sure Gamble" 4)]
                         :deck ["Unity"]}})
      (play-from-hand state :corp "House of Knives" "New remote")
      (play-from-hand state :corp "Daniela Jorge Inácio" "Server 1")
      (rez state :corp (get-content state :remote1 1))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-card state :runner (get-content state :remote1 1))
      (is (changed? [(count (:hand (get-runner))) -2]
            (click-prompt state :runner "Pay 2 [Credits] to trash"))
          "2 random cards left the grip")
      (is (= 3 (count (:deck (get-runner)))) "3 cards in the stack")
      (is (changed? [(count (:hand (get-runner))) -2]
            (click-prompt state :runner "Pay to steal"))
          "2 random cards left the grip")
      (is (= 5 (count (:deck (get-runner)))) "5 cards in the stack")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda")))

(deftest daniela-jorge-inacio-on-central-servers
  (do-game
      (new-game {:corp {:hand ["Daniela Jorge Inácio" (qty "House of Knives" 2)]}
                 :runner {:hand [(qty "Sure Gamble" 5)]
                         :deck ["Unity"]}})
      (play-from-hand state :corp "Daniela Jorge Inácio" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Card from hand")
      (is (changed? [(count (:hand (get-runner))) -2]
            (click-prompt state :runner "Pay to steal"))
          "2 random cards left the grip")
      (is (= 3 (count (:deck (get-runner)))) "3 cards in the stack")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Daniela Jorge Inácio")
      (is (changed? [(count (:hand (get-runner))) -2]
            (click-prompt state :runner "Pay 2 [Credits] to trash"))
          "2 random cards left the grip")
      (click-prompt state :runner "No action" "Can't steal agendas")))

(deftest daruma-swapping-with-another-installed-card
    ;; swapping with another installed card
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daruma" "Hostile Takeover" "Snare!"]
                        :credits 10}})
      (play-from-hand state :corp "Daruma" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "Server 1")
      (play-from-hand state :corp "Snare!" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-card state :corp "Hostile Takeover")
      (click-card state :corp "Snare!")
      (is (find-card "Daruma" (:discard (get-corp))))
      (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to Jack out")
      (click-prompt state :runner "No")
      (is (= "Pay 4 [Credits] to use Snare! ability?" (:msg (prompt-map :corp))))))

(deftest daruma-works-only-on-runs-on-its-server
    (do-game
      (new-game {:corp {:hand ["Daruma"]}})
      (play-from-hand state :corp "Daruma" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :hq)
      (is (no-prompt? state :runner))))

(deftest daruma-swapping-with-a-card-in-hq
    ;; swapping with a card in HQ
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Daruma" "Hostile Takeover" "Snare!"]
                        :credits 10}})
      (play-from-hand state :corp "Daruma" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "Server 1")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-card state :corp "Hostile Takeover")
      (click-card state :corp "Snare!")
      (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to Jack out")
      (click-prompt state :runner "No")
      (is (= "Pay 4 [Credits] to use Snare! ability?" (:msg (prompt-map :corp))))))

(deftest daruma-runner-jacks-out
    ;; Runner jacks out
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                       :hand ["Daruma" "Hostile Takeover" "Snare!"]
                       :credits 10}})
     (play-from-hand state :corp "Daruma" "New remote")
     (play-from-hand state :corp "Hostile Takeover" "Server 1")
     (play-from-hand state :corp "Snare!" "New remote")
     (rez state :corp (get-content state :remote1 0))
     (take-credits state :corp)
     (run-empty-server state :remote1)
     (click-prompt state :corp "Yes")
     (click-card state :corp "Hostile Takeover")
     (click-card state :corp "Snare!")
     (is (find-card "Daruma" (:discard (get-corp))))
     (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to Jack out")
     (click-prompt state :runner "Yes")
     (is (not= "Pay 4 [Credits] to use Snare! ability?" (:msg (prompt-map :corp))))
     (is (not (:run @state)))))

(deftest dedicated-technician-team-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:corp {:hand ["Dedicated Technician Team" (qty "Enigma" 3)]}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Enigma" "New remote")
      (play-from-hand state :corp "Enigma" "Server 1")
      (play-from-hand state :corp "Dedicated Technician Team" "Server 1")
      (let [dtt (get-content state :remote1 0)]
        (rez state :corp dtt)
        (is (changed? [(:credit (get-corp)) 0]
              (play-from-hand state :corp "Enigma" "Server 1")
              (click-card state :corp (refresh dtt))
              (click-card state :corp (refresh dtt)))
            "Used 3 credits from Dedicated Technician Team")
        (is (zero? (get-counters (refresh dtt) :recurring)) "Took 2 credits from Dedicated Technician Team"))))

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
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (second (:discard (get-corp))))
      (is (= advance-tokens (count (filter #(= (:title %) "PAD Campaign") (:hand (get-corp))))) "2 advance tokens of DC so 2 PAD Campaign back to corp")
      (is (= "Defense Construct" (:title (first (:discard (get-corp)))))))) "Defense Construct is trashed after using it")

(deftest disposable-hq
  ;; Disposable HQ
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
      (is (no-prompt? state :corp) "Corp should be waiting on Runner")
      (is (no-prompt? state :runner) "Runner should be able to take actions")
      (is (= ["Ice Wall" "Fire Wall" "Hedge Fund" "Spiderweb"]
             (->> (get-corp) :deck (take 4) (map :title) (into [])))
          "Deck should be ordered top to bottom")))

(deftest disposable-hq-handles-eid-when-cancelled-issue-4912
    ;; Handles eid when cancelled. Issue #4912
    (do-game
      (new-game {:corp {:hand ["Disposable HQ" "Fire Wall" "Hedge Fund" "Spiderweb"]
                        :deck ["Ice Wall"]}})
      (play-from-hand state :corp "Disposable HQ" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (no-prompt? state :corp) "Corp should be waiting on Runner")
      (is (no-prompt? state :runner) "Runner should be able to take actions")))

(deftest djupstad-grid
  (do-game
    (new-game {:corp {:hand ["Project Atlas" "Djupstad Grid"] :credits 10}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Djupstad Grid" "New remote")
    (play-from-hand state :corp "Project Atlas" "Server 1")
    (rez state :corp (get-content state :remote1 0))
    (score-agenda state :corp (get-content state :remote1 1))
    (is (= 1 (:brain-damage (get-runner))) "Did 1 core damage")))

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
  ;; Embolus - 1 power counter to end the run, counters are lost on successful runs
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

(deftest experiential-data
  (do-game
    (new-game {:corp {:credits 7 :hand ["Experiential Data" "Ice Wall" "Enigma"]}})
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Experiential Data" "Server 1")
    (play-from-hand state :corp "Enigma" "Server 1")
    (take-credits state :runner)
    (let [iw (get-ice state :remote1 0)
          ed (get-content state :remote1 0)
          enigma (get-ice state :remote1 1)]
      (rez state :corp iw)
      (rez state :corp enigma)
      (is (= 1 (core/ice-strength state :corp iw)) "Initial strength of Ice Wall is 1")
      (is (= 2 (core/ice-strength state :corp enigma)) "Initial strength of Enigma is 2")
      (rez state :corp ed)
      (is (= 2 (core/ice-strength state :corp iw)) "Strength of Ice Wall after playing Experiential Data on the same server is now 2")
      (is (= 3 (core/ice-strength state :corp enigma)) "Strength of Enigma after playing Experiential Data on the same server is now 3")
      (trash state :corp ed)
      (is (= 1 (core/ice-strength state :corp iw)) "Strength of Ice Wall after trashing Experiential Data is back to 1")
      (is (= 2 (core/ice-strength state :corp enigma)) "Strength of Enigma after trashing Experiential Data is back to 2"))))

(deftest expo-grid
  (do-game
    (new-game {:corp {:hand ["Expo Grid" "Dedicated Response Team" "Breaking News"]}})
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (play-from-hand state :corp "Expo Grid" "Server 1")
    (let [drt (get-content state :remote1 0)
          expo (get-content state :remote1 1)]
      (rez state :corp drt)
      (rez state :corp expo)
      (take-credits state :corp)
      (let [total-corp-credits (:credit (get-corp))]
        (take-credits state :runner)
        (is (= (+ 1 total-corp-credits) (:credit (get-corp))) "Corp gains 1c")
        ;;Replace asset with agenda
        (play-from-hand state :corp "Breaking News" "Server 1")
        (click-prompt state :corp "OK")
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= (+ 3 total-corp-credits) (:credit (get-corp))) "Corp does not gain any extra c with agenda")))))

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

(deftest fractal-threat-matrix
  (do-game
    (new-game {:corp {:hand ["Fractal Threat Matrix" "Najja 1.0"]}
               :runner {:deck [(qty "Acacia" 7)]}})
    (play-from-hand state :corp "Fractal Threat Matrix" "New remote")
    (play-from-hand state :corp "Najja 1.0" "Server 1")
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
      (is (zero? (count (:deck (get-runner)))))
      (is (= 2 (count (:discard (get-runner))))))))

(deftest ganked-access-ability-forces-runner-to-encounter-ice
    ;; Access ability forces Runner to encounter ice
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                       :hand ["Ice Wall" "Ganked!"]}})
     (play-from-hand state :corp "Ice Wall" "HQ")
     (let [iw (get-ice state :hq 0)]
       (rez state :corp iw)
       (take-credits state :corp)
       (run-on state :hq)
       (run-continue state)
       (run-continue state)
       (run-continue state)
       (is (last-log-contains? state "Runner accesses Ganked!") "Ganked! message printed to log")
       (is (= "Trash Ganked! to force the Runner to encounter a piece of ice?"
              (:msg (prompt-map :corp))) "Corp has Ganked! prompt")
       (click-prompt state :corp "Yes")
       (is (= :select (prompt-type :corp)))
       (is (= :waiting (prompt-type :runner)))
       (click-card state :corp iw)
       (is (core/get-current-encounter state) "The runner should be encountering an ice")
       (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
       (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
       (is (empty? (remove :seen (:discard (get-corp)))) "Cards in Archives are faceup"))))

(deftest ganked-access-ability-only-works-for-ice-protecting-the-server-ganked-is-accessed-from
    ;; Access ability only works for ice protecting the server Ganked! is accessed from
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                       :hand ["Ice Wall" "Enigma" "Ganked!"]}})
     (play-from-hand state :corp "Ice Wall" "HQ")
     (play-from-hand state :corp "Enigma" "R&D")
     (let [iw (get-ice state :hq 0)
           enigma (get-ice state :rd 0)]
       (rez state :corp iw)
       (rez state :corp enigma)
       (take-credits state :corp)
       (run-on state :hq)
       (run-continue state)
       (run-continue state)
       (run-continue state)
       (is (last-log-contains? state "Runner accesses Ganked!") "Ganked! message printed to log")
       (is (= "Trash Ganked! to force the Runner to encounter a piece of ice?"
              (:msg (prompt-map :corp))) "Corp has Ganked! prompt")
       (click-prompt state :corp "Yes")
       (is (= :select (prompt-type :corp)))
       (is (= :waiting (prompt-type :runner)))
       (click-card state :corp enigma)
       (is (nil? (core/get-current-encounter state)) "The runner should not be encountering enigma")
       (click-card state :corp iw)
       (is (core/get-current-encounter state) "The runner should be encountering ice wall")
       (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall"))))

(deftest ganked-no-access-ability-when-there-are-no-rezzed-ice-protecting-the-server
    ;; No access ability when there are no rezzed ice protecting the server
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                       :hand ["Ice Wall" "Enigma" "Ganked!"]}})
     (play-from-hand state :corp "Ice Wall" "HQ")
     (play-from-hand state :corp "Enigma" "R&D")
     (rez state :corp (get-ice state :rd 0))
     (take-credits state :corp)
     (run-on state :hq)
     (run-continue state)
     (run-continue state)
     (is (last-log-contains? state "Runner accesses Ganked!") "Ganked! message printed to log")
     (is (accessing state "Ganked!") "Runner has normal access prompt")
     (click-prompt state :runner "No action")
     (is (not (get-run)) "Run has been ended")
     (is (no-prompt? state :corp) "No more prompts")
     (is (no-prompt? state :runner) "No more prompts")))

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

(deftest giordano-memorial-field-payable-with-net-mercur
    ;; Payable with net mercur
    (do-game
      (new-game {:corp {:deck ["Giordano Memorial Field" "Hostile Takeover"]}
                 :runner {:deck [(qty "Fan Site" 3) "Net Mercur"]}})
      (play-from-hand state :corp "Giordano Memorial Field" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Net Mercur")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (take-credits state :corp)
      (let [nm (get-resource state 0)]
        (core/command-counter state :runner '("c" "3"))
        (click-card state :runner nm)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 6 [Credits]")
        (click-card state :runner nm)
        (click-card state :runner nm)
        (click-card state :runner nm)
        (click-prompt state :runner "Place 1 [Credits] on Net Mercur")
        (click-prompt state :runner "No action"))))

(deftest giordano-memorial-field-ending-the-run-doesn-t-mark-the-run-as-unsuccessful-issue-4223
    ;; Ending the run doesn't mark the run as unsuccessful. Issue #4223
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
      (click-prompt state :runner "End the run")
      (is (zero? (count-tags state)) "Don't gain a tag from John Masanori")))

(deftest heinlein-grid
  (do-game
    (new-game {:corp {:credits 10 :hand ["Heinlein Grid" "Najja 1.0"]}})
    (play-from-hand state :corp "Heinlein Grid" "New remote")
    (play-from-hand state :corp "Najja 1.0" "Server 1")
    (let [hg (get-content state :remote1 0)
          najja (get-ice state :remote1 0)]
      (rez state :corp hg)
      (take-credits state :corp)
      (run-on state "Server 1")
      (rez state :corp najja)
      (run-continue state)
      (card-side-ability state :runner najja 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Done")
      (card-ability state :corp (refresh hg) 0)
      (is (= 2 (:click (get-runner))) "Runner spent 1 click on the run and 1 more to break 1st sub")
      (is (zero? (:credit (get-runner) "Heinlein Grid did the runner loose all credits"))))))

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
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (not (:run @state)))
      (is (= 6 (get-strength (refresh gutenberg))))
      (is (zero? (get-strength (refresh vanilla)))))))

(deftest henry-phillips-basic-behavior
    ;; Basic behavior
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
        (auto-pump-and-break state pc)
        (core/continue state :corp nil)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (= corp-creds (:credit (get-corp))) "Henry doesn't gain credits if runner not tagged")
        (gain-tags state :runner 1)
        (run-on state :remote1)
        (run-continue state)
        (auto-pump-and-break state pc)
        (core/continue state :corp nil)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (= (+ 4 corp-creds) (:credit (get-corp))) "Henry gains 4 credits if runner tagged and breaks two subs"))))

(deftest hired-help-normal-usage
    ;; Normal usage
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
       (is (no-prompt? state :runner) "No Hired Help prompt"))))

(deftest hired-help-crisium-grid-fake-agenda-interactions
    ;; Crisium Grid, fake agenda interactions
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
           "Runner prompt is on Hired Help despite HQ run because of Crisium"))))

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
    (click-prompt state :runner "Take 1 tag")
    (is (= 1 (count-tags state)) "Runner takes 1 tag to prevent Corp from removing 1 BP")
    (click-prompt state :runner "Pay 2 [Credits] to trash") ; trash
    (run-empty-server state "Archives")
    (is (= 1 (count-bad-pub state)))
    (click-prompt state :runner "The Corp removes 1 bad publicity")
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
    (is (no-prompt? state :corp) "No prompt from Archives access")
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
    (is (no-prompt? state :corp) "Prompt closes after done")
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (run-empty-server state "HQ")
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "5")
    (is (no-prompt? state :corp) "Prompt closes after lost trace")))

(deftest isaac-liberdade
  (do-game
    (new-game {:corp {:hand ["Isaac Liberdade" "Ice Wall" "Tithe"]
                      :credits 10}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Isaac Liberdade" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (play-from-hand state :corp "Tithe" "R&D")
    (let [il (get-content state :remote1 0)
          iw (get-ice state :remote1 0)
          tithe (get-ice state :rd 0)]
      (rez state :corp iw)
      (rez state :corp tithe)
      (click-advance state :corp iw)
      (is (changed? [(get-strength (refresh iw)) 2]
                    (rez state :corp il))
          "Ice Wall got +2 strength")
      (take-credits state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "R&D")
      (is (not (no-prompt? state :corp)) "Corp has Isaac Liberdade prompt")
      (is (changed? [(get-strength (refresh tithe)) 2
                     (get-counters (refresh tithe) :advancement) 1]
                    (click-card state :corp tithe))
          "Tithe got +2 strength and 1 advancement counter")
      (is (no-prompt? state :runner) "No lingering prompt"))))

(deftest jinja-city-grid-single-draws
    ;; Single draws
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
        (is (= (inc n) (count (get-in @state [:corp :servers :remote1 :ices]))) (str n " pieces of ice protecting Remote1")))
      (click-draw state :corp)
      (click-prompt state :corp (first (prompt-buttons :corp)))
      (is (= 3 (:credit (get-corp))) "Charged to install ice")
      (is (= 6 (count (get-in @state [:corp :servers :remote1 :ices]))) "6 pieces of ice protecting Remote1")))

(deftest jinja-city-grid-drawing-non-ice
    ;; Drawing non-ice cards shows bogus prompt to the Runner
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Jinja City Grid"]}
                 :runner {:id "Laramy Fisk: Savvy Investor"
                          :deck ["Eden Shard"]}})
      (play-from-hand state :corp "Jinja City Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (click-draw state :corp)
      (is (= :waiting (prompt-type :runner)) "Runner has wait prompt")
      (is (= :bogus (prompt-type :corp)) "Corp has a bogus prompt to fake out the runner")
      (click-prompt state :corp "Carry on!")
      (take-credits state :corp)
      (run-empty-server state :rd)
      (is (= "Force the Corp to draw 1 card?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= :waiting (prompt-type :runner)) "Runner has wait prompt")
      (is (= :bogus (prompt-type :corp)) "Corp has a bogus prompt to fake out the runner")
      (click-prompt state :corp "Carry on!")))

(deftest k-p-lynn
  (before-each [state (new-game {:corp {:hand ["K. P. Lynn" "Ice Wall"]
                                        :credits 10}})
                _ (do (play-from-hand state :corp "K. P. Lynn" "New remote")
                      (rez state :corp (get-content state :remote1 0))
                      (play-from-hand state :corp "Ice Wall" "Server 1")
                      (take-credits state :corp)
                      (run-on state :remote1)
                      (run-continue state))]
  (testing "take the tag"
    (do-game state
      (click-prompt state :runner "Take 1 tag")
      (is (is-tagged? state))))
  (testing "end the run"
    (do-game state
      (click-prompt state :runner "End the run")
      (is (nil? (get-run)))))))

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

(deftest khondi-plaza-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:corp {:hand ["Khondi Plaza" "Ice Wall" "Enigma" (qty "PAD Campaign" 3)]}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Khondi Plaza" "New remote")
      (play-from-hand state :corp "Enigma" "Server 1")
      (dotimes [_ 3] (play-from-hand state :corp "PAD Campaign" "New remote"))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [kh (get-content state :remote1 0)
            en (get-ice state :remote1 0)]
        (rez state :corp kh)
        (is (= 4 (get-counters (refresh kh) :recurring)) "4 recurring credits on Khondi")
        (is (changed? [(:credit (get-corp)) 0]
              (rez state :corp en {:expect-rez false})
              (dotimes [_ 3] (click-card state :corp kh)))
            "Used 3 credits from Khondi Plaza"))))

(deftest la-costa-grid-la-costa-grid-cannot-be-installed-in-a-central-server
    ;; La Costa Grid cannot be installed in a central server
    (do-game
      (new-game {:corp {:hand ["La Costa Grid"]}})
      (play-from-hand state :corp "La Costa Grid")
      (is (not (some #{"HQ" "R&D" "Archives"} (prompt-buttons :corp)))
          "Central servers are not listed in the install prompt")))

(deftest la-costa-grid-at-the-start-of-their-turn-the-corp-may-place-an-advancement-token-on-a-card-in-la-costa-grid-s-server
    ;; At the start of their turn The Corp may place an advancement token on a card in La Costa Grid's server
    (do-game
      (new-game {:corp {:hand ["La Costa Grid", "Breaking News"]}})
      (play-from-hand state :corp "La Costa Grid" "New remote")
      (play-from-hand state :corp "Breaking News" "Server 1")
      (let [[la-costa breaking-news] (get-content state :remote1)]
        (rez state :corp la-costa)
        (take-credits state :corp)
        (take-credits state :runner)
        (end-phase-12 state :corp)
        (is (not (no-prompt? state :corp)) "The Corp is prompted to place one advancement token on a card")
        (click-card state :corp la-costa)
        (is (= 1 (get-counters (refresh la-costa) :advancement)) "Clicking on La Costa Grid advances itself")
        (take-credits state :corp)
        (take-credits state :runner)
        (end-phase-12 state :corp)
        (click-card state :corp breaking-news)
        (is (= 1 (get-counters (refresh breaking-news) :advancement)) "Clicking on a card in La Costa Grid's server advances it"))))

(deftest la-costa-grid-the-corp-may-not-advance-cards-which-are-not-in-la-costa-grid-s-server
    ;; The Corp may not advance cards which are not in La Costa Grid's server
    (do-game
      (new-game {:corp {:hand ["La Costa Grid", (qty "Mumbad Virtual Tour" 2), (qty "Vanilla" 3)]}})
      (play-from-hand state :corp "La Costa Grid" "New remote")
      (let [[la-costa] (get-content state :remote1)]
        (rez state :corp la-costa)
        (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
        (let [[remote-mvt] (get-content state :remote2)]
          (take-credits state :corp)
          (take-credits state :runner)
          (end-phase-12 state :corp)
          (click-card state :corp remote-mvt)
          (is (not (no-prompt? state :corp)) "Clicking a card in a different remote does not clear the prompt")
          (is (zero? (get-counters (refresh remote-mvt) :advancement)) "Clicking a card in a different remote does not advance it"))
        (click-prompt state :corp "Done")
        (play-from-hand state :corp "Mumbad Virtual Tour" "HQ")
        (let [[central-mvt] (get-content state :hq)]
          (take-credits state :corp)
          (take-credits state :runner)
          (end-phase-12 state :corp)
          (click-card state :corp central-mvt)
          (is (not (no-prompt? state :corp)) "Clicking a card in a central does not clear the prompt")
          (is (zero? (get-counters (refresh central-mvt) :advancement)) "Clicking a card in a central does not advance it"))
        (click-prompt state :corp "Done")
        (play-from-hand state :corp "Vanilla" "Server 1")
        (let [[vanilla] (get-ice state :remote1)]
          (take-credits state :corp)
          (take-credits state :runner)
          (end-phase-12 state :corp)
          (click-card state :corp vanilla)
          (is (not (no-prompt? state :corp)) "Clicking an ice protecting La Costa Grid does not clear the prompt")
          (is (zero? (get-counters (refresh vanilla) :advancement)) "Clicking a an ice protecting La Costa Grid does not advance it"))
        (click-prompt state :corp "Done")
        (play-from-hand state :corp "Vanilla" "Server 2")
        (let [[remote-vanilla] (get-ice state :remote2)]
          (take-credits state :corp)
          (take-credits state :runner)
          (end-phase-12 state :corp)
          (click-card state :corp remote-vanilla)
          (is (not (no-prompt? state :corp)) "Clicking an ice protecting La Costa Grid does not clear the prompt")
          (is (zero? (get-counters (refresh remote-vanilla) :advancement)) "Clicking a an ice protecting La Costa Grid does not advance it"))
        (click-prompt state :corp "Done")
        (play-from-hand state :corp "Vanilla" "HQ")
        (let [[central-vanilla] (get-ice state :hq)]
          (take-credits state :corp)
          (take-credits state :runner)
          (end-phase-12 state :corp)
          (click-card state :corp central-vanilla)
          (is (not (no-prompt? state :corp)) "Clicking an ice protecting HQ does not clear the prompt")
          (is (zero? (get-counters (refresh central-vanilla) :advancement)) "Clicking a an ice protecting HQ does not advance it")))))

(deftest la-costa-grid-the-corp-may-advance-hosted-cards-in-la-costa-grid-s-server
    ;; The Corp may advance hosted cards in La Costa Grid's server
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
          (end-phase-12 state :corp)
          (click-card state :corp beale)
          (is (= 1 (get-counters (refresh beale) :advancement)) "Clicking on a hosted card in the La Costa Grid server advances it")))))

(deftest la-costa-grid-properly-async-5049
    ;; Properly async. #5049
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
      (end-phase-12 state :corp)
      (is (= "Choose a card in Server 2" (:msg (prompt-map :corp))))
      (click-card state :corp "Project Beale")
      (is (last-n-log-contains? state 2 "La Costa Grid to place 1 advancement counter on a card in Server 2"))
      (is (= "Choose 1 card to add to the bottom of R&D" (:msg (prompt-map :corp))))
      (click-card state :corp "Ice Wall")
      (is (last-log-contains? state "Daily Business Show to add"))
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))))

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
       (auto-pump-and-break state cor)
       (core/continue state :corp nil)
       (run-continue-until state :success)
       (click-prompt state :corp "0 [Credits]")
       (click-prompt state :runner "1 [Credits]")
       (is (zero? (:position (:run @state))) "Runner should be approaching the server")
       (click-prompt state :corp "Yes")
       (is (= 2 (:position (:run @state))) "Runner should be approaching outermost ice")
       (is (nil? (refresh letheia)) "Letheia is trashed")
       (is (find-card "Letheia Nisei" (:discard (get-corp))) "Letheia is in Archives")
       (click-prompt state :runner "No")
       (is (last-log-contains? state "Runner approaches Vanilla protecting R&D at position 1") "Approach ice phase begins"))))

(deftest letheia-nisei-jack-out
    ;; jack out
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
       (auto-pump-and-break state cor)
       (core/continue state :corp nil)
       (run-continue-until state :success)
       (click-prompt state :corp "0 [Credits]")
       (click-prompt state :runner "1 [Credits]")
       (is (zero? (:position (:run @state))) "Runner should be approaching the server")
       (click-prompt state :corp "Yes")
       (is (= 2 (:position (:run @state))) "Runner should be approaching outermost ice")
       (is (nil? (refresh letheia)) "Letheia is trashed")
       (is (find-card "Letheia Nisei" (:discard (get-corp))) "Letheia is in Archives")
       (click-prompt state :runner "Yes")
       (is (empty? (:run @state)))
       (is (not (last-log-contains? state "Runner approaches Vanilla protecting R&D at position 1")) "Run has ended"))))

(deftest malapert-data-vault
  ;; Malapert Data Vault
  (do-game
    (new-game {:corp {:deck ["Quandary" "Project Atlas" "Government Takeover" "Hostile Takeover"]
                      :hand ["Project Atlas" "Malapert Data Vault"]
                      :click 10}})
    (play-from-hand state :corp "Malapert Data Vault" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Project Atlas" "Server 1")
    (let [atlas (get-content state :remote1 1)]
      (score-agenda state :corp (refresh atlas))
      (click-prompt state :corp "Yes")
      (is (= 2 (count (prompt-buttons :corp))) "Corp should have prompt back with 2 options (Quandry and Cancel)")
      (is (= ["Quandary" "Cancel"] (map #(or (:title %) %) (prompt-buttons :corp))))
      (is (changed? [(count (:hand (get-corp))) 1]
            (click-prompt state :corp "Quandary"))
          "Clicking prompt causes Quandary to move to HQ"))))

(deftest manegarm-skunkworks
  ;; Manegarm Skunkworks
  (do-game
      (new-game {:corp {:hand ["Manegarm Skunkworks"]}})
      (play-from-hand state :corp "Manegarm Skunkworks" "New remote")
      (let [encryption (get-content state :remote1 0)]
        (rez state :corp encryption)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (changed? [(:click (get-runner)) -2]
              (click-prompt state :runner "Spend [Click][Click]"))
            "Spend 2 clicks")
        (is (:run @state) "Run not ended by Manegarm Skunkworks")))
  (testing "Basic test - credits"
    (do-game
      (new-game {:corp {:hand ["Manegarm Skunkworks"]}})
      (play-from-hand state :corp "Manegarm Skunkworks" "New remote")
      (let [encryption (get-content state :remote1 0)]
        (rez state :corp encryption)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (changed? [(:credit (get-runner)) -5]
              (click-prompt state :runner "Pay 5 [Credits]"))
            "Pay 5 credits")
        (is (:run @state) "Run not ended by Manegarm Skunkworks"))))
  (testing "Basic test - ETR"
    (do-game
      (new-game {:corp {:hand ["Manegarm Skunkworks"]}})
      (play-from-hand state :corp "Manegarm Skunkworks" "New remote")
      (let [encryption (get-content state :remote1 0)]
        (rez state :corp encryption)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "End the run")
        (is (not (:run @state)) "Run ended by Manegarm Skunkworks")))))

(deftest manegarm-skunkworks-no-prompt-for-runs-on-other-servers
    ;; No prompt for runs on other servers
    (do-game
      (new-game {:corp {:hand ["Manegarm Skunkworks"]}})
      (play-from-hand state :corp "Manegarm Skunkworks" "New remote")
      (let [encryption (get-content state :remote1 0)]
        (rez state :corp encryption)
        (take-credits state :corp)
        (run-empty-server state "HQ")
        (is (no-prompt? state :runner) "Manegarm Skunkworks didn't trigger"))))

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
        (is (nil? (refresh mb)) "Marcus Batty is trashed"))))

(deftest mason-bellamy
  (do-game
    (new-game {:corp {:hand ["Mason Bellamy" "Cobra"]
                      :deck [(qty "Hedge Fund" 5)]}
               :runner {:credits 10 :hand [(qty "Garrote" 5)]}})
    (play-from-hand state :corp "Mason Bellamy" "HQ")
    (play-from-hand state :corp "Cobra" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Garrote")
    (let [mb (get-content state :hq 0)
          cobra (get-ice state :hq 0)
          garrote (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp mb)
      (rez state :corp cobra)
      (run-continue state)
      (card-ability state :runner (refresh garrote) 0)
      (click-prompt state :runner "Trash a program")
      (click-prompt state :runner "Done")
      (fire-subs state cobra)
      (run-continue state)
      (is (:run @state) "Run is not over")
      ;; 1 click to play program, 1 for run, 1 of Mason
      (is (= 1 (:click (get-runner)))))))

(deftest mavirus-purge-ability
    ;; Purge ability
    (do-game
      (new-game {:corp {:deck [(qty "Mavirus" 3)]}
                 :runner {:hand ["Cache" "Medium" "Sure Gamble" "Sure Gamble"]}})
      (play-from-hand state :corp "Mavirus" "HQ")
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state (refresh card)))
            cache (find-card "Cache" (get-program state))
            mav (get-content state :hq 0)]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :runner "Medium")
        (take-credits state :runner 2)
        (rez state :corp mav)
        (card-ability state :corp mav 0)
        ;; nothing in hq content
        (is (empty? (get-content state :hq)) "CVS was trashed")
        ;; no cards trashed from grip
        (is (= 2 (count (:hand (get-runner)))) "No cards trashed by Mavirus")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-program state))))
            "Medium has no counters"))))

(deftest mavirus-purge-on-access-rezzed
    ;; Purge on access
    (do-game
      (new-game {:corp {:deck [(qty "Mavirus" 3)]}
                 :runner {:hand ["Cache" "Medium" "Sure Gamble" "Sure Gamble"]}})
      (play-from-hand state :corp "Mavirus" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state (refresh card)))
            cache (find-card "Cache" (get-program state))]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :runner "Medium")
        (run-empty-server state "Server 1")
        ;; corp now has optional prompt to trigger virus purge
        (is (= 2 (count (:hand (get-runner)))) "No damage dealt yet")
        (click-prompt state :corp "Yes")
        (is (= 1 (count (:hand (get-runner)))) "1 damage dealt with Mavirus")
        ;; runner has prompt to trash Mavirus
        (click-prompt state :runner "Pay 0 [Credits] to trash")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-program state))))
            "Medium has no counters"))))

(deftest mavirus-purge-on-access-unrezzed
    ;; Purge on access
    (do-game
      (new-game {:corp {:deck [(qty "Mavirus" 3)]}
                 :runner {:hand ["Cache" "Medium" "Sure Gamble" "Sure Gamble"]}})
      (play-from-hand state :corp "Mavirus" "New remote")
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state (refresh card)))
            cache (find-card "Cache" (get-program state))]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :runner "Medium")
        (run-empty-server state "Server 1")
        ;; corp now has optional prompt to trigger virus purge
        (is (= 2 (count (:hand (get-runner)))) "No damage dealt yet")
        (click-prompt state :corp "Yes")
        (is (= 2 (count (:hand (get-runner)))) "No damage dealt (mav unrezzed)")
        ;; runner has prompt to trash Mavirus
        (click-prompt state :runner "Pay 0 [Credits] to trash")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-program state))))
            "Medium has no counters"))))

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
         "Ice Wall has been added to hand")
     (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to Jack out")
     (click-prompt state :runner "No")))

(deftest midori-jack-out
    ;; jack out
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
         "Ice Wall has been added to hand")
     (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to Jack out")
     (is (prompt-is-type? state :corp :waiting) "Corp waiting for Runner to jack out")
     (click-prompt state :runner "Yes")
     (is (empty? (:run @state)))
     (is (no-prompt? state :runner) "No open runner prompts")
     (is (no-prompt? state :corp)) "No open corp prompts"))

(deftest midway-station-grid-addtional-cost-on-single-sub-break
    ;; Addtional cost on single sub break
    (do-game (new-game {:corp  {:deck [(qty "Hedge Fund" 5)]
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
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner corroder 1))
          "Runner loses 1 credit only for boosting strength")
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner corroder 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "Done"))
          "Runner should lose 2 credits, 1 for Midway Station, 1 for base ability")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "Server 1")
      (rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner corroder 0)
            (click-prompt state :runner "End the run"))
          "Runner loses 1 credit only for running on a different server"))))

(deftest midway-station-grid-addtional-cost-when-breaking-all
    ;; Addtional cost when breaking all
    (do-game (new-game {:corp {:hand ["Midway Station Grid" "Quandary"] :credits 20} :runner {:hand ["Cradle"] :credits 20} })
      (play-from-hand state :corp "Midway Station Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (play-from-hand state :corp "Quandary" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cradle")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
            (card-ability state :runner (get-program state 0) 0)
            (click-prompt state :runner "End the run"))
          "Runner loses 3 credits, 2 for cradle 1 for midway")))

(deftest mr-hendrik-corp-declines
  ;; Pay 2: runner suffers core or loses all (at least 1) clicks
  (do-game
    (new-game {:corp {:hand ["Mr. Hendrik"] :credits 20}})
    (play-from-hand state :corp "Mr. Hendrik" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (changed? [(:credit (get-corp)) 0]
          (click-prompt state :corp "No"))
        "spent 0 credits declining hendrik")
    (is (= 0 (:brain-damage (get-runner))) "Did 0 core damage")
    (is (= 3 (:click (get-runner))))
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest mr-hendrik-take-damage
  ;; Pay 2: runner suffers core or loses all (at least 1) clicks
  (do-game
    (new-game {:corp {:hand ["Mr. Hendrik"] :credits 20}})
    (play-from-hand state :corp "Mr. Hendrik" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (changed? [(:credit (get-corp)) -2]
          (click-prompt state :corp "Yes"))
        "spent 2 credits on hendrik")
    (is (= 3 (:click (get-runner))))
    (click-prompt state :runner "Suffer 1 core damage")
    (is (= 1 (:brain-damage (get-runner))) "Did 1 core damage")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest mr-hendrik-decline-damage
  ;; Pay 2: runner suffers core or loses all (at least 1) clicks
  (do-game
    (new-game {:corp {:hand ["Mr. Hendrik"] :credits 20}})
    (play-from-hand state :corp "Mr. Hendrik" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (changed? [(:credit (get-corp)) -2]
          (click-prompt state :corp "Yes"))
        "spent 2 credits on hendrik")
    (is (= 3 (:click (get-runner))))
    (click-prompt state :runner "Lose all remaining [Click]")
    (is (= 0 (:brain-damage (get-runner))) "Did 0 core damage")
    (is (= 0 (:click (get-runner))) "lost remaining clicks")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest mr-hendrik-no-clicks-remaining
  ;; Pay 2: runner suffers core or loses all (at least 1) clicks
  (do-game
    (new-game {:corp {:hand ["Mr. Hendrik"] :credits 20}})
    (play-from-hand state :corp "Mr. Hendrik" "New remote")
    (take-credits state :corp)
    (core/lose state :runner :click 3)
    (run-empty-server state "Server 1")
    (is (= 0 (:click (get-runner))))
    (is (changed? [(:credit (get-corp)) -2]
          (click-prompt state :corp "Yes"))
        "spent nothing declining hendrik")
    (click-prompt state :runner "Suffer 1 core damage")
    (is (= 1 (:brain-damage (get-runner))) "Did 1 core damage")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest mumbad-city-grid-1-ice
    ;; 1 ice
    (do-game
      (new-game {:corp {:deck ["Mumbad City Grid" "Quandary"]}})
      (play-from-hand state :corp "Mumbad City Grid" "New remote")
      (play-from-hand state :corp "Quandary" "Server 1")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state "Server 1")
      (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "1 ice on server")
      (run-continue state)
      (is (no-prompt? state :corp))
      (run-jack-out state)
      (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "Still 1 ice on server")))

(deftest mumbad-city-grid-fire-before-pass
    ;; fire before pass
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
      (is (no-prompt? state :corp) "Mumbad doesn't trigger on other servers")))

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
    (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed")))

(deftest mumbad-virtual-tour-interaction-with-imp
    ;; interaction with Imp
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
      (is (= 2 (count (prompt-titles :runner))) "Runner has 2 choices when Imp is installed")
      (is (= 5 (:credit (get-runner))) "Runner not forced to trash MVT when Imp installed")
      (is (empty? (:discard (get-corp))) "MVT is not force-trashed when Imp installed")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed with Imp")))

(deftest mumbad-virtual-tour-interactions-with-imp-and-various-amounts-of-money
    ;; interactions with Imp and various amounts of money
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
             (into #{} (prompt-titles :runner))) "Should have Imp and MVT options")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 2")
      (is (= ["[Imp] Hosted virus counter: Trash card"] (prompt-titles :runner)) "Should only have Imp option")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 3")
      (is (= ["No action"] (prompt-titles :runner)) "Should only have no action option")
      (click-prompt state :runner "No action")
      (is (= 2 (->> (get-corp) :discard count)) "Runner was not forced to trash MVT")))

(deftest mumbad-virtual-tour-not-forced-to-trash-when-credits-below-5
    ;; not forced to trash when credits below 5
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 3)]}
                 :runner {:deck ["Daily Casts"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Daily Casts")
      (is (= 2 (:credit (get-runner))) "Runner paid install costs")
      (run-empty-server state "Server 1")
      (is (= ["No action"] (prompt-buttons :runner)) "Runner is not given the choice")))

(deftest mumbad-virtual-tour-forced-to-trash-when-playing-as-khumalo
    ;; forced to trash when playing as Khumalo
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 3)]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Daily Casts"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Daily Casts")
      (is (= 2 (:credit (get-runner))) "Runner paid install costs")
      (run-empty-server state "Server 1")
      (is (= ["[Freedom Khumalo] Trash card"] (prompt-titles :runner)) "Runner is not given the choice")))

(deftest mumbad-virtual-tour-forced-to-trash-after-playing-demolition-run
    ;; forced to trash after playing Demolition Run
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
      (is (= ["[Demolition Run] Trash card"] (prompt-titles :runner)) "Runner is not given the choice")))

(deftest mumbad-virtual-tour-not-to-trash-after-installing-salsette-slums
    ;; not to trash after installing Salsette Slums
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
      (is (= ["Pay 5 [Credits] to trash"] (prompt-titles :runner)) "Runner is not given the choice")))

(deftest mwanza-city-grid
  ;; Mwanza City Grid - runner accesses 3 additional cards, gain 2C for each card accessed
  (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Mwanza City Grid")
      (is (= ["R&D" "HQ"] (prompt-titles :corp)) "Mwanza can only be installed in root of HQ or R&D")
      (click-prompt state :corp "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (rez state :corp mcg)
        (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
        (run-continue state)
        (click-prompt state :runner "Mwanza City Grid")
        (click-prompt state :runner "No action")
        (dotimes [_ 4]
          (click-prompt state :runner "No action"))
        (is (no-prompt? state :runner) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))

(deftest mwanza-city-grid-effect-persists-through-current-run-after-trash
    ;; effect persists through current run after trash
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
        (dotimes [_ 4]
          (click-prompt state :runner "No action"))
        (is (no-prompt? state :runner) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-corp))) "Corp gains 10 credits"))))

(deftest mwanza-city-grid-works-well-with-replacement-effects
    ;; Regression test for #3456
    ;; works well with replacement effects
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

(deftest mwanza-city-grid-multiple-breaches-correctness
  ;; Test that mwanza acts correctly when we breach multiple times in a run
  (do-game
   (new-game {:corp {:hand ["Mwanza City Grid" "Shiro"]
                     :deck ["Advanced Assembly Lines" "Biotic Labor" "Caduceus"
                            "Death and Taxes" "Economic Warfare"]}
              :runner {:hand ["Kongamato"]}})
   (play-from-hand state :corp "Mwanza City Grid" "R&D")
   (play-from-hand state :corp "Shiro" "R&D")
   (take-credits state :corp)
   (play-from-hand state :runner "Kongamato")
   (rez state :corp (get-content state :rd 0))
   (let [shiro (get-ice state :rd 0)]
     (rez state :corp shiro)
     (run-on state "R&D")
     (run-continue state)
     (card-ability state :runner (get-resource state 0) 0)
     (fire-subs state (refresh shiro))
     (click-prompt state :corp "No")
     ;;first access - we should only see 4 cards
     (is (:breach @state) "Currently breaching")
     (click-prompt state :runner "No action")
     (click-prompt state :runner "No action")
     (click-prompt state :runner "No action")
     (is (changed? [(:credit (get-corp)) +8]
           (click-prompt state :runner "No action")
           (is (not (:breach @state)) "Not currently breaching"))
         "Gained 6c from Mwanza City Grid")
     ;; continue until access
     (run-continue state :movement)
     (run-continue state :success)
     (is (:breach @state) "Currently breaching (for real)")
     (click-prompt state :runner "Mwanza City Grid")
     (click-prompt state :runner "No action")
     ;; plus four cards from deck
     (click-prompt state :runner "No action")
     (click-prompt state :runner "No action")
     (click-prompt state :runner "No action")
     (is (changed? [(:credit (get-corp)) +10]
           (click-prompt state :runner "No action")
           (is (not (:breach @state)) "Not currently breaching"))
         "Five cards accessed, +10 credits"))))

(deftest mwanza-city-grid-salsette-slums
  (do-game
   (new-game {:corp {:hand ["Mwanza City Grid"]}
              :runner {:hand ["Salsette Slums"] :credits 10}})
   (play-from-hand state :corp "Mwanza City Grid" "HQ")
   (rez state :corp (get-content state :hq 0))
   (take-credits state :corp)
   (play-from-hand state :runner "Salsette Slums")
   (run-on state "HQ")
   (run-continue state)
   (is (changed? [(:credit (get-corp)) +2]
         (click-prompt state :runner "[Salsette Slums] Remove card from game"))
       "Corp gained +2, even after mwanza was RFG'd")))

(deftest mwanza-city-grid-interaction-with-kitsune
    ;; Regression test for #3469
    ;; interaction with Kitsune
    (do-game
      (new-game {:corp {:deck ["Mwanza City Grid" "Breached Dome"
                               (qty "Kitsune" 2) (qty "Hedge Fund" 3)]}})
      (draw state :corp 1) ; Draw last card of deck
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
        (dotimes [_ 3]
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
        (dotimes [_ 3]
          (click-prompt state :runner "No action"))
        (run-jack-out state)
        (is (= 2 (-> (get-corp) :discard count)) "Two Kitsunes trashed after resolving their subroutines"))))

(deftest nanisivik-grid
  ;; Nanisivik Grid
  (do-game
      (new-game {:corp {:hand ["Nanisivik Grid" "Ice Wall"]
                        :discard ["Anemone"]}
                 :runner {:hand [(qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Nanisivik Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (core/move state :corp (get-ice state :hq 0) :discard)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-card state :corp "Ice Wall")
      (is (= :select (prompt-type :corp)) "Cannot select faceup pieces of ice")
      (is (changed? [(count (:hand (get-runner))) -1]
            (click-card state :corp "Anemone")
            (click-prompt state :corp "Do 1 net damage"))
          "Runner suffered 1 net damage")
      (is (empty? (remove #(:seen %) (:discard (get-corp)))) "Anemone was turned faceup")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (is (no-prompt? state :corp) "No prompt when no facedown cards are in Archives")))

(deftest navi-mumbai-city-grid
  ;; Navi Mumbai City Grid
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
        (is (no-prompt? state :runner) "Could not use D4v1d")
        (card-ability state :runner ddos 0)
        (is (empty? (:discard (get-runner))) "DDoS not trashed")
        (is (changed? [(:credit (get-runner)) -4]
              (auto-pump-and-break state (refresh cor)))
            "Paid 3+1 to break Fire Wall with Corroder"))))

(deftest navi-mumbai-city-grid-navi-mumbai-city-grid-blocks-only-runner-abilities-test
    ;; Navi Mumbai City Grid blocks only runner abilities test
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
        (is (changed? [(:credit (get-corp)) 5]
              (card-ability state :corp (refresh ngo) 0))
            "+5 credits from NGO"))))

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
      (click-advance state :corp (refresh nis))
      (is (= 3 (get-counters (refresh nis) :advancement)) "3 advancements on agenda")
      (is (= 3 (:credit (get-corp))) "No credit gained")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (click-advance state :corp (refresh (get-ice state :remote1 0)))
      (is (= 2 (:credit (get-corp))) "No credit gained from advancing ice"))))

(deftest nihongai-grid
  ;; Nihongai Grid
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
      (is (accessing state "Beanstalk Royalties") "Runner accesses switched card")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (find-card "Accelerated Beta Test" (:hand (get-corp))))
      (is (find-card "Beanstalk Royalties" (:deck (get-corp))))
      (take-credits state :runner)
      (is (find-card "Beanstalk Royalties" (:hand (get-corp))))))

(deftest nihongai-grid-interaction-with-rng-key-5046
    ;; Interaction with RNG Key. #5046
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
      (is (= "Name a number?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= "Guess a number" (:msg (prompt-map :runner))))
      (click-prompt state :runner "3")
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
      (click-prompt state :runner "No action")))

(deftest oaktown-grid
  (do-game
    (new-game {:corp {:hand ["Oaktown Grid" "PAD Campaign"]}
               :runner {:credits 7}})
    (play-from-hand state :corp "Oaktown Grid" "New remote")
    (play-from-hand state :corp "PAD Campaign" "Server 1")
    (let [og (get-content state :remote1 0)
          pad (get-content state :remote1 1)]
      (rez state :corp og)
      (rez state :corp pad)
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-continue state)
      (click-card state :runner pad)
      (click-prompt state :runner "Pay 7 [Credits] to trash")
      (is (= "PAD Campaign" (:title (first (:discard (get-corp))))))
      (is (= 0 (:credit (get-runner)))))))

(deftest oberth-protocol
  ;; Oberth Protocol
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Oberth Protocol" "Oaktown Renovation"]}})
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Oberth Protocol" "New remote")
    (play-from-hand state :corp "Oaktown Renovation" "Server 2")
    (take-credits state :corp)
    (take-credits state :runner)
    (let [oberth (get-content state :remote2 0)
          oak (get-content state :remote2 1) ]
      (rez state :corp (refresh oberth) {:expect-rez false})
      (click-card state :corp (get-scored state :corp 0))
      (advance state oak)
      (is (= 2 (get-counters (refresh oak) :advancement)) "Oaktown should have 2 advancement tokens on itself"))))

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
        (click-prompt state :runner "No action")
        (core/steal state :runner (make-eid state) (find-card "House of Knives" (:hand (get-corp))))
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Steal")
        (is (= 2 (count (:scored (get-runner)))) "2 stolen agendas"))))

(deftest old-hollywood-grid-central-server
    ;; Central server
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
        (click-prompt state :runner "Pay 4 [Credits] to trash") ;; trash OHG
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))

(deftest old-hollywood-grid-gang-sign-interaction-prevent-the-steal-outside-of-a-run-2169
    ;; Gang Sign interaction. Prevent the steal outside of a run. #2169
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
      (is (accessing state "Project Beale"))
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (zero? (count (:scored (get-runner)))) "No stolen agendas")))

(deftest old-hollywood-grid-trash-order
    ;; Trash order
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" "Project Beale"]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)]
        (run-on state "Server 1")
        (rez state :corp ohg)
        (run-continue state)
        (is (empty? (:scored (get-runner))) "Start with no stolen agendas")
        ;; runner now chooses which to access.
        (click-card state :runner (refresh ohg))
        (click-prompt state :runner "Pay 4 [Credits] to trash") ;; trash OHG
        (click-prompt state :runner "No action")
        (is (empty? (:scored (get-runner))) "End with no stolen agendas")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))

(deftest old-hollywood-grid-steal-other-agendas
    ;; Steal other agendas
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "Project Beale" 2)]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)]
        (rez state :corp ohg)
        (run-empty-server state "Server 2")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))

(deftest old-hollywood-grid-stops-protecting-archives-after-being-trashed-4501
    ;; Stops protecting Archives after being trashed #4501
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
      (click-prompt state :runner "Steal")))

(deftest overseer-matrix-basic-functionality
    ;; Basic functionality
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
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 2 (count-tags state)) "Runner takes a tag"))))

(deftest overseer-matrix-effect-persists-after-trash
    ;; Effect persists after trash
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" (qty "Red Herrings" 3)]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)]
        (run-on state "Server 1")
        (rez state :corp om)
        (run-continue state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 2 (count-tags state)) "Runner takes a tag"))))

(deftest overseer-matrix-effect-ends-after-current-run
    ;; Effect ends after current run
    (do-game
      (new-game {:corp {:deck ["Overseer Matrix" (qty "Red Herrings" 3)]}})
      (play-from-hand state :corp "Overseer Matrix" "New remote")
      (play-from-hand state :corp "Red Herrings" "Server 1")
      (take-credits state :corp)
      (let [om (get-content state :remote1 0)]
        (run-on state "Server 1")
        (rez state :corp om)
        (run-continue state)
        (is (zero? (count-tags state)) "Runner starts with no tags")
        (click-card state :runner om)
        (click-prompt state :runner "Pay 2 [Credits] to trash")
        (is (= {:number 1 :default 0} (:choices (prompt-map :corp))))
        (click-prompt state :corp "1")
        (is (= 1 (count-tags state)) "Runner takes a tag")
        (click-prompt state :runner "No action")
        (is (= 1 (count-tags state)) "Runner doesn't take a tag")
        (run-on state "Server 1")
        (run-continue state)
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (is (no-prompt? state :corp) "No prompt for Overseer Matrix")
        (is (= 1 (count-tags state)) "Runner doesn't take a tag"))))

(deftest overseer-matrix-takes-into-account-apocalypse-like-full-server-trashes
    ;; Takes into account apocalypse-like full-server trashes
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

(deftest overseer-matrix-only-works-on-corp-card-trashes-issue-4739
    ;; Only works on Corp card trashes. Issue #4739
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
      (is (changed? [(count-tags state) 0]
            (card-ability state :runner (get-hardware state 0) 1))
          "Runner should not gain a tag from trashing Spy Camera")
      (click-prompt state :runner "OK")
      (run-jack-out state)
      (is (changed? [(count-tags state) 0]
            (run-empty-server state :remote2)
            (click-prompt state :runner "Pay 4 [Credits] to trash"))
          "Runner should not gain a tag from trashing a card in another server")))

(deftest overseer-matrix-doesn-t-trigger-on-maxx-ability-5129
    ;; Doesn't trigger on MaxX ability #5129
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
      (is (no-prompt? state :corp))))

(deftest panic-button
  (do-game
    (new-game {:corp {:hand ["Panic Button"]
                      :deck ["Enigma"]}})
    (play-from-hand state :corp "Panic Button")
    (is (= ["HQ"] (prompt-buttons :corp)) "Can only be installed in HQ")
    (click-prompt state :corp "HQ")
    (let [panic-btn (get-content state :hq 0)]
      (rez state :corp panic-btn)
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :corp (refresh panic-btn) 0)
      (is (find-card "Enigma" (:hand (get-corp))))
      (is (zero? (count (:deck (get-corp))))))))

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
      (is (no-prompt? state :corp) "Prisec does not trigger from HQ")))

(deftest prisec-multiple-unrezzed-upgrades-in-archives-interaction-with-drt
    ;; Multiple unrezzed upgrades in Archives interaction with DRT
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
      (is (= 4 (count (:discard (get-runner)))) "Runner took 4 meat damage")))

(deftest product-placement
  ;; Product Placement - Gain 2 credits when Runner accesses it
  (do-game
    (new-game {:corp {:deck ["Product Placement"]}})
    (play-from-hand state :corp "Product Placement" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-empty-server state "Server 1")
    (is (= 9 (:credit (get-corp))) "Gained 2 credits from Runner accessing Product Placement")
    (click-prompt state :runner "Pay 2 [Credits] to trash") ; Runner trashes PP
    (run-empty-server state "Archives")
    (is (= 9 (:credit (get-corp)))
        "No credits gained when Product Placement accessed in Archives")))

(deftest red-herrings
  ;; Red Herrings
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
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest red-herrings-cost-increase-even-when-trashed
    ;; Cost increase even when trashed
    (do-game
      (new-game {:corp {:deck [(qty "Red Herrings" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [rh (get-content state :remote1 0)]
        (rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        ;; should now have prompt to pay 5cr for HoK
        (click-prompt state :runner "Pay to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest red-herrings-trashed-from-hq
    ;; Trashed from HQ
    (do-game
      (new-game {:corp {:deck ["Red Herrings" "House of Knives"]}})
      (trash-from-hand state :corp "Red Herrings")
      (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      ;; prompt should be asking to steal HoK
      (is (= ["Steal"] (prompt-buttons :runner)) "Runner being asked to Steal")))

(deftest red-herrings-don-t-affect-runs-on-other-servers
    ;; Don't affect runs on other servers
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
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest reduced-service
  ;; Reduced Service - Increase run cost by 2x number of power counters
  (do-game
     (new-game {:corp {:deck ["Reduced Service"]}
                :runner {:deck ["Dirty Laundry"]}})
     (play-from-hand state :corp "Reduced Service" "HQ")
     (take-credits state :corp)
     (let [rs (get-content state :hq 0)]
       (rez state :corp rs)
       (is (changed? [(:credit (get-corp)) -4]
             (click-prompt state :corp "4")))
       (is (= 4 (get-counters (refresh rs) :power)) "4 counters placed on Reduced Service")
       (play-from-hand state :runner "Dirty Laundry")
       (is (not (some #{"HQ"} (prompt-buttons :runner)))
           "Runner should not get to choose HQ due to increased cost")
       (click-prompt state :runner "Archives")
       (is (= 4 (get-counters (refresh rs) :power)) "No counter removed by only making a run")
       (run-continue state)
       (is (= 3 (get-counters (refresh rs) :power)) "1 counters removed from Reduced Service by successful run")
       (is (changed? [(:credit (get-runner)) -6]
             (run-on state :hq)))
       (run-continue state)
       (is (= 2 (get-counters (refresh rs) :power)) "1 counters removed from Reduced Service by successful run")
       (click-prompt state :runner "Pay 2 [Credits] to trash")
       (is (= 1 (count (:discard (get-corp)))) "Reduced Service trashed")
       (is (changed? [(:credit (get-runner)) 0]
             (run-on state :hq)))
       (is (:run @state) "Runner got to run without paying anything after trashing reduced service"))))

(deftest research-station
  (do-game
    (new-game {:corp {:hand [(qty "Research Station" 8)]
                      :deck ["Research Station"]}})
    (play-from-hand state :corp "Research Station" "HQ")
    (rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (is (= 7 (count (:hand (get-corp)))) "+3 cards, 2 of Research Station and +1 of ID")
    (is (no-prompt? state :corp) "No prompt asking to discard cards from hand")
    (take-credits state :runner)
    (is (= 8 (count (:hand (get-corp)))) "Double check that you start next turn with 8 cards")))

(deftest ruhr-valley
  ;; Ruhr Valley
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

(deftest ruhr-valley-if-the-runner-trashes-with-one-click-left-the-ability-to-run-is-enabled
    ;; If the runner trashes with one click left, the ability to run is enabled
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
       (is (:run @state) "Runner got to run"))))

(deftest rutherford-grid
  (do-game
    (new-game {:corp {:hand ["Rutherford Grid" "Caduceus"]}})
    (play-from-hand state :corp "Rutherford Grid" "New remote")
    (play-from-hand state :corp "Caduceus" "Server 1")
    (let [rg (get-content state :remote1 0)
          caduceus (get-ice state :remote1 0)]
      (rez state :corp rg)
      (rez state :corp caduceus)
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-continue state)
      (fire-subs state caduceus)
      (click-prompt state :corp "0")
      (is (= 5 (:strength (get-prompt state :runner))) "3 base, +2 of upgrade"))))

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
      (is (= 1 (:brain-damage (get-runner))) "Did 1 core damage")
      (is (= 1 (count (:discard (get-corp)))) "Ryon trashed"))))

(deftest sansan-city-grid
  ;; SanSan City Grid
  (do-game
      (new-game {:corp {:hand ["Merger" "SanSan City Grid"]
                        :credits 10}})
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (play-from-hand state :corp "Merger" "Server 1")
      (is (= 3 (core/get-advancement-requirement (get-content state :remote1 1))))
      (rez state :corp (get-content state :remote1 0))
      (is (= 2 (core/get-advancement-requirement (get-content state :remote1 1))))))

(deftest satellite-grid
  ;; Satellite Grid - Add 1 fake advancement on all ice protecting server
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
      (is (= 1 (get-strength (refresh iw2))) "Satellite Grid not impacting ice elsewhere")
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

(deftest shell-corporation
  (do-game
    (new-game {:corp {:hand ["Shell Corporation"]}})
    (play-from-hand state :corp "Shell Corporation" "New remote")
    (let [sc (get-content state :remote1 0)]
      (rez state :corp sc)
      (card-ability state :corp (refresh sc) 0)
      (is (changed? [(get-counters (refresh sc) :credit) 0]
            (card-ability state :corp (refresh sc) 0))
          "No additional credits are placed on Shell Corp")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (changed? [(get-counters (refresh sc) :credit) 3]
            (card-ability state :corp (refresh sc) 0))
          "Gains 3 more credits")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (changed? [(:credit (get-corp)) 6]
            (card-ability state :corp (refresh sc) 1))
          "Clears out shell corp")
      (is (zero? (get-counters (refresh sc) :credit))))))

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
      (is (no-prompt? state :runner) "SJ blocking SMC")
      (run-jack-out state)
      (card-ability state :runner smc2 0)
      (click-prompt state :runner "Reaver"))))

(deftest simone-diego-pay-credits-prompt
    ;; Pay-credits prompt
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
        (is (changed? [(:credit (get-corp)) 0]
              (click-advance state :corp (refresh iw))
              (click-card state :corp sd))
            "Used 1 credit from Simone Diego to advance Ice Wall")
        (is (changed? [(:credit (get-corp)) 0]
              (click-advance state :corp (refresh pj))
              (click-card state :corp sd))
            "Used 1 credit from Simone Diego to advance Project Junebug"))))

(deftest strongbox
  ;; Strongbox
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
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (= 1 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest strongbox-click-cost-even-when-trashed
    ;; Click cost even when trashed
    (do-game
      (new-game {:corp {:deck [(qty "Strongbox" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Strongbox" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [sb (get-content state :remote1 0)]
        (rez state :corp sb)
        (run-empty-server state "Server 1")
        (click-card state :runner sb)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

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
        (is (= (:cid scg2) (-> (prompt-map :corp) :card :cid)) "Surat City Grid triggered from upgrade in root of HQ")
        (click-prompt state :corp "No")
        (derez state :corp (refresh wrap))
        (rez state :corp enig)
        (is (= (:cid scg2) (-> (prompt-map :corp) :card :cid)) "SCG did trigger for ice protecting HQ")))))

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
    (is (zero? (:brain-damage (get-runner))) "Runner starts with 0 core damage")
    (click-prompt state :runner "0")
    (click-prompt state :runner "Suffer 1 core damage")
    (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (take-credits state :runner)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (is (= 1 (:brain-damage (get-runner))) "Runner takes no core damage")
    (is (= 3 (:click (get-runner))) "Runner loses no clicks")
    (run-on state "HQ")
    (run-continue state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "0")
    (is (= 1 (:brain-damage (get-runner))) "Runner starts with 1 core damage")
    (click-prompt state :runner "Suffer 1 core damage")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 1 core damage")
    (click-prompt state :runner "No action") ; don't trash
    (run-on state "HQ")
    (run-continue state)
    (click-prompt state :corp "0") ; trace
    (click-prompt state :runner "4")
    (click-prompt state :runner "Pay 0 [Credits] to trash")))

(deftest the-holo-man
  (do-game
    (new-game {:corp {:hand ["The Holo Man" "Vanilla" "Rashida Jaheem"]
                      :credits 10}})
    (play-from-hand state :corp "The Holo Man" "HQ")
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (take-credits state :runner)
    (is (:corp-phase-12 @state) "The Holo Man is waiting")
    (end-phase-12 state :corp)
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Server 1")
    (let [rash (get-content state :remote1 0)
          thm (get-content state :remote1 1)]
      (is (changed? [(get-counters (refresh rash) :advancement) 3]
                    (card-ability state :corp (refresh thm) 0)
                    (click-card state :corp rash))
          "Corp placed 3 advancement counters on Rashida")
      (card-ability state :corp (refresh thm) 0)
      (is (no-prompt? state :corp) "The Holo Man ability is once per turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (end-phase-12 state :corp)
      (click-prompt state :corp "No")
      (play-from-hand state :corp "Vanilla" "Server 1")
      (let [van (get-ice state :remote1 0)]
        (is (changed? [(get-counters (refresh van) :advancement) 2]
                      (card-ability state :corp (refresh thm) 0)
                      (click-card state :corp van))
            "Corp placed 2 advancement counters on Vanilla")))))

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
      (is (last-log-contains? state "Runner passes Quicksand protecting Server 1 at position 0") "Pass Quicksand")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Quicksand" (:hand (get-corp))))
      (is (= 0 (:position (get-run))) "Run should still be at position 0")
      (is (utils/same-card? quicksand (core/get-current-ice state)))
      (is (= 2 (get-counters (get-ice state :remote1 0) :power)) "Encounter abilities resolve a second time")
      (run-continue state)
      (is (not (second-last-log-contains? state "Runner passes Quicksand protecting Server 1 at position 0"))
          "Does not pass Quicksand again")
      (is (= 1 (count (:discard (get-corp)))) "The copy of Quicksand was trashed"))))

(deftest tori-hanzo
  ;; Tori Hanzō - Pay to do 1 brain damage instead of net damage
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
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state "HQ")
        (run-continue state)
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Suffer 1 net damage")
        (click-prompt state :runner "Done")
        (click-prompt state :corp "Yes")
        (is (= 2 (count (:discard (get-runner)))) "1 core damage suffered")
        (is (= 1 (:brain-damage (get-runner)))))))

(deftest tori-hanzo-with-hokusai-grid-issue-2702
    ;; with Hokusai Grid: Issue #2702
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
        (click-prompt state :runner "No action")
        (is (no-prompt? state :corp) "No prompts, run ended")
        (run-empty-server state "Archives")
        (click-prompt state :corp "Yes") ; Tori prompt to pay 2c to replace 1 net with 1 brain
        (is (= 2 (count (:discard (get-runner)))))
        (is (= 1 (:brain-damage (get-runner))) "1 core damage suffered")
        (click-prompt state :runner "Hokusai Grid")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (no-prompt? state :corp) "No prompts, run ended"))))

(deftest tori-hanzo-breaking-subsequent-net-damage-issue-3176
    ;; breaking subsequent net damage: Issue #3176
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
        (is (= 1 (count (:discard (get-runner)))) "1 core damage suffered")
        (is (= 1 (:brain-damage (get-runner))))
        (run-continue state :movement)
        (run-jack-out state)
        (take-credits state :runner)
        (play-from-hand state :corp "Neural EMP")
        (is (= 2 (count (:discard (get-runner)))) "Net damage processed correctly"))))

(deftest traffic-analyzer
  (do-game
    (new-game {:corp   {:hand ["Traffic Analyzer" "Ice Wall"]}
               :runner {:hand [(qty "Corroder" 2)]}})
    (play-from-hand state :corp "Traffic Analyzer" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-content state :hq 0))
    (rez state :corp (get-ice state :hq 0))
    (is (changed? [(:credit (get-corp)) 1]
          (click-prompt state :corp "0")
          (click-prompt state :runner "0"))
        "Gains 1 credit from Traffic Analyzer")))

(deftest tranquility-home-grid
  ;; Tranquility Home Grid
  (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 5)]
                        :hand ["Tranquility Home Grid" "PAD Campaign"]}})
      (play-from-hand state :corp "Tranquility Home Grid" "New remote")
      (let [thg (get-content state :remote1 0)]
        (rez state :corp thg)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (is (no-prompt? state :corp) "THG didn't trigger on the PAD Campaign install, because its own install was the first install this turn")
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (click-prompt state :corp "OK") ; Trash existing PAD Campaign
        (is (changed? [(:credit (get-corp)) 2]
              (click-prompt state :corp "Gain 2 [Credits]"))
            "Gained 2 credits from THG")
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (click-prompt state :corp "OK") ; Trash existing PAD Campaign
        (is (changed? [(count (:hand (get-corp))) 1]
              (click-prompt state :corp "Draw 1 card"))
            "Drew 1 card from THG"))))

(deftest tranquility-home-grid-not-installable-on-centrals
    ;; Not installable on centrals
    (do-game
      (new-game {:corp {:hand ["Tranquility Home Grid"]}})
      (play-from-hand state :corp "Tranquility Home Grid")
      (is (= ["New remote"] (prompt-buttons :corp)) "Only installable in a remote server")))

(deftest tranquility-home-grid-restore-interaction
    ;; Restore interaction
    (do-game
      (new-game {:corp {:hand ["Tranquility Home Grid" "Restore"]}})
      (core/move state :corp (find-card "Tranquility Home Grid" (:hand (get-corp))) :discard)
      (play-from-hand state :corp "Restore")
      (click-card state :corp (find-card "Tranquility Home Grid" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (no-prompt? state :corp) "No prompt from THG on its own install, because it was inactive at the point of install triggers")))

(deftest tranquility-home-grid-thg-interaction-with-ice-install
    ;; THG interaction with ice install
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 5)]
                        :hand ["Tranquility Home Grid" "PAD Campaign" "Ice Wall"]}})
      (play-from-hand state :corp "Tranquility Home Grid" "New remote")
      (let [thg (get-content state :remote1 0)]
        (rez state :corp thg)
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (is (no-prompt? state :corp) "THG didn't trigger on the PAD Campaign install, because its own install was the first install this turn")
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "Ice Wall" "Server 1")
        (is (no-prompt? state :corp) "No prompt from Ice Wall install")
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (click-prompt state :corp "OK") ; Trash existing PAD Campaign
        (is (changed? [(:credit (get-corp)) 2]
              (click-prompt state :corp "Gain 2 [Credits]"))
            "Gained 2 credits from THG"))))

(deftest tranquility-home-grid-interaction-with-pad-tap-issue-4835
    ;; Interaction with PAD Tap. Issue #4835
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
        (is (changed? [(:credit (get-runner)) 1]
              (click-prompt state :corp "Gain 2 [Credits]"))
            "Runner gained 1 credit from PAD Tap"))))

(deftest tranquility-home-grid-interaction-with-adt-and-agendas
  ;;check tranquility fires at the right time when installing an agenda and failing to rez it, issue #6588
  (do-game
    (new-game {:corp {:deck [(qty "PAD Campaign" 3) (qty "Project Atlas" 2)]
                      :hand ["Tranquility Home Grid" "Architect Deployment Test"]
                      :credits 10}})
    (play-from-hand state :corp "Tranquility Home Grid" "New remote")
    (play-from-hand state :corp "Architect Deployment Test" "Server 1")
    (rez state :corp (get-content state :remote1 0))
    (let [adt (get-content state :remote1 1)]
      (take-credits state :corp)
      (take-credits state :runner)
      (score-agenda state :corp (refresh adt))
      (click-prompt state :corp "OK")
      (click-prompt state :corp "Project Atlas")
      (click-prompt state :corp "Server 1")
      (click-prompt state :corp "Gain 2 [Credits]"))))

(deftest tranquility-home-grid-asa-interaction
  (do-game
    (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                      :deck ["Ice Wall"]
                      :hand ["Tranquility Home Grid" "Vanilla" "Rashida Jaheem"]}})
    (play-from-hand state :corp "Tranquility Home Grid" "New remote")
    (click-prompt state :corp "Done")
    (take-credits state :corp)
    (take-credits state :runner)
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Rashida Jaheem" "Server 1")
    (click-prompt state :corp "Tranquility Home Grid")
    (click-prompt state :corp "Draw 1 card")
    (click-card state :corp "Ice Wall")))

(deftest tranquility-home-grid-a-teia-interaction
  (do-game
    (new-game {:corp {:id "A Teia: IP Recovery"
                      :deck ["Ice Wall"]
                      :hand ["Tranquility Home Grid" "Vanilla" "Rashida Jaheem"]}})
    (play-from-hand state :corp "Tranquility Home Grid" "New remote")
    (click-prompt state :corp "Done")
    (take-credits state :corp)
    (take-credits state :runner)
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Rashida Jaheem" "Server 1")
    (click-prompt state :corp "Tranquility Home Grid")
    (click-prompt state :corp "Draw 1 card")
    (click-card state :corp "Ice Wall")
    (click-prompt state :corp "New remote")))

(deftest tucana-corp-scores
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Fire Wall"]
                      :hand ["Tucana" "Project Atlas"]}})
    (play-from-hand state :corp "Tucana" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Project Atlas" "Server 1")
    (score-agenda state :corp (get-content state :remote1 1))
    (is (changed? [(:credit (get-corp)) -2]
          (is (= ["Fire Wall" "Ice Wall" "Cancel"] (prompt-titles :corp)))
          (click-prompt state :corp "Fire Wall")
          (is (= ["Archives" "R&D" "HQ" "Server 1" "New remote"] (prompt-buttons :corp)))
          (click-prompt state :corp "HQ")
          (is (= "Fire Wall" (:title (first (get-in @state [:corp :servers :hq :ices])))) "Fire Wall on HQ"))
        "Corp spends 5 - 3 credits to rez Fire Wall")))

(deftest tucana-runner-steals
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Fire Wall"]
                     :hand ["Tucana" "Project Atlas"]}})
    (play-from-hand state :corp "Tucana" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Project Atlas" "Server 1")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-card state :runner (get-content state :remote1 0))
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (changed? [(:credit (get-corp)) -2]
          (click-prompt state :runner "Steal")
          (is (= ["Fire Wall" "Ice Wall" "Cancel"] (prompt-titles :corp)) "Tucana persistent, effect fires")
          (click-prompt state :corp "Fire Wall")
          (is (= ["Archives" "R&D" "HQ" "New remote"] (prompt-buttons :corp)) "Corp choices should not include original server as it's gone")
          (click-prompt state :corp "HQ")
          (is (= "Fire Wall" (:title (first (get-in @state [:corp :servers :hq :ices])))) "Fire Wall on HQ"))
        "Corp spends 5 - 3 credits to rez Fire Wall")))

(deftest tucana-is-remote-only
  (do-game
    (new-game {:corp {:hand ["Tucana"]}})
    (play-from-hand state :corp "Tucana")
      (is (not (some #{"HQ" "R&D" "Archives"} (prompt-buttons :corp)))
          "Central servers are not listed in the install prompt")
    (click-prompt state :corp "New remote")))

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

(deftest valley-grid-reduce-runner-max-hand-size-and-restore-it-even-if-trashed
    ;; Reduce Runner max hand size and restore it even if trashed
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
        (is (= 5 (hand-size :runner)) "Runner max hand size back to normal"))))

(deftest vladisibirsk-city-grid
  ;; Vladisibirsk Grid: can't target self, once per turn, moves counters, same server
  (do-game
   (new-game  {:corp {:deck ["Vladisibirsk City Grid" "NGO Front" "NGO Front" "Dedication Ceremony" "Warroid Tracker"]
                      :credits 10}})
   (core/gain state :corp :click 10)
   (play-from-hand state :corp "Vladisibirsk City Grid", "New remote")
   (play-from-hand state :corp "NGO Front", "Server 1")
   (play-from-hand state :corp "NGO Front", "New remote")
   (play-from-hand state :corp "Warroid Tracker", "Server 1")
   (let [vlad (get-content state :remote1 0)
         ngo1 (get-content state :remote1 1)
         war (get-content state :remote1 2)
         ngo2 (get-content state :remote2 0)]
     (rez state :corp (refresh vlad))
     (play-from-hand state :corp "Dedication Ceremony")
     (click-card state :corp vlad)
     (is (= 3 (get-counters (refresh vlad) :advancement)) "Vladisibirsk City Grid has 3 counters on it")
     (advance state (refresh vlad) 1)
     (is (= 4 (get-counters (refresh vlad) :advancement)) "Vladisibirsk City Grid has 4 counters on it")
     (card-ability state :corp (refresh vlad) 0)
     (is (not (no-prompt? state :corp)) "Vlad Grid prompt is active")
     ;; check it cant be used on itself
     (click-card state :corp "Vladisibirsk City Grid")
     (is (not (no-prompt? state :corp)) "Vlad Grid prompt still active")
     (is (= 4 (get-counters (refresh vlad) :advancement)) "Vladisibirsk City Grid still has 4 counters on it")
     ;; check it can't be used on cards that cannot be advanced
     (click-card state :corp "Warroid Tracker")
     (is (not (no-prompt? state :corp)) "ability not used, prompt still active")
     (is (= 4 (get-counters (refresh vlad) :advancement)) "Vladisibirsk City Grid still has 4 counters on it")
     (is (= 0 (get-counters (refresh war) :advancement)) "Warroid Tracker has no counters on it")
     ;; check it works on cards that can be advanced
     (click-card state :corp ngo1)
     (is (= 2 (get-counters (refresh vlad) :advancement)) "Vladisibirsk City Grid has spent 2 counters")
     (is (= 2 (get-counters (refresh ngo1) :advancement)) "NGO Front has gained 2 counters")
     (is (no-prompt? state :corp) "ability used, prompt gone?")
     ;;check it only works once per turn
     (card-ability state :corp (refresh vlad) 0)
     (is (no-prompt? state :corp) "Vlad Grid prompt is not active")
     (take-credits state :corp)
     ;;check it only works on cards installed in the same server
     (card-ability state :corp (refresh vlad) 0)
     (is (not (no-prompt? state :corp)) "Vlad Grid prompt is active")
     (click-card state :corp ngo2)
     (is (= 2 (get-counters (refresh vlad) :advancement)) "Vladisibirsk City Grid has not spent counters")
     (is (= 0 (get-counters (refresh ngo2) :advancement)) "NGO Front 2 has gained no counters")
     (is (not (no-prompt? state :corp)) "Vlad Grid prompt is still active"))))

(deftest vovo-ozetti
  (do-game
    (new-game  {:corp {:hand ["Vovô Ozetti" "Enigma" "Vanity Project" "PAD Campaign" "Tranquility Home Grid"]
                       :credits 10}})
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "Vovô Ozetti", "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Enigma", "Server 1")
    (is (changed? [(:credit (get-corp)) -1]
          (rez state :corp (get-ice state :remote1 0)))
        "Rezzed ice paying 2 less credits")
    (play-from-hand state :corp "PAD Campaign", "Server 1")
    (is (changed? [(:credit (get-corp)) -2]
          (rez state :corp (get-content state :remote1 1)))
        "No discount on assets or upgrades when under threat")
    (play-and-score state "Vanity Project")
    (play-from-hand state :corp "Tranquility Home Grid", "Server 1")
    (is (changed? [(:credit (get-corp)) 0]
          (rez state :corp (get-content state :remote1 2)))
        "Rezzed upgrade paying 2 less credits")
    (end-turn state :corp)
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "R&D")
    (is (= "Vovô Ozetti" (:title (get-content state :rd 0))))))

(deftest warroid-tracker-trashing-warroid-directly-starts-trace
    ;; Trashing Warroid starts trace
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

(deftest warroid-tracker-trashing-from-central-triggers-warroid-in-root-issue-3725
    ;; Trashing from central triggers Warroid in root. Issue #3725
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

(deftest warroid-tracker-doesn-t-trigger-from-maxx-issue-4329
    ;; Doesn't trigger from Maxx. Issue #4329
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
        (is (no-prompt? state :corp) "Corp has no prompt")
        (is (no-prompt? state :runner) "Runner has no prompt"))))

(deftest warroid-tracker-interactions-with-ig-issue-4329
    ;; Interactions with IG. Issue #4329
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck [(qty "Hedge Fund" 3)]
                        :hand ["Warroid Tracker" "Launch Campaign" (qty "Hedge Fund" 2)]
                        :credits 100}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Corroder" "Dyson Mem Chip"]
                          :credits 100}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (play-from-hand state :corp "Launch Campaign" "Server 1")
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
        (click-prompt state :runner "No action")
        (is (= 2 (count (:discard (get-runner)))) "Runner trashes 2 cards to Warriod Tracker"))))

(deftest warroid-tracker-trashing-warroid-with-card-ability-starts-trace
    ;; Trashing Warroid starts trace
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Warroid Tracker" "PAD Campaign"]
                        :credits 15}
                 :runner {:hand ["Singularity" (qty "Akamatsu Mem Chip" 5)]
                          :credits 100}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (play-from-hand state :corp "PAD Campaign" "Server 1")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (dotimes [_ 5]
        (play-from-hand state :runner "Akamatsu Mem Chip"))
      (rez state :corp (get-content state :remote1 0))
      (play-from-hand state :runner "Singularity")
      (click-prompt state :runner "Server 1")
      (run-continue state)
      (is (= 2 (-> (get-corp) :discard count)) "Corp has both cards in discard")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0") ; Corp wins trace
      (click-card state :runner (get-hardware state 0))
      (click-card-impl state :runner (get-hardware state 1))
      (is (no-prompt? state :corp) "Warroid Tracker can't trash anything else")
      (is (= 3 (-> (get-runner) :discard count)) "Runner should trash 2 installed cards")))

(deftest warroid-tracker-should-trigger-from-self-trash-in-root-of-central-server
    ;; Should trigger from self-trash in root of central server
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
        (is (not (no-prompt? state :corp)))
        (is (not (no-prompt? state :runner))))))

(deftest warroid-tracker-should-trigger-when-trashed-card-is-in-root-of-central-server
    ;; Should trigger when trashed card is in root of central server.
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
        (click-prompt state :corp "0")
        (click-prompt state :runner "5")
        (click-prompt state :runner "No action"))))

(deftest warroid-tracker-shouldn-t-trigger-when-trashed-by-corp-via-hellion-beta-test-issue-4941
    ;; Shouldn't trigger when trashed by corp (via Hellion Beta Test). Issue #4941
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Warroid Tracker" (qty "Marilyn Campaign" 3) "Hellion Beta Test"]
                        :credits 20}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Corroder" "Dyson Mem Chip"]
                          :credits 20}})
      (play-from-hand state :corp "Warroid Tracker" "New remote")
      (play-from-hand state :corp "Marilyn Campaign" "Server 1")
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
        (is (no-prompt? state :corp) "Corp has no prompt"))))

(deftest will-o-the-wisp
  ;; Will-o'-the-Wisp
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
       (auto-pump-and-break state (refresh cor))
       (core/continue state :corp nil)
       (run-continue state)
       (click-prompt state :corp "Yes")
       (click-card state :corp (refresh cor))
       (is (empty? (get-program state)) "Corroder uninstalled")
       (is (= "Corroder" (:title (last (:deck (get-runner))))) "GoCorroderrdian on bottom of Stack"))))

(deftest zato-basic
  (do-game
    (new-game {:corp {:hand ["ZATO City Grid" "Vanilla"] :credits 10}})
    (play-from-hand state :corp "ZATO City Grid" "New remote")
    (play-from-hand state :corp "Vanilla" "Server 1")
    (take-credits state :corp)
    (run-on state :remote1)
    (rez state :corp (get-ice state :remote1 0))
    (rez state :corp (get-content state :remote1 0))
    (run-continue state :encounter-ice)
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "End the run")
    (is (= 1 (count (:discard (get-corp)))) "trashed vanilla")
    (is (not (:run @state)) "Run ended")))

(deftest zato-multiple-encounters
  (do-game
    (new-game {:corp {:hand ["ZATO City Grid" "Funhouse"] :credits 10}})
    (play-from-hand state :corp "ZATO City Grid" "New remote")
    (play-from-hand state :corp "Funhouse" "Server 1")
    (take-credits state :corp)
    (run-on state :remote1)
    (rez state :corp (get-ice state :remote1 0))
    (rez state :corp (get-content state :remote1 0))
    (run-continue state :encounter-ice)
    (click-prompt state :corp "ZATO Ability")
    (click-prompt state :corp "No")
    (click-prompt state :runner "End the run")
    (is (not (:run @state)) "Run ended")
    (run-on state :remote1)
    (run-continue state :encounter-ice)
    (click-prompt state :corp "Funhouse")
    (click-prompt state :runner "End the run")
    (is (not (:run @state)) "Run ended")
    (is (no-prompt? state :corp))))

(deftest yakov-game-trash
  (do-game
    (new-game {:corp {:hand ["Yakov Erikovich Avdakov" (qty "NGO Front" 2)]}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Yakov Erikovich Avdakov" "New remote")
    (play-from-hand state :corp "NGO Front" "Server 1")
    (let [yakov (get-content state :remote1 0)]
      (rez state :corp yakov)
      (is (changed? [(:credit (get-corp)) 0]
            (play-from-hand state :corp "NGO Front" "Server 1")
            (click-prompt state :corp "OK"))
          "hard trash doesn't trigger"))))

(deftest yakov-corp-trash
  (do-game
    (new-game {:corp {:hand ["Yakov Erikovich Avdakov" "NGO Front"]}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Yakov Erikovich Avdakov" "New remote")
    (play-from-hand state :corp "NGO Front" "Server 1")
    (let [yakov (get-content state :remote1 0)
          ngo (get-content state :remote1 1)]
      (advance state (refresh ngo))
      (rez state :corp yakov)
      (rez state :corp (refresh ngo))
      (is (changed? [(:credit (get-corp)) +7]
            (card-ability state :corp (refresh ngo) 0))
          "5 + 2 from ngo/yakov"))))

(deftest yakov-runner-trash
  (do-game
    (new-game {:corp {:hand ["Yakov Erikovich Avdakov"]}
               :runner {:hand ["Pinhole Threading"]}})
    (play-from-hand state :corp "Yakov Erikovich Avdakov" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Pinhole Threading")
    (click-prompt state :runner "Archives")
    (run-continue state)
    (is (changed? [(:credit (get-corp)) +2]
          (click-card state :runner "Yakov Erikovich Avdakov")
          (click-prompt state :runner "Pay 2 [Credits] to trash"))
        "+2 credits from Yakov been trashed")
    (is (no-prompt? state :corp))))

(deftest yakov-runner-trash-multiple
  (do-game
    (new-game {:corp {:hand ["Yakov Erikovich Avdakov" "NGO Front" "Prisec"]}
               :runner {:hand ["Apocalypse"]}})
    (play-from-hand state :corp "Yakov Erikovich Avdakov" "New remote")
    (play-from-hand state :corp "NGO Front" "Server 1")
    (play-from-hand state :corp "Prisec" "Server 1")
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (is (changed? [(:credit (get-corp)) +6]
          (play-from-hand state :runner "Apocalypse")
          (click-prompt state :corp "Yakov Erikovich Avdakov")
          (click-prompt state :corp "Yakov Erikovich Avdakov"))
        "+6 from 3 trashes")
    (is (no-prompt? state :corp))))

(deftest yakov-multiple-trashed
  (do-game
    (new-game {:corp {:hand ["Yakov Erikovich Avdakov" (qty "NGO Front" 2)
                             "Prisec" "Mutually Assured Destruction"]
                      :credits 15}})
    (core/gain state :corp :click 6)
    (play-from-hand state :corp "Yakov Erikovich Avdakov" "New remote")
    (play-from-hand state :corp "NGO Front" "Server 1")
    (play-from-hand state :corp "Prisec" "Server 1")
    (play-from-hand state :corp "NGO Front" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (rez state :corp (get-content state :remote1 1))
    (rez state :corp (get-content state :remote1 2))
    (rez state :corp (get-content state :remote2 0))
    (is (changed? [(:click (get-corp)) -3]
          (play-from-hand state :corp "Mutually Assured Destruction"))
        "Spent 3 clicks to go MAD")
    (is (changed? [(:credit (get-corp)) +6]
          (click-card state :corp (get-content state :remote1 0))
          (click-card state :corp (get-content state :remote1 1))
          (click-card state :corp (get-content state :remote1 2))
          (click-card state :corp (get-content state :remote2 0))
          (click-prompt state :corp "Yakov Erikovich Avdakov")
          (click-prompt state :corp "Yakov Erikovich Avdakov"))
        "trashed 4 cards - 3 in Yakov server")
    (is (no-prompt? state :corp))))
