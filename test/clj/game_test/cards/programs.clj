(ns game-test.cards.programs
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [jinteki.utils :refer [count-tags]]
            [clojure.test :refer :all]))

(deftest algernon
  ;; Algernon - pay 2 credits to gain a click, trash if no successful run
  (testing "Use, successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-on state "Archives")
      (run-successful state)
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed")))
  (testing "Use, no successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-on state "Archives")
      (core/jack-out state :runner nil)
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))) "Algernon trashed")
      (is (empty? (get-program state)) "No programs installed")))
  (testing "Not used, no successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "No")
      (is (= 8 (:credit (get-runner))) "No credits spent")
      (is (= 4 (:click (get-runner))) "No clicks gained")
      (run-on state "Archives")
      (core/jack-out state :runner nil)
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed"))))

(deftest au-revoir
  ;; Au Revoir - Gain 1 credit every time you jack out
  (do-game
    (new-game {:runner {:deck [(qty "Au Revoir" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :corp nil)
    (core/jack-out state :runner nil)
    (is (= 5 (:credit (get-runner))) "Gained 1 credit from jacking out")
    (play-from-hand state :runner "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :corp nil)
    (core/jack-out state :runner nil)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit from each copy of Au Revoir")))

(deftest bankroll
  ;; Bankroll
  (do-game
    (new-game {:runner {:deck ["Bankroll"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Bankroll")
    (is (= 3 (core/available-mu state)) "Bankroll uses up 1 MU")
    (is (= 4 (:credit (get-runner))) "Bankroll cost 1 to install")
    (let [bankroll (get-program state 0)
          hosted-credits #(get-counters (refresh bankroll) :credit)]
      (is (zero? (hosted-credits)) "No counters on Bankroll on install")
      (run-empty-server state "Archives")
      (is (= 1 (hosted-credits)) "One credit counter on Bankroll after one successful run")
      (run-empty-server state "R&D")
      (is (= 2 (hosted-credits)) "Two credit counter on Bankroll after two successful runs")
      (run-empty-server state "HQ")
      (is (= 3 (hosted-credits)) "Three credit counter on Bankroll after three successful runs")
      (card-ability state :runner bankroll 0)
      (is (= (+ 4 3) (:credit (get-runner))) "Gained 3 credits when trashing Bankroll")
      (is (= 1 (-> (get-runner) :discard count)) "Bankroll was trashed"))))

(deftest consume
  ;; Consume - gain virus counter for trashing corp card. click to get 2c per counter.
  (testing "Trash and cash out"
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (let [c (get-program state 0)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters"))))
  (testing "Hivemind interaction"
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume" "Hivemind"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 3)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind retains counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 4 (:credit (get-runner))) "Runner gains 4 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters"))))
  (testing "Hivemind counters only"
    (do-game
      (new-game {:runner {:deck ["Consume" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters")))))

(deftest crescentus
  ;; Crescentus should only work on rezzed ice
  (do-game
    (new-game {:corp {:deck ["Quandary"]}
               :runner {:deck ["Crescentus"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Crescentus")
    (run-on state "HQ")
    (let [cres (get-program state 0)
          q (get-ice state :hq 0)]
      (card-ability state :runner cres 0)
      (is (not (nil? (get-program state 0))) "Crescentus could not be used because the ICE is not rezzed")
      (core/rez state :corp q)
      (is (:rezzed (refresh q)) "Quandary is now rezzed")
      (card-ability state :runner cres 0)
      (is (nil? (get-program state 0)) "Crescentus could be used because the ICE is rezzed")
      (is (not (:rezzed (refresh q))) "Quandary is no longer rezzed"))))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered ICE
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Datasucker"]}})
      (play-from-hand state :corp "Fire Wall" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (play-from-hand state :runner "Datasucker")
      (let [ds (get-program state 0)
            fw (get-ice state :remote1 0)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh ds) :virus)))
        (run-empty-server state "Archives")
        (is (= 2 (get-counters (refresh ds) :virus)))
        (run-on state "Server 1")
        (run-continue state)
        (run-successful state)
        (is (= 2 (get-counters (refresh ds) :virus)) "No counter gained, not a central server")
        (run-on state "Server 1")
        (core/rez state :corp fw)
        (is (= 5 (:current-strength (refresh fw))))
        (card-ability state :runner ds 0)
        (is (= 1 (get-counters (refresh ds) :virus)) "1 counter spent from Datasucker")
        (is (= 4 (:current-strength (refresh fw))) "Fire Wall strength lowered by 1"))))
  (testing "does not affect next ice when current is trashed. Issue #1788"
    (do-game
      (new-game {:corp {:deck ["Wraparound" "Spiderweb"]}
                 :runner {:deck ["Datasucker" "Parasite"]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (play-from-hand state :runner "Datasucker")
      (let [sucker (get-program state 0)
            spider (get-ice state :hq 0)
            wrap (get-ice state :hq 1)]
        (core/add-counter state :runner sucker :virus 2)
        (core/rez state :corp spider)
        (core/rez state :corp wrap)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner "Spiderweb")
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner (refresh sucker) 0)
        (card-ability state :runner (refresh sucker) 0)
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Datasucker")
        (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Datasucker")))))

(deftest dhegdheer
  ;; Dheghdheer - hosting a breaker with strength based on unused MU should calculate correctly
  (do-game
    (new-game {:runner {:deck ["Adept" "Dhegdheer"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Dhegdheer")
    (play-from-hand state :runner "Adept")
    (is (= 3 (:credit (get-runner))) "3 credits left after individual installs")
    (is (= 2 (core/available-mu state)) "2 MU used")
    (let [dheg (get-program state 0)
          adpt (get-program state 1)]
      (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
      (card-ability state :runner dheg 1)
      (click-card state :runner (refresh adpt))
      (let [hosted-adpt (first (:hosted (refresh dheg)))]
        (is (= 4 (:credit (get-runner))) "4 credits left after hosting")
        (is (= 4 (core/available-mu state)) "0 MU used")
        (is (= 6 (:current-strength (refresh hosted-adpt))) "Adept at 6 strength hosted")))))

(deftest disrupter
  ;; Disrupter
  (do-game
    (new-game {:corp {:deck [(qty "SEA Source" 2)]}
               :runner {:deck ["Disrupter"]}})
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (play-from-hand state :runner "Disrupter")
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :runner "Yes")
    (is (zero? (-> (get-corp) :prompt first :base)) "Base trace should now be 0")
    (is (= 1 (-> (get-runner) :discard count)) "Disrupter should be in Heap")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (zero? (count-tags state)) "Runner should gain no tag from beating trace")
    (play-from-hand state :corp "SEA Source")
    (is (= 3 (-> (get-corp) :prompt first :base)) "Base trace should be reset to 3")))

(deftest diwan
  ;; Diwan - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)]}
               :runner {:deck ["Diwan"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Diwan")
    (click-prompt state :runner "HQ")
    (take-credits state :runner)
    (is (= 8 (:credit (get-corp))) "8 credits for corp at start of second turn")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (is (= 8 (:credit (get-corp))) "Diwan did not charge extra for install on another server")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (= 7 (:credit (get-corp))) "Diwan charged 1cr to install ice protecting the named server")
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (is (= 7 (:credit (get-corp))) "Diwan didn't charge to install another upgrade in root of HQ")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (= 5 (:credit (get-corp))) "Diwan charged 1cr + 1cr to install a second ice protecting the named server")
    (core/gain state :corp :click 1)
    (core/purge state :corp)
    (play-from-hand state :corp "Fire Wall" "HQ") ; 2cr cost from normal install cost
    (is (= "Diwan" (-> (get-runner) :discard first :title)) "Diwan was trashed from purge")
    (is (= 3 (:credit (get-corp))) "No charge for installs after Diwan purged")))

(deftest djinn
  ;; Djinn
  (testing "Hosted Chakana does not disable advancing agendas. Issue #750"
    (do-game
      (new-game {:corp {:deck ["Priority Requisition"]}
                 :runner {:deck ["Djinn" "Chakana"]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Djinn")
      (let [djinn (get-program state 0)
            agenda (get-content state :remote1 0)]
        (is agenda "Agenda was installed")
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (let [chak (first (:hosted (refresh djinn)))]
          (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
          ;; manually add 3 counters
          (core/add-counter state :runner (first (:hosted (refresh djinn))) :virus 3)
          (take-credits state :runner 2)
          (core/advance state :corp {:card agenda})
          (is (= 1 (get-counters (refresh agenda) :advancement)) "Agenda was advanced")))))
  (testing "Host a non-icebreaker program"
    (do-game
      (new-game {:runner {:deck ["Djinn" "Chakana"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (is (= 3 (core/available-mu state)))
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (is (= 3 (core/available-mu state)) "No memory used to host on Djinn")
        (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
        (is (= 1 (:credit (get-runner))) "Full cost to host on Djinn"))))
  (testing "Tutor a virus program"
    (do-game
      (new-game {:runner {:deck ["Djinn" "Parasite"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand after moving Parasite to deck")
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 0)
        (click-prompt state :runner (find-card "Parasite" (:deck (get-runner))))
        (is (= "Parasite" (:title (first (:hand (get-runner))))) "Djinn moved Parasite to hand")
        (is (= 2 (:credit (get-runner))) "1cr to use Djinn ability")
        (is (= 2 (:click (get-runner))) "1click to use Djinn ability")))))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game {:corp {:deck [(qty "Restructure" 3) (qty "Hedge Fund" 3)]}
               :runner {:id "Laramy Fisk: Savvy Investor"
                        :deck ["Equivocation" "Desperado"]}})
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (play-from-hand state :runner "Equivocation")
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :rd)
    (click-prompt state :runner "Laramy Fisk: Savvy Investor")
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:hand (get-corp)))) "Corp forced to draw by Fisk")
    (click-prompt state :runner "Yes") ; Equivocation prompt
    (click-prompt state :runner "Yes") ; force the draw
    (is (= 1 (:credit (get-runner))) "Runner gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-corp)))) "Corp forced to draw by Equivocation")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest false-echo
  ;; False Echo - choice for Corp
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
               :runner {:deck [(qty "False Echo" 3)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "False Echo")
    (play-from-hand state :runner "False Echo")
    (run-on state "Archives")
    (run-continue state)
    (let [echo1 (get-program state 0)
          echo2 (get-program state 1)]
      (card-ability state :runner echo1 0)
      (click-prompt state :corp "Add to HQ")
      (is (= 2 (count (:hand (get-corp)))) "Ice Wall added to HQ")
      (is (= 1 (count (:discard (get-runner)))) "False Echo trashed")
      (run-continue state)
      (card-ability state :runner echo2 0)
      (click-prompt state :corp "Rez")
      (is (:rezzed (get-ice state :archives 0)) "Ice Wall rezzed")
      (is (= 2 (count (:discard (get-runner)))) "False Echo trashed"))))

(deftest gravedigger
  ;; Gravedigger - Gain counters when Corp cards are trashed, spend click-counter to mill Corp
  (do-game
    (new-game {:corp {:deck [(qty "Launch Campaign" 2) (qty "Enigma" 2)]}
               :runner {:deck ["Gravedigger"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Gravedigger")
    (let [gd (get-program state 0)]
      (core/trash state :corp (get-content state :remote1 0))
      (is (= 1 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/trash state :corp (get-content state :remote2 0))
      (is (= 2 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (is (= 2 (count (:deck (get-corp)))))
      (card-ability state :runner gd 0)
      (is (= 1 (get-counters (refresh gd) :virus)) "Spent 1 counter from Gravedigger")
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (count (:deck (get-corp)))))
      (is (= 3 (count (:discard (get-corp)))) "Milled 1 card from R&D"))))

(deftest harbinger
  ;; Harbinger
  (testing "install facedown when Blacklist installed"
    (do-game
      (new-game {:corp {:deck ["Blacklist"]}
                 :runner {:deck ["Harbinger"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Harbinger")
      (core/trash state :runner (-> (get-runner) :rig :program first))
      (is (zero? (count (:discard (get-runner)))) "Harbinger not in heap")
      (is (-> (get-runner) :rig :facedown first :facedown) "Harbinger installed facedown"))))

(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Hyperdriver"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hyperdriver")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (:runner-phase-12 @state) "Runner in Step 1.2")
      (let [hyp (get-program state 0)]
        (card-ability state :runner hyp 0)
        (core/end-phase-12 state :runner nil)
        (is (= 7 (:click (get-runner))) "Gained 3 clicks")
        (is (= 1 (count (:rfg (get-runner)))) "Hyperdriver removed from game"))))
  (testing "triggering a Dhegdeered Hyperdriver should not grant +3 MU"
    (do-game
      (new-game {:runner {:deck ["Hyperdriver" "Dhegdheer"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dhegdheer")
      (let [dheg (get-program state 0)]
        (card-ability state :runner dheg 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "0 MU used by Hyperdriver hosted on Dhegdheer")
        (is (= 2 (:click (get-runner))) "2 clicks used")
        (is (= 3 (:credit (get-runner))) "2 credits used")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (let [hyp (first (:hosted (refresh dheg)))]
          (card-ability state :runner hyp 0)
          (core/end-phase-12 state :runner nil)
          (is (= 7 (:click (get-runner))) "Used Hyperdriver")
          (is (= 4 (core/available-mu state)) "Still 0 MU used after Hyperdriver removed from game"))))))

(deftest imp
  ;; Imp
  (testing "Full test"
    (letfn [(imp-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:deck ["Imp"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Imp")
                (run-empty-server state "HQ")
                (click-prompt state :runner "[Imp]: Trash card")
                (is (= 1 (count (:discard (get-corp)))))))]
      (doall (map imp-test
                  ["Hostile Takeover"
                   "Dedicated Response Team"
                   "Beanstalk Royalties"
                   "Ice Wall"
                   "Oberth Protocol"]))))
  (testing "vs an ambush"
    (do-game
      (new-game {:corp {:deck ["Prisec"]}
                 :runner {:deck ["Imp" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Prisec" "New remote")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))
            tags (count-tags state)
            grip (count (:hand (get-runner)))
            archives (count (:discard (get-corp)))]
        (play-from-hand state :runner "Imp")
        (run-empty-server state :remote1)
        (click-prompt state :corp "Yes")
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= 2 (- credits (:credit (get-corp)))) "Corp paid 2 for Prisec")
        (is (= 1 (- (count-tags state) tags)) "Runner has 1 tag")
        (is (= 2 (- grip (count (:hand (get-runner))))) "Runner took 1 meat damage")
        (is (= 1 (- (count (:discard (get-corp))) archives)) "Used Imp to trash Prisec"))))
  (testing "vs The Future Perfect"
    ;; Psi-game happens on access [5.5.1], Imp is a trash ability [5.5.2]
    (do-game
      (new-game {:corp {:deck ["The Future Perfect"]}
                 :runner {:deck ["Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (testing "Access, corp wins psi-game"
        (run-empty-server state "HQ")
        ;; Should access TFP at this point
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (click-prompt state :runner "[Imp]: Trash card")
        (take-credits state :runner)
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP")
        (core/move state :corp (find-card "The Future Perfect" (:discard (get-corp))) :hand))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Access, runner wins psi-game"
        (run-empty-server state "HQ")
        ;; Access prompt for TFP
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        ;; Fail psi game
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP"))))
  (testing "vs cards in Archives"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Imp"]}})
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :discard)
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (run-empty-server state "Archives")
      (is (= ["Steal"] (->> (get-runner) :prompt first :choices)) "Should only get the option to steal Hostile on access in Archives"))))

(deftest incubator
  ;; Incubator - Gain 1 virus counter per turn; trash to move them to an installed virus program
  (do-game
    (new-game {:runner {:deck ["Incubator" "Datasucker"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Datasucker")
    (play-from-hand state :runner "Incubator")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [ds (get-program state 0)
          incub (get-program state 1)]
      (is (= 1 (get-counters (refresh incub) :virus)) "Incubator gained 1 virus counter")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh incub) :virus)) "Incubator has 2 virus counters")
      (card-ability state :runner incub 0)
      (click-card state :runner ds)
      (is (= 2 (get-counters (refresh ds) :virus)) "Datasucker has 2 virus counters moved from Incubator")
      (is (= 1 (count (get-program state))))
      (is (= 1 (count (:discard (get-runner)))) "Incubator trashed")
      (is (= 3 (:click (get-runner)))))))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game {:corp {:deck ["Snowflake"]}
               :runner {:deck ["Ixodidae" "Lamprey"]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Corp at 7 credits")
    (play-from-hand state :runner "Ixodidae")
    (play-from-hand state :runner "Lamprey")
    (is (= 3 (:credit (get-runner))) "Runner paid 3 credits to install Ixodidae and Lamprey")
    (run-on state :hq)
    (let [s (get-ice state :hq 0)]
      (core/rez state :corp s)
      (card-subroutine state :corp s 0)
      (is (prompt-is-card? state :corp s) "Corp prompt is on Snowflake")
      (is (prompt-is-card? state :runner s) "Runner prompt is on Snowflake")
      (is (= 6 (:credit (get-corp))) "Corp paid 1 credit to rezz Snowflake")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Corp paid 1 credit to psi game")
      (is (= 2 (:credit (get-runner))) "Runner did not gain 1 credit from Ixodidae when corp spent on psi game")
      (run-continue state)
      (run-successful state)
      (is (= 4 (:credit (get-corp))) "Corp lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-runner))) "Runner gains 1 credit from Ixodidae due to Lamprey"))))

(deftest kyuban
  ;; Kyuban
  (testing "Gain creds when passing a piece of ice, both when rezzed and when unrezzed."
    (do-game
      (new-game {:corp {:deck [(qty "Lockdown" 3)]}
                 :runner {:deck [(qty "Kyuban" 1)]}})
      (play-from-hand state :corp "Lockdown" "HQ")
      (play-from-hand state :corp "Lockdown" "Archives")
      (let [ld1 (get-ice state :archives 0)
            ld2 (get-ice state :hq 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Kyuban")
        (click-card state :runner ld1)
        (let [starting-creds (:credit (get-runner))]
          (run-on state "HQ")
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= starting-creds (:credit (get-runner))) "Gained no money for passing other ice")
          (core/jack-out state :runner nil)
          (run-on state "Archives")
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= (+ starting-creds 2) (:credit (get-runner)))
              "Gained 2 creds for passing unrezzed host ice"))
        (let [starting-creds-2 (:credit (get-runner))]
          (core/jack-out state :runner nil)
          (run-on state "Archives")
          (core/rez state :corp ld1)
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= (+ starting-creds-2 2) (:credit (get-runner)))
              "Gained 2 creds for passing rezzed host ice")))))
  (testing "HB: Architects of Tomorrow interaction"
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Architects of Tomorrow"
                        :deck ["Eli 1.0"]}
                 :runner {:deck ["Kyuban"]}})
      (play-from-hand state :corp "Eli 1.0" "HQ")
      (let [eli (get-ice state :hq 0)]
        (core/rez state :corp eli)
        (take-credits state :corp)
        (play-from-hand state :runner "Kyuban")
        (click-card state :runner eli)
        (let [starting-creds (:credit (get-runner))]
          (run-on state "HQ")
          (core/no-action state :corp nil)
          (core/continue state :runner nil)
          (click-prompt state :corp "Done")
          (core/continue state :runner nil)
          (is (= (+ starting-creds 2) (:credit (get-runner)))
              "Only gained 2 credits for passing Eli"))))))

(deftest lamprey
  ;; Lamprey - Corp loses 1 credit for each successful HQ run; trashed on purge
  (do-game
    (new-game {:runner {:deck ["Lamprey"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Lamprey")
    (let [lamp (get-program state 0)]
      (run-empty-server state :hq)
      (is (= 7 (:credit (get-corp))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 6 (:credit (get-corp))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 5 (:credit (get-corp))) "Corp lost 1 credit")
      (take-credits state :runner)
      (core/purge state :corp)
      (is (empty? (get-program state)) "Lamprey trashed by purge"))))

(deftest leprechaun
  ;; Leprechaun - hosting a breaker with strength based on unused MU should calculate correctly
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Adept" "Leprechaun"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Leprechaun")
      (play-from-hand state :runner "Adept")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (let [lep (get-program state 0)
            adpt (get-program state 1)]
        (is (= 3 (:current-strength (refresh adpt))) "Adept at 3 strength individually")
        (card-ability state :runner lep 1)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh lep)))]
          (is (= 3 (core/available-mu state)) "1 MU used")
          (is (= 5 (:current-strength (refresh hosted-adpt))) "Adept at 5 strength hosted")))))
  (testing "Keep MU the same when hosting or trashing hosted programs"
    (do-game
      (new-game {:runner {:deck ["Leprechaun" "Hyperdriver" "Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Leprechaun")
      (let [lep (get-program state 0)]
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not deducted from available MU")
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Imp" (:hand (get-runner))))
        (is (= 1 (:click (get-runner))))
        (is (zero? (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Imp 1 MU not deducted from available MU")
        ;; Trash Hyperdriver
        (core/move state :runner (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not added to available MU")
        (core/move state :runner (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
        (is (= 3 (core/available-mu state)) "Imp 1 MU not added to available MU")))))

(deftest magnum-opus
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game {:runner {:deck ["Magnum Opus"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 2 (core/available-mu state)))
    (is (zero? (:credit (get-runner))))
    (let [mopus (get-program state 0)]
      (card-ability state :runner mopus 0)
      (is (= 2 (:credit (get-runner))) "Gain 2cr"))))

(deftest nyashia
  ;; Nyashia
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
               :runner {:deck ["Nyashia"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Nyashia")
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :runner "Yes")
    (is (= 2 (+ (get-in @state [:runner :rd-access])
                (core/access-bonus-count (:run @state) :rd))))))

(deftest origami
  ;; Origami - Increases Runner max hand size
  (do-game
    (new-game {:runner {:deck [(qty "Origami" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Origami")
    (is (= 6 (core/hand-size state :runner)))
    (play-from-hand state :runner "Origami")
    (is (= 9 (core/hand-size state :runner)) "Max hand size increased by 2 for each copy installed")))

(deftest paintbrush
  ;; Paintbrush - Give rezzed ICE a chosen subtype until the end of the next run
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Paintbrush"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Paintbrush")
    (is (= 2 (core/available-mu state)))
    (let [iwall (get-ice state :hq 0)
          pb (get-program state 0)]
      (card-ability state :runner pb 0)
      (click-card state :runner iwall)
      (is (= 3 (:click (get-runner))) "Ice Wall not rezzed, so no click charged")
      (click-prompt state :runner "Done") ; cancel out
      (core/rez state :corp iwall)
      (card-ability state :runner pb 0)
      (click-card state :runner iwall)
      (click-prompt state :runner "Code Gate")
      (is (= 2 (:click (get-runner))) "Click charged")
      (is (true? (utils/has? (refresh iwall) :subtype "Code Gate")) "Ice Wall gained Code Gate")
      (run-empty-server state "Archives")
      (is (false? (utils/has? (refresh iwall) :subtype "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest parasite
  (testing "Basic functionality: Gain 1 counter every Runner turn"
    (do-game
      (new-game {:corp {:deck [(qty "Wraparound" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (core/rez state :corp wrap)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner wrap)
        (is (= 3 (core/available-mu state)) "Parasite consumes 1 MU")
        (let [psite (first (:hosted (refresh wrap)))]
          (is (zero? (get-counters psite :virus)) "Parasite has no counters yet")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (get-counters (refresh psite) :virus))
              "Parasite gained 1 virus counter at start of Runner turn")
          (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))
  (testing "Installed facedown w/ Apex"
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Parasite"]}})
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (click-card state :runner (find-card "Parasite" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No prompt to host Parasite")
      (is (= 1 (count (get-runner-facedown state))) "Parasite installed face down")))
  (testing "Installed on untrashable Architect should keep gaining counters past 3 and make strength go negative"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (let [arch (get-ice state :hq 0)]
        (core/rez state :corp arch)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner arch)
        (let [psite (first (:hosted (refresh arch)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 4 (get-counters (refresh psite) :virus)) "Parasite has 4 counters")
          (is (= -1 (:current-strength (refresh arch))) "Architect at -1 strength")))))
  (testing "Should stay on hosted card moved by Builder"
    (do-game
      (new-game {:corp {:deck [(qty "Builder" 3) "Ice Wall"]}
                 :runner {:deck [(qty "Parasite" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Builder" "Archives")
      (let [builder (get-ice state :archives 0)]
        (core/rez state :corp builder)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner builder)
        (let [psite (first (:hosted (refresh builder)))]
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 3 (:current-strength (refresh builder))) "Builder reduced to 3 strength")
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner))
        (let [orig-builder (refresh builder)]
          (card-ability state :corp builder 0)
          (click-prompt state :corp "HQ")
          (let [moved-builder (get-ice state :hq 1)]
            (is (= (:current-strength orig-builder) (:current-strength moved-builder)) "Builder's state is maintained")
            (let [orig-psite (dissoc (first (:hosted orig-builder)) :host)
                  moved-psite (dissoc (first (:hosted moved-builder)) :host)]
              (is (= orig-psite moved-psite) "Hosted Parasite is maintained"))
            (take-credits state :corp)
            (let [updated-builder (refresh moved-builder)
                  updated-psite (first (:hosted updated-builder))]
              (is (= 2 (:current-strength updated-builder)) "Builder strength still reduced")
              (is (= 2 (get-counters (refresh updated-psite) :virus)) "Parasite counters still incremented")))))))
  (testing "Use Hivemind counters when installed; instantly trash ICE if counters >= ICE strength"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Parasite"
                                 "Grimoire"
                                 "Hivemind"
                                 "Sure Gamble"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Sure Gamble")
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-program state 0)]
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind has 2 counters")
          (play-from-hand state :runner "Parasite")
          (click-card state :runner enig)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed instantly")
          (is (= 4 (core/available-mu state)))
          (is (= 2 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed")))))
  (testing "Trashed along with host ICE when its strength has been reduced to 0"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner enig)
        (let [psite (first (:hosted (refresh enig)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (is (= 1 (:current-strength (refresh enig))) "Enigma reduced to 1 strength")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed")
          (is (= 1 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed"))))))

(deftest pheromones
  ;; Pheromones ability shouldn't have a NullPointerException when fired with 0 virus counter
  (do-game
    (new-game {:runner {:deck ["Pheromones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Pheromones")
    (let [ph (get-program state 0)]
      (card-ability state :runner (refresh ph) 0)
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (refresh ph) :virus)) "Pheromones gained 1 counter")
      (card-ability state :runner (refresh ph) 0)))) ; this doesn't do anything, but shouldn't crash

(deftest plague
  ;; Plague
  (do-game
    (new-game {:corp {:deck ["Mark Yale"]}
               :runner {:deck ["Plague"]}})
    (play-from-hand state :corp "Mark Yale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Plague")
    (click-prompt state :runner "Server 1")
    (let [plague (get-program state 0)]
      (run-empty-server state "Server 1")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Server 1")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))

(deftest progenitor
  ;; Progenitor
  (testing "Hosting Hivemind, using Virus Breeding Ground. Issue #738"
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Virus Breeding Ground" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (play-from-hand state :runner "Virus Breeding Ground")
      (is (= 4 (core/available-mu state)))
      (let [prog (get-program state 0)
            vbg (get-resource state 0)]
        (card-ability state :runner prog 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "No memory used to host on Progenitor")
        (let [hive (first (:hosted (refresh prog)))]
          (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
          (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
          (is (zero? (:credit (get-runner))) "Full cost to host on Progenitor")
          (take-credits state :runner 1)
          (take-credits state :corp)
          (card-ability state :runner vbg 0) ; use VBG to transfer 1 token to Hivemind
          (click-card state :runner hive)
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
          (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))
  (testing "Keep MU the same when hosting or trashing hosted programs"
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (let [pro (get-program state 0)]
        (card-ability state :runner pro 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not deducted from available MU")
        ;; Trash Hivemind
        (core/move state :runner (find-card "Hivemind" (:hosted (refresh pro))) :discard)
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not added to available MU")))))

(deftest reaver
  ;; Reaver - Draw a card the first time you trash an installed card each turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Reaver" (qty "Fall Guy" 5)]}})
      (starting-hand state :runner ["Reaver" "Fall Guy"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Reaver")
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 [Credits] to trash") ; Trash PAD campaign
      (is (= 2 (count (:hand (get-runner)))) "Drew a card from trash of corp card")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Fall Guy")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      ; No draw from Fall Guy trash as Reaver already fired this turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      (take-credits state :runner)
      ; Draw from Fall Guy trash on corp turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")))
  (testing "with Freelance Coding Construct - should not draw when trash from hand #2671"
    (do-game
      (new-game {:runner {:deck [(qty "Reaver" 9) "Imp" "Snitch" "Freelance Coding Contract"]}})
      (starting-hand state :runner ["Reaver" "Imp" "Snitch" "Freelance Coding Contract"])
      (take-credits state :corp)
      (play-from-hand state :runner "Reaver")
      (is (= 3 (count (:hand (get-runner)))) "Four cards in hand")
      (is (= 3 (:credit (get-runner))) "3 credits")
      (play-from-hand state :runner "Freelance Coding Contract")
      (click-card state :runner "Snitch")
      (click-card state :runner "Imp")
      (click-prompt state :runner "Done")
      (is (= 7 (:credit (get-runner))) "7 credits - FCC fired")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand"))))

(deftest rezeki
  ;; Rezeki - gain 1c when turn begins
  (do-game
    (new-game {:runner {:deck ["Rezeki"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Rezeki")
    (take-credits state :runner)
    (let [credits (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (:credit (get-runner)) (+ credits 1)) "Gain 1 from Rezeki"))))

(deftest rng-key
  ;; RNG Key - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
  (testing "Basic behaviour - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 5) "Hedge Fund"]}
                 :runner {:deck ["RNG Key" (qty "Paperclip" 2)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (starting-hand state :runner ["RNG Key"])
      (take-credits state :corp)
      (testing "Gain 3 credits"
        (play-from-hand state :runner "RNG Key")
        (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "5")
        (click-prompt state :runner "Gain 3 [Credits]")
        (is (= 8 (:credit (get-runner))) "Gained 3 credits")
        (click-prompt state :runner "No action"))
      (testing "Do not trigger on second successful run"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (take-credits state :corp))
      (testing "Do not trigger on archives"
        (run-on state "Archives")
        (run-successful state))
      (testing "Do not get choice if trigger declined"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "No")
        (click-prompt state :runner "No action"))
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Do not gain credits / cards if guess incorrect"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner "No action"))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Gain 2 cards"
        (is (zero? (count (:hand (get-runner)))) "Started with 0 cards")
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "3")
        (click-prompt state :runner "Draw 2 cards")
        (click-prompt state :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Gained 2 cards")
        (is (zero? (count (:deck (get-runner)))) "Cards came from stack"))))
  (testing "Do not pay out if accessing an upgrade first -- regression test for #3150"
    (do-game
      (new-game {:corp {:deck ["Hokusai Grid" "Hedge Fund"]}
                 :runner {:deck ["RNG Key"]}})
      (play-from-hand state :corp "Hokusai Grid" "HQ")
      (take-credits state :corp)
      (testing "Gain 3 credits"
        (play-from-hand state :runner "RNG Key")
        (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner "Unrezzed upgrade in HQ")
        (is (= "You accessed Hokusai Grid." (-> (get-runner) :prompt first :msg))
            "No RNG Key prompt, straight to access prompt")
        (is (= 5 (:credit (get-runner))) "Gained no credits")))))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a program
  (do-game
    (new-game {:runner {:deck ["Scheherazade" "Cache"
                               "Inti" "Fall Guy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scheherazade")
    (let [sch (get-program state 0)]
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Inti" (:hand (get-runner))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-runner))) "Spent 1 click to install and host")
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (is (= 3 (core/available-mu state)) "Programs hosted on Scheh consume MU")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Cache" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Fall Guy" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-program")
      (is (= 1 (count (:hand (get-runner))))))))

(deftest self-modifying-code
  ;; Trash & pay 2 to search deck for a program and install it. Shuffle.
  (do-game
    (new-game {:runner {:deck [(qty "Self-modifying Code" 3) "Reaver"]}})
    (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
    (core/gain state :runner :credit 5)
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Self-modifying Code")
    (let [smc1 (get-program state 0)
          smc2 (get-program state 1)]
      (card-ability state :runner smc1 0)
      (click-prompt state :runner (find-card "Reaver" (:deck (get-runner))))
      (is (= 6 (:credit (get-runner))) "Paid 2 for SMC, 2 for install - 6 credits left")
      (is (= 1 (core/available-mu state)) "SMC MU refunded")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner smc2 0)
      (is (= 1 (count (:hand (get-runner)))) "1 card drawn due to Reaver before SMC program selection")
      (is (zero? (count (:deck (get-runner)))) "Deck empty"))))

(deftest sneakdoor-beta
  (testing "Gabriel Santiago, Ash on HQ should prevent Sneakdoor HQ access but still give Gabe credits. Issue #1138."
    (do-game
      (new-game {:corp {:deck ["Ash 2X3ZB9CY"]}
                 :runner {:id "Gabriel Santiago: Consummate Professional"
                          :deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (is (= 1 (:credit (get-runner))) "Sneakdoor cost 4 credits")
      (let [sb (get-program state 0)
            ash (get-content state :hq 0)]
        (core/rez state :corp ash)
        (card-ability state :runner sb 0)
        (run-successful state)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 3 (:credit (get-runner))) "Gained 2 credits from Gabe's ability")
        (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)) "Ash interrupted HQ access after Sneakdoor run")
        (is (= :hq (-> (get-runner) :register :successful-run first)) "Successful Run on HQ recorded"))))
  (testing "do not switch to HQ if Archives has Crisium Grid. Issue #1229."
    (do-game
      (new-game {:corp {:deck ["Crisium Grid" "Priority Requisition" "Private Security Force"]}
                 :runner {:deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (trash-from-hand state :corp "Priority Requisition")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (let [sb (get-program state 0)
            cr (get-content state :archives 0)]
        (core/rez state :corp cr)
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= :archives (get-in @state [:run :server 0])) "Crisium Grid stopped Sneakdoor Beta from switching to HQ"))))
  (testing "Allow Nerve Agent to gain counters. Issue #1158/#955"
    (do-game
      (new-game {:runner {:deck ["Sneakdoor Beta" "Nerve Agent"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Nerve Agent")
      (play-from-hand state :runner "Sneakdoor Beta")
      (let [nerve (get-program state 0)
            sb (get-program state 1)]
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= 1 (get-counters (refresh nerve) :virus)))
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= 2 (get-counters (refresh nerve) :virus))))))
  (testing "Grant Security Testing credits on HQ."
    (do-game
      (new-game {:runner {:deck ["Security Testing" "Sneakdoor Beta"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (is (= 3 (:credit (get-runner))))
      (take-credits state :corp)
      (let [sb (get-program state 0)]
        (click-prompt state :runner "HQ")
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (not (:run @state)) "Switched to HQ and ended the run from Security Testing")
        (is (= 5 (:credit (get-runner))) "Sneakdoor switched to HQ and earned Security Testing credits")))))

(deftest snitch
  ;; Snitch - Only works on unrezzed ice
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 2)]}
               :runner {:deck ["Snitch"]}})
    (play-from-hand state :corp "Quandary" "R&D")
    (play-from-hand state :corp "Quandary" "HQ")
    (let [hqice (get-ice state :hq 0)]
      (core/rez state :corp hqice))
    (take-credits state :corp)
    (play-from-hand state :runner "Snitch")
    (let [snitch (get-program state 0)]
      ;; unrezzed ice scenario
      (run-on state "R&D")
      (card-ability state :runner snitch 0)
      (is (prompt-is-card? state :runner snitch) "Option to jack out")
      (click-prompt state :runner "Yes")
      ;; rezzed ice scenario
      (run-on state "HQ")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out")
      ;; no ice scenario
      (run-on state "Archives")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out"))))

(deftest surfer
  ;; Surfer - Swap position with ice before or after when encountering a Barrier ICE
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Quandary"]}
               :runner {:deck ["Surfer"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Surfer")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits to install Surfer")
    (core/rez state :corp (get-ice state :hq 1))
    (run-on state "HQ")
    (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
    (let [surf (get-program state 0)]
      (card-ability state :runner surf 0)
      (click-card state :runner (get-ice state :hq 0))
      (is (= 1 (:credit (get-runner))) "Paid 2 credits to use Surfer")
      (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall now at position 1"))))

(deftest takobi
  ;; Takobi - 2 power counter to add +3 strength to a non-AI icebreaker for encounter
  (do-game
    (new-game {:corp {:deck ["Enigma"]}
               :runner {:deck ["Takobi" "Corroder" "Faust"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Takobi")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Faust")
    (let [tako (get-program state 0)
          corr (get-program state 1)
          faus (get-program state 2)]
      (dotimes [_ 3]
        (card-ability state :runner tako 0))
      (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")
      (run-on state "HQ")
      (card-ability state :runner tako 1)
      (is (empty? (:prompt (get-runner))) "No prompt for un-rezzed ice")
      (core/rez state :corp (get-ice state :hq 0))
      (card-ability state :runner tako 1)
      (click-card state :runner (refresh faus))
      (is (not-empty (:prompt (get-runner))) "Can't select AI breakers")
      (click-card state :runner (refresh corr))
      (is (empty? (:prompt (get-runner))) "Can select non-AI breakers")
      (is (= 5 (:current-strength (refresh corr))) "Corroder at +3 strength")
      (is (= 1 (get-counters (refresh tako) :power)) "1 counter on Takobi")
      (card-ability state :runner tako 1)
      (is (empty? (:prompt (get-runner))) "No prompt when too few power counters")
      (core/no-action state :corp nil)
      (run-continue state)
      (is (= 2 (:current-strength (refresh corr))) "Corroder returned to normal strength"))))

(deftest trypano
  (testing "Hivemind and Architect interactions"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 2)]}
                 :runner {:deck [(qty "Trypano" 2) "Hivemind"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-rezzed (get-ice state :hq 0)
            architect-unrezzed (get-ice state :rd 0)]
        (core/rez state :corp architect-rezzed)
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (game.core/get-card state architect-rezzed))
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (= 2 (core/available-mu state)) "Trypano consumes 1 MU"))
      ;; wait 4 turns to make both Trypanos have 4 counters on them
      (dotimes [n 4]
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes"))
      (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
      (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect is not trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect is not trashed")
      (play-from-hand state :runner "Hivemind") ; now Hivemind makes both Trypanos have 5 counters
      (is (zero? (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect was trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect was not trashed")
      (is (= 1 (count (:discard (get-runner)))) "Trypano went to discard")))
  (testing "Fire when Hivemind gains counters"
    (do-game
      (new-game {:corp {:deck ["Architect"]}
                 :runner {:deck ["Trypano" "Hivemind" (qty "Surge" 2)]}})
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-unrezzed (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
        (is (= 1 (count (get-ice state :rd))) "Unrezzed Architect is not trashed")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-program state 0)]
          (is (= 1 (get-counters (refresh hive) :virus)) "Hivemind starts with 1 virus counter")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 3 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 5 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters (now at 5)")
          (is (zero? (count (get-ice state :rd))) "Unrezzed Architect was trashed")
          (is (= 3 (count (:discard (get-runner)))) "Trypano went to discard"))))))

(deftest upya
  (do-game
    (new-game {:runner {:deck ["Upya"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Upya")
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 3 (get-counters (get-program state 0) :power)) "3 counters on Upya")
    (take-credits state :corp)
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 6 (get-counters (get-program state 0) :power)) "6 counters on Upya")
    (let [upya (get-program state 0)]
      (card-ability state :runner upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 2 (:click (get-runner))) "Gained 2 clicks")
      (card-ability state :runner upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "Upya not used more than once a turn")
      (is (= 2 (:click (get-runner))) "Still at 2 clicks"))
    (take-credits state :runner)
    (take-credits state :corp)
    (let [upya (get-program state 0)]
      (card-ability state :runner upya 0)
      (is (zero? (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 5 (:click (get-runner))) "Gained 2 clicks"))))

(deftest wari
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Wari"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Wari")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Barrier")
    (click-card state :runner (get-ice state :rd 0))
    (is (= 1 (count (:discard (get-runner)))) "Wari in heap")
    (is (seq (get-in @state [:runner :prompt])) "Runner is currently accessing Ice Wall")))
