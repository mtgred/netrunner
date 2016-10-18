(ns test.cards.identities
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adam-directives
  ;; Adam - Allow runner to choose directives
  (do-game
    (new-game
      (default-corp)
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (is (= 4 (count (get-in @state [:runner :play-area]))) "All directives are in the runner's play area")
    (is (= 0 (count (get-in @state [:runner :hand]))))
    (prompt-select :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
    (is (= 3 (count (get-in @state [:runner :rig :resource]))) "3 directives were installed")
    (is (= 0 (count (get-in @state [:runner :play-area]))) "The play area is empty")
    (let [nat (find-card "Neutralize All Threats" (get-in @state [:runner :rig :resource]))
          sf (find-card "Safety First" (get-in @state [:runner :rig :resource]))
          abr (find-card "Always Be Running" (get-in @state [:runner :rig :resource]))]
      (is (and nat sf abr) "The chosen directives were installed"))))

(deftest adam-palana
  ;; Adam - Directives should not grant Pālanā credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (prompt-select :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
    (prompt-choice :corp "Keep")
    (prompt-choice :runner "Keep")
    (core/start-turn state :corp nil)
    (is (= 5 (:credit (get-corp))) "Pālanā does not gain credit from Adam's starting Directives")))

(deftest adam-advanceable-traps
  ;; Adam - Neutralize All Threats interaction with advanceable traps.
  (do-game
    (new-game
      (default-corp [(qty "Cerebral Overwriter" 3)])
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (prompt-select :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
    (prompt-choice :corp "Keep")
    (prompt-choice :runner "Keep")
    (core/start-turn state :corp nil)

    (play-from-hand state :corp "Cerebral Overwriter" "New remote")
    (advance state (get-content state :remote1 0) 2)
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-choice :runner "No") ; Dismiss prompt from non-exiled Find the Truth directive
    (prompt-choice :corp "Yes")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
    (is (= 1 (count (:discard (get-corp)))) "1 card in archives")))

(deftest andromeda
  ;; Andromeda - 9 card starting hand, 1 link
  (do-game
    (new-game
      (default-corp)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 1 (:link (get-runner))) "1 link")
    (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-mulligan
  ;; Andromeda - 9 card starting hand after mulligan
  (do-game
    (new-game
      (default-corp)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)])
      {:mulligan :runner})
    (is (= 1 (:link (get-runner))) "1 link")
    (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-palana
  ;; Andromeda - should not grant Palana credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 5 (:credit (get-corp))) "Palana does not gain credit from Andromeda's starting hand")))

(deftest apex-facedown-console
  ;; Apex - Allow facedown install of a second console. Issue #1326
  (do-game
    (new-game
      (default-corp)
      (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2)]))
    (take-credits state :corp)
    (prompt-choice :runner "Done") ; no facedown install on turn 1
    (play-from-hand state :runner "Heartbeat")
    (is (= 1 (count (get-in @state [:runner :rig :hardware]))))
    (take-credits state :runner)
    (take-credits state :corp)
    (prompt-select :runner (find-card "Heartbeat" (:hand (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rig :facedown]))) "2nd console installed facedown")))

(deftest cerebral-imaging-max-hand-size
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Has 13 credits")
    (is (= 13 (core/hand-size state :corp)) "Max hand size is 13")))

(deftest chronos-protocol
  ;; Chronos Protocol - Choose Runner discard for first net damage of a turn
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Neural EMP" 2)])
      (default-runner [(qty "Imp" 3)]))
    (play-from-hand state :corp "Pup" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [pup (get-ice state :hq 0)]
      (core/rez state :corp pup)
      (card-subroutine state :corp pup 0)
      (prompt-choice :corp "Yes")
      (let [imp (find-card "Imp" (:hand (get-runner)))]
        (prompt-choice :corp imp)
        (is (= 1 (count (:discard (get-runner)))))
        (card-subroutine state :corp pup 0)
        (is (empty? (:prompt (get-corp))) "No choice on second net damage")
        (is (= 2 (count (:discard (get-runner)))))
        (run-jack-out state)
        (take-credits state :runner)
        (core/move state :runner (find-card "Imp" (:discard (get-runner))) :hand)
        (play-from-hand state :corp "Neural EMP")
        (prompt-choice :corp "No")
        (is (= 2 (count (:discard (get-runner)))) "Damage dealt after declining ability")
        (play-from-hand state :corp "Neural EMP")
        (is (empty? (:prompt (get-corp))) "No choice after declining on first damage")
        (is (= 3 (count (:discard (get-runner)))))))))

(deftest edward-kim
  ;; Edward Kim - Trash first operation accessed each turn, but not if first one was in Archives
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 2) (qty "PAD Campaign" 1)])
      (make-deck "Edward Kim: Humanity's Hammer" [(qty "Eater" 1) (qty "Sure Gamble" 2)]))
    (play-from-hand state :corp "Hedge Fund")
    (trash-from-hand state :corp "PAD Campaign")
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-corp)))) "No operation trashed from HQ; accessed one in Archives first")
    (take-credits state :runner)
    (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :hand)
    (is (= 1 (count (:discard (get-corp)))))
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
    (take-credits state :runner)
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (play-from-hand state :runner "Eater")
    (let [eater (get-in @state [:runner :rig :program 0])]
      (run-on state "Archives")
      (card-ability state :runner eater 0) ; pretend to break a sub so no cards in Archives will be accessed
      (run-successful state)
      (is (= 3 (count (:discard (get-corp)))))
      (run-empty-server state "HQ")
      (is (= 4 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))

(deftest gabriel-santiago
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game
      (default-corp)
      (make-deck "Gabriel Santiago: Consummate Professional" [(qty "Easy Mark" 1)]))
    (take-credits state :corp)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-runner))) "No credits gained")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "No credits gained")))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (default-runner))
    (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))

(deftest grndl-valencia
  ;; GRNDL vs Valencia - only 1 bad pub at start
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))

(deftest haarpsichord-studios
  ;; Haarpsichord Studios - Prevent stealing more than 1 agenda per turn
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-runner [(qty "Gang Sign" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Gang Sign")
    (run-empty-server state "HQ")
    (prompt-choice :runner "Steal")
    (is (= 1 (:agenda-point (get-runner))))
    (run-empty-server state "HQ")
    (prompt-choice :runner "Steal")
    (is (= 1 (:agenda-point (get-runner))) "Second steal of turn prevented")
    (take-credits state :runner)
    (play-from-hand state :corp "15 Minutes" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (prompt-choice :runner "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Steal prevention didn't carry over to Corp turn")))

(deftest haarpsichord-studios-employee-strike
  ;; Haarpsichord Studios - Interactions with Employee Strike. Issue #1313.
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-runner [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :click 5)
    (run-empty-server state "HQ")
    (prompt-choice :runner "Steal")
    (is (= 1 (:agenda-point (get-runner))))
    (play-from-hand state :runner "Employee Strike")
    (run-empty-server state "HQ")
    (prompt-choice :runner "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Second steal not prevented")
    (play-from-hand state :runner "Scrubbed")
    (run-empty-server state "HQ")
    (prompt-choice :runner "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Third steal prevented")))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Stronger Together" [(qty "Eli 1.0" 1)])
      (default-runner))
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (core/rez state :corp eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest iain-stirling-credits
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game
      (default-corp [(qty "Breaking News" 1)])
      (make-deck "Iain Stirling: Retired Spook" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (take-credits state :corp)
      (is (= 1 (:agenda-point (get-corp))) "Corp gains 1 agenda point from Breaking News")
      (take-credits state :runner 1)
      (is (= 8 (:credit (get-runner))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-trash-cost
  ;; Industrial Genomics - Increase trash cost
  (do-game
    (new-game
      (make-deck "Industrial Genomics: Growing Solutions" [(qty "PAD Campaign" 3)
                                                           (qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (trash-from-hand state :corp "PAD Campaign")
    (trash-from-hand state :corp "PAD Campaign")
    (trash-from-hand state :corp "Hedge Fund")
    (trash-from-hand state :corp "Hedge Fund")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :corp pad)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 8 (core/trash-cost state :runner (refresh pad)))))))

(deftest jesminder-sareen-ability
  ;; Jesminder Sareen - avoid tags only during a run
  (do-game
    (new-game (default-corp [(qty "SEA Source" 1) (qty "Data Raven" 1)])
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Data Raven" "Archives")
    (take-credits state :corp)
    (let [dr (-> @state :corp :servers :archives :ices first)]
      (core/rez state :corp dr)
      (core/click-run state :runner {:server "Archives"})
      (card-ability state :corp dr 0)
      (is (= 0 (:tag (get-runner))) "Jesminder avoided first tag during the run")
      (card-ability state :corp dr 0)
      (is (= 1 (:tag (get-runner))) "Jesminder did not avoid the second tag during the run")
      (core/no-action state :corp nil)
      (core/continue state :runner nil)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (run-empty-server state "R&D") ; clear per-run buffer
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 2 (:tag (get-runner))) "Jesminder did not avoid the tag outside of a run"))))

(deftest jesminder-john-masanori
  ;; Jesminder Sareen - don't avoid John Masanori tag
  (do-game
    (new-game (default-corp)
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "John Masanori" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "John Masanori")
    (run-on state "HQ")
    (core/jack-out state :runner nil)
    (is (= 1 (:tag (get-runner))) "Jesminder did not avoid John Masanori tag")))

(deftest jinteki-biotech-brewery
  ;; Jinteki Biotech - Brewery net damage
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Braintrust" 1)])
      (default-runner)
      {:dont-start-turn true})
    (prompt-choice :corp "[The Brewery~brewery]")
    (core/start-turn state :corp nil)
    (card-ability state :corp (:identity (get-corp)) 1)
    (is (= 1 (count (:hand (get-runner)))) "Runner took 2 net damage from Brewery flip")))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Braintrust" 1)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Steal")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage from steal")))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-runner))
    (play-from-hand state :corp "Mental Health Clinic" "New remote")
    (take-credits state :corp)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (run-empty-server state "HQ")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")))

(deftest jinteki-replicating-perfection-employee-strike
  ;; Replicating Perfection - interaction with Employee Strike. Issue #1313.
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-runner [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (play-from-hand state :corp "Mental Health Clinic" "New remote")
    (take-credits state :corp)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (play-from-hand state :runner "Employee Strike")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")))

(deftest kate-mac-mccaffrey-discount
  ;; Kate 'Mac' McCaffrey - Install discount
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

(deftest kate-mac-mccaffrey-no-discount
  ;; Kate 'Mac' McCaffrey - No discount for 0 cost
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         [(qty "Magnum Opus" 1)
                          (qty "Self-modifying Code" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Magnum Opus")
    (is (= 0 (:credit (get-runner))) "No Kate discount on second program install")))

(deftest kate-mac-mccaffrey-discount-cant-afford
  ;; Kate 'Mac' McCaffrey - Can Only Afford With the Discount
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :corp)
    (core/lose state :runner :credit 1)
    (is (= 4 (:credit (get-runner))))
    (play-from-hand state :runner "Magnum Opus")
    (is (= 1 (count (get-in @state [:runner :rig :program]))) "Magnum Opus installed")
    (is (= 0 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

(deftest ken-tenma-run-event-credit
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game (default-corp)
              (make-deck "Ken \"Express\" Tenma: Disappeared Clone" [(qty "Account Siphon" 2)]))
    (take-credits state :corp)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit for first Run event")
    (prompt-choice :runner "Run ability")
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 16 (:credit (get-runner))) "No credit gained for second Run event")))

(deftest khan-vs-caprice
  ;; Khan - proper order of events when vs. Caprice
  (do-game
    (new-game
      (default-corp [(qty "Eli 1.0" 1) (qty "Caprice Nisei" 1)])
      (make-deck "Khan: Savvy Skiptracer" [(qty "Corroder" 1)]))
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (play-from-hand state :corp "Caprice Nisei" "Archives")
    (core/rez state :corp (get-content state :archives 0))
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (is (and (empty? (:prompt (get-corp)))
             (= 1 (count (:prompt (get-runner))))
             (= "Khan: Savvy Skiptracer" (-> (get-runner) :prompt first :card :title)))
        "Only Khan prompt showing")
    (prompt-select :runner (first (:hand (get-runner))))
    (is (find-card "Corroder" (-> (get-runner) :rig :program)) "Corroder installed")
    (is (= 4 (:credit (get-runner))) "1cr discount from Khan")
    (is (= "Caprice Nisei" (-> (get-runner) :prompt first :card :title)) "Caprice prompt showing")
    (prompt-choice :runner "0 [Credits]")
    (prompt-choice :corp "1 [Credits]")
    (is (not (:run @state)) "Run ended")))

(deftest leela-gang-sign-complicated
  ;; Leela Patel - complicated interaction with mutiple Gang Sign
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)
                                                                  (qty "Hostile Takeover" 1)
                                                                  (qty "Geothermal Fracking" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" [(qty "Gang Sign" 2)]))
    (play-from-hand state :corp "Project Atlas" "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Geothermal Fracking" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Gang Sign")
    (play-from-hand state :runner "Gang Sign")
    (take-credits state :runner)
    (score-agenda state :corp (get-content state :remote1 0))
    (prompt-choice :runner "Leela Patel: Trained Pragmatist")
    (prompt-select :runner (get-content state :remote2 0))
    (is (find-card "Hostile Takeover" (:hand (get-corp))) "Hostile Takeover returned to hand")
    (prompt-choice :runner "Gang Sign")
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-runner))) "Hostile Takeover stolen with Gang Sign")
    (prompt-select :runner (get-content state :remote3 0))
    (is (find-card "Geothermal Fracking" (:hand (get-corp))) "Geothermal Fracking returned to hand")
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-runner))) "Geothermal Fracking stolen with Gang Sign")
    (prompt-choice :runner "Done")))

(deftest leela-lingering-successful-run-prompt
  ;; Leela Patel - issues with lingering successful run prompt
  (do-game
    (new-game
      (make-deck "NBN: Making News" [(qty "Breaking News" 1) (qty "SanSan City Grid" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" []))
    (starting-hand state :corp ["SanSan City Grid"])
    (play-from-hand state :corp "SanSan City Grid" "New remote")
    (take-credits state :corp)
    (run-empty-server state :rd)
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :remote1 0))
    (is (not (:run @state)) "Run is over")))

(deftest leela-upgrades
  ;; Leela Patel - upgrades returned to hand in the middle of a run do not break the run. Issue #2008.
  (do-game
    (new-game (default-corp [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) (qty "Shock!" 1)])
              (make-deck "Leela Patel: Trained Pragmatist" [(qty "Sure Gamble" 1)]))
    (starting-hand state :corp ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (play-from-hand state :corp "Crisium Grid" "Archives")
    (play-from-hand state :corp "Crisium Grid" "R&D")
    (trash-from-hand state :corp "Project Atlas")
    (trash-from-hand state :corp "Shock!")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :hq 0))
    (is (not (get-content state :hq 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "R&D")
    (prompt-choice :runner "Card from deck")
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :rd 0))
    (is (not (get-content state :rd 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "Archives")
    (prompt-choice :runner "Shock!")
    (prompt-choice :runner "Project Atlas")
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :archives 0))
    (is (not (get-content state :archives 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")))

(deftest maxx-wyldside-start-of-turn
  ;; MaxX and Wyldside - using Wyldside during Step 1.2 should lose 1 click
  (do-game
    (new-game (default-corp)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                     (qty "Sure Gamble" 3)
                                                     (qty "Infiltration" 3)
                                                     (qty "Corroder" 3)
                                                     (qty "Eater" 3)]))
    (take-credits state :corp)
    (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
    (starting-hand state :runner ["Wyldside"])
    (play-from-hand state :runner "Wyldside")
    (take-credits state :runner 3)
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits at end of first turn")
    (is (find-card "Wyldside" (get-in @state [:runner :rig :resource])) "Wyldside was installed")
    (take-credits state :corp)
    (is (= 0 (:click (get-runner))) "Runner has 0 clicks")
    (is (:runner-phase-12 @state) "Runner is in Step 1.2")
    (let [maxx (get-in @state [:runner :identity])
          wyld (find-card "Wyldside" (get-in @state [:runner :rig :resource]))]
      (card-ability state :runner maxx 0)
      (card-ability state :runner wyld 0)
      (core/end-phase-12 state :runner nil)
      (is (= 4 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (= 3 (:click (get-runner))) "Wyldside caused 1 click to be lost")
      (is (= 3 (count (:hand (get-runner)))) "3 cards drawn total"))))

(deftest nasir-ability-basic
  ;; Nasir Ability - Basic
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Nasir Meidan: Cyber Explorer" []))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)

    (run-on state "HQ")
    (let [iwall (get-ice state :hq 0)
          nasir (get-in @state [:runner :identity])]
      (core/rez state :corp iwall)
      (is (= 5 (:credit (get-runner))) "Nasir Ability does not trigger automatically")
      (card-ability state :runner nasir 0)
      (is (= 1 (:credit (get-runner))) "Credits at 1 after Nasir ability trigger"))))

(deftest nasir-ability-xanadu
  ;; Nasir Ability - Xanadu
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 1)])
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Xanadu" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)

    (swap! state assoc-in [:runner :credit] 6)
    (play-from-hand state :runner "Xanadu")
    (run-on state "HQ")
    (let [iwall (get-in @state [:corp :servers :hq :ices 0])
          nasir (get-in @state [:runner :identity])]
      (core/rez state :corp iwall)
      (is (= 3 (:credit (get-runner))) "Pay 3 to install Xanadu")
      (card-ability state :runner nasir 0)
      (is (= 2 (:credit (get-runner))) "Gain 1 more credit due to Xanadu"))))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message - Trace to tag Runner when first installed Corp card is trashed
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 2)])
      (default-runner [(qty "Forger" 1)]))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Forger")
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Yes")
    (prompt-choice :corp "Yes")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (empty? (:prompt (get-runner))) "Forger can't avoid the tag")
    (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
    (run-empty-server state "Server 2")
    (prompt-choice :runner "Yes")
    (is (empty? (:prompt (get-corp))) "No trace chance on 2nd trashed card of turn")))

(deftest nbn-controlling-the-message-drt
  ;; NBN: Controlling the Message - Interaction with Dedicated Response Team
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 1) (qty "Dedicated Response Team" 1)])
      (default-runner))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (core/rez state :corp (get-content state :remote2 0))
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Yes")
    (prompt-choice :corp "Yes")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
    (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from DRT")))

(deftest new-angeles-sol-on-steal
  ;; New Angeles Sol - interaction with runner stealing agendas
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Paywall Implementation" 2) (qty "Breaking News" 1)])
      (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Paywall Implementation")
    (take-credits state :corp)
    (is (= 6 (:credit (get-corp))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-corp))) "Corp gained 1cr from successful run")
    (prompt-choice :runner "Steal")
    (prompt-choice :corp "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-corp))) "Paywall trashed before Sol triggers")
    (prompt-select :corp (find-card "Paywall Implementation" (:hand (get-corp))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall back in play")))

(deftest nisei-division
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game
      (make-deck "Nisei Division: The Next Generation" [(qty "Snowflake" 2)])
      (default-runner))
    (play-from-hand state :corp "Snowflake" "HQ")
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (let [s1 (get-in @state [:corp :servers :hq :ices 0])
          s2 (get-in @state [:corp :servers :hq :ices 1])]
      (run-on state "HQ")
      (core/rez state :corp s2)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s2 0)
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game")
      (core/no-action state :corp nil)
      (core/rez state :corp s1)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s1 0)
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game"))))

(deftest noise-ability
  ;; Noise: Hacker Extraordinaire - Ability
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)])
      (make-deck "Noise: Hacker Extraordinaire" [(qty "Datasucker" 1) (qty "Cache" 1) (qty "Sure Gamble" 1) (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]))
    (starting-hand state :runner ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-corp)))) "Corp should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-corp)))) "Corp deck should contain 3 cards")
    (take-credits state :corp)
    (is (= 0 (count (:discard (get-corp)))) "Archives started empty")
    (play-from-hand state :runner "Datasucker")
    (is (= 1 (count (:discard (get-corp)))) "Playing virus should cause card to be trashed from R&D")
    (is (= 4 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 1 (count (:discard (get-corp)))) "Playing non-virus should not cause card to be trashed from R&D")
    (core/click-draw state :runner nil)
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Clone Chip")
    (trash-from-hand state :runner "Cache")
    (trash-from-hand state :runner "Sharpshooter")
    (take-credits state :runner)
    ;; playing virus via Clone Chip on Corp's turn should trigger Noise ability
    (let [chip (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner chip 0)
      (prompt-select :runner (find-card "Cache" (:discard (get-runner))))
      (let [ds (get-in @state [:runner :rig :program 1])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing virus via Clone Chip on corp's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
    (let [chip-2 (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner chip-2 0)
      (prompt-select :runner (find-card "Sharpshooter" (:discard (get-runner))))
      (let [ss (get-in @state [:runner :rig :program 2])]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing non-virus via Clone Chip on corp's turn should not trigger Noise ability")))

(deftest null-ability
  ;; Null ability - once per turn
  (do-game
    (new-game
      (default-corp [(qty "Wraparound" 3)])
      (make-deck "Null: Whistleblower" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Wraparound" "HQ")
    (play-from-hand state :corp "Wraparound" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [null (get-in @state [:runner :identity])
          wrap1 (get-ice state :hq 0)
          wrap2 (get-ice state :hq 1)]
      (card-ability state :runner null 0)
      (is (empty? (:prompt (get-runner))) "Ability won't work on unrezzed ICE")
      (core/rez state :corp wrap2)
      (card-ability state :runner null 0)
      (prompt-select :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
      (run-continue state)
      (core/rez state :corp wrap1)
      (card-ability state :runner null 0)
      (is (empty? (:prompt (get-runner))) "Ability already used this turn")
      (run-jack-out state)
      (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))

(deftest null-trashed
  ;; Null ability - does not affect next ice when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-corp [(qty "Wraparound" 1) (qty "Spiderweb" 1)])
      (make-deck "Null: Whistleblower" [(qty "Parasite" 3)]))
    (play-from-hand state :corp "Spiderweb" "HQ")
    (play-from-hand state :corp "Wraparound" "HQ")
    (take-credits state :corp)
    (core/gain state :corp :credit 10)
    (let [null (get-in @state [:runner :identity])
          spider (get-ice state :hq 0)
          wrap (get-ice state :hq 1)]
      (core/rez state :corp spider)
      (core/rez state :corp wrap)
      (play-from-hand state :runner "Parasite")
      (prompt-select :runner (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner null 0)
      (prompt-select :runner (first (:hand (get-runner))))
      (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Null")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null"))))

(deftest omar-ability
  ;; Omar Keung - Make a successful run on the chosen server once per turn
  (do-game
    (new-game
      (default-corp)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (let [omar (get-in @state [:runner :identity])]
      (card-ability state :runner omar 0)
      (run-successful state)
      (prompt-choice :runner "HQ")
      (is (= [:hq] (-> (get-runner) :register :successful-run)))
      (is (= "You accessed Hedge Fund" (-> (get-runner) :prompt first :msg)))
      (prompt-choice :runner "OK")
      (is (= 3 (:click (get-runner))))
      (card-ability state :runner omar 0)
      (is (= 3 (:click (get-runner))))
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :rd)
      (is (= [:rd] (-> (get-runner) :register :successful-run)))
      (card-ability state :runner omar 0)
      (run-successful state)
      (prompt-choice :runner "HQ")
      (is (= [:hq :rd] (-> (get-runner) :register :successful-run))))))

(deftest omar-ash
  ;; Omar Keung - Ash prevents access, but not successful run
  (do-game
    (new-game
      (default-corp [(qty "Ash 2X3ZB9CY" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
    (take-credits state :corp)
    (let [omar (get-in @state [:runner :identity])
          ash (get-content state :hq 0)]
      (core/rez state :corp ash)
      (card-ability state :runner omar 0)
      (run-successful state)
      (prompt-choice :runner "HQ")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)))
      (is (= :hq (-> (get-runner) :register :successful-run first))))))

(deftest omar-crisium-grid
  ;; Omar Keung - Crisium Grid prevents prompt
  (do-game
    (new-game
      (default-corp [(qty "Crisium Grid" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Crisium Grid" "Archives")
    (take-credits state :corp)
    (let [omar (get-in @state [:runner :identity])
          cr (get-content state :archives 0)]
      (core/rez state :corp cr)
      (card-ability state :runner omar 0)
      (run-successful state)
      (is (= (:cid cr) (-> (get-runner) :prompt first :card :cid)))
      (is (empty? (-> (get-runner) :register :successful-run)))
      (is (= :archives (get-in @state [:run :server 0]))))))

(deftest omar-medium
  ;; Omar Keung - When selecting R&D, ability adds counters to Medium
  (do-game
    (new-game
      (default-corp)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Medium" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Medium")
    (let [omar (get-in @state [:runner :identity])
          medium (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner omar 0)
      (run-successful state)
      (prompt-choice :runner "R&D")
      (is (= 1 (get-counters (refresh medium) :virus))))))

(deftest omar-nerve-agent
  ;; Omar Keung - When selecting HQ, ability adds counters to Nerve Agent
  (do-game
    (new-game
      (default-corp)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Nerve Agent" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Nerve Agent")
    (let [omar (get-in @state [:runner :identity])
          nerve (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner omar 0)
      (run-successful state)
      (prompt-choice :runner "HQ")
      (is (= 1 (get-counters (refresh nerve) :virus))))))

(deftest quetzal-ability
  ;; Quetzal ability- once per turn
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Quetzal: Free Spirit" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [q (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)
          qdef (core/card-def (get-in @state [:runner :identity]))]
      (core/rez state :corp iwall)
      (card-ability state :runner q 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (not (last-log-contains? state (get-in qdef [:abilities 0 :msg])))
          "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (core/jack-out state :runner nil))))

(deftest reina-rez-cost-increase
  ;; Reina Roja - Increase cost of first rezzed ICE
  (do-game
    (new-game
      (default-corp [(qty "Quandary" 3)])
      (make-deck "Reina Roja: Freedom Fighter" []))
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (core/rez state :corp quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))

(deftest rielle-kit-peddler-ability
  ;; Rielle "Kit" Peddler - Give ice code gate
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 2)])
              (make-deck "Rielle \"Kit\" Peddler: Transhuman" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [k (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)]
      (core/rez state :corp iwall)
      (card-ability state :runner k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has code gate"))))

(deftest silhouette-temujin-weirdness
  ;; Silhouette - broken interaction with other successful-run triggers. Issue #1968.
  (do-game
    (new-game
      (default-corp [(qty "PAD Campaign" 1) (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)])
      (make-deck "Silhouette: Stealth Operative" [(qty "Temüjin Contract" 1) (qty "Desperado" 1)]))
    (starting-hand state :corp ["Hedge Fund" "PAD Campaign"])
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Temüjin Contract")
    (prompt-choice :runner "HQ")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state :hq)
    (prompt-choice :runner "Temüjin Contract")
    (prompt-select :runner (get-content state :remote1 0))
    (prompt-choice :runner "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
    (is (= 8 (:credit (get-runner))) "Gained 4cr")

    ;; second run
    (run-empty-server state :hq)
    (prompt-choice :runner "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 12 (:credit (get-runner))) "Gained 4cr")
    (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin")))

(deftest spark-advertisements
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game
      (make-deck "Spark Agency: Worldswide Reach" [(qty "Launch Campaign" 2)])
      (default-runner))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [lc1 (get-content state :remote1 0)
          lc2 (get-content state :remote2 0)]
      (core/rez state :corp lc1)
      (is (= 4 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (take-credits state :corp)
      (run-on state "Server 1")
      (core/rez state :corp lc2)
      (is (= 3 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Runner turn)"))))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward - Ability
  (do-game
    (new-game
      (make-deck "Strategic Innovations: Future Forward" [(qty "Hedge Fund" 2)
                                                          (qty "Eli 1.0" 2)
                                                          (qty "Crick" 2)])
      (default-runner))
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Crick" "New remote")
    (let [i1 (get-ice state :remote1 0)
          i2 (get-ice state :remote2 0)]
      (take-credits state :corp 0)
      (take-credits state :runner)
      (core/rez state :corp i1)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (count (:prompt (get-corp)))) "Corp prompted to trigger Strategic Innovations")
      (prompt-select :corp (first (:discard (get-corp))))
      (is (empty? (:discard (get-corp))) "Hedge Fund moved back to R&D")
      (take-credits state :corp)
      (core/rez state :corp i2)
      (take-credits state :runner)
      (is (= 0 (count (:prompt (get-corp))))
          "Corp not prompted to trigger Strategic Innovations"))))

(deftest titan-agenda-counter
  ;; Titan Transnational - Add a counter to a scored agenda
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)])
      (default-runner))
    (play-from-hand state :corp "Project Atlas" "New remote")
    (let [atl (get-content state :remote1 0)]
      (core/gain state :corp :click 1)
      (core/advance state :corp {:card (refresh atl)})
      (core/advance state :corp {:card (refresh atl)})
      (core/advance state :corp {:card (refresh atl)})
      (core/score state :corp {:card (refresh atl)})
      (let [scored (get-in @state [:corp :scored 0])]
        (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))

(deftest titan-corporate-sales-team
  ;; Titan, only use one counter of Corporate Sales Team
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Corporate Sales Team" 1) (qty "Mark Yale" 1)])
      (default-runner))
    (play-from-hand state :corp "Corporate Sales Team" "New remote")
    (play-from-hand state :corp "Mark Yale" "New remote")
    (let [cst (get-content state :remote1 0)
          my (get-content state :remote2 0)]
      (core/gain state :corp :click 3)
      (core/advance state :corp {:card (refresh cst)})
      (core/advance state :corp {:card (refresh cst)})
      (core/advance state :corp {:card (refresh cst)})
      (core/advance state :corp {:card (refresh cst)})
      (core/score state :corp {:card (refresh cst)})
      (let [scored (get-in @state [:corp :scored 0])]
        (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
        (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
        (core/rez state :corp my)
        (card-ability state :corp my 1)
        (prompt-select :corp (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
        (card-ability state :corp my 1)
        (prompt-select :corp (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")))))

(deftest whizzard
  ;; Whizzard - Recurring credits
  (do-game
    (new-game (default-corp) (make-deck "Whizzard: Master Gamer" ["Sure Gamble"]))

    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :runner (:identity (get-runner)) 0)))]
      (is (changes-credits (get-runner) 1 (click-whizzard 1)))
      (is (changes-credits (get-runner) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")

      (take-credits state :corp)
      (is (changes-credits (get-runner) 3 (click-whizzard 3)) "Credits reset at start of Runner's turn")

      (take-credits state :runner)
      (is (changes-credits (get-runner) 0 (click-whizzard 1)) "Credits don't reset at start of Corp's turn"))))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced - Ability
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 3)])
              (make-deck "Wyvern: Chemically Enhanced" [(qty "Sure Gamble" 2)
                                                        (qty "Corroder" 1)
                                                        (qty "Clone Chip" 1)
                                                        (qty "Easy Mark" 1)]))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Easy Mark")
    (play-from-hand state :runner "Corroder")
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Yes")
    ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-runner))) "Easy Mark moved to deck")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (run-empty-server state "Server 2")
    (prompt-choice :runner "Yes")
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")))
