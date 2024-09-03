(ns jinteki.preconstructed)

(defn precon
  [name id deck]
  {:identity id
   :name name
   :format "Preconstructed"
   :cards deck})

(defn matchup
  [tr-inner tr-tag tr-desc corp runner]
  {:tr-inner tr-inner
   :tr-tag tr-tag
   :tr-desc tr-desc
   :corp corp
   :runner runner})

;; Concept: Worlds matchups/decks

;; System Gateway preconstructed decks
(def gateway-beginner-corp
  {:format "system-gateway"
   :identity {:title "The Syndicate: Profit over Principle" :side "Corp" :code 30077}
   :name "System Gateway Starter Corp"
   :cards [{:qty 3 :card "Offworld Office"}
           {:qty 2 :card "Send a Message"}
           {:qty 2 :card "Superconducting Hub"}
           {:qty 2 :card "Nico Campaign"}
           {:qty 2 :card "Regolith Mining License"}
           {:qty 2 :card "Urtica Cipher"}
           {:qty 2 :card "Government Subsidy"}
           {:qty 3 :card "Hedge Fund"}
           {:qty 2 :card "Seamless Launch"}
           {:qty 1 :card "Manegarm Skunkworks"}
           {:qty 2 :card "Brân 1.0"}
           {:qty 3 :card "Palisade"}
           {:qty 2 :card "Diviner"}
           {:qty 2 :card "Whitespace"}
           {:qty 2 :card "Karunā"}
           {:qty 2 :card "Tithe"}]})

(def gateway-intermediate-corp
  {:format "system-gateway"
   :identity {:title "The Syndicate: Profit over Principle" :side "Corp" :code 30077}
   :name "System Gateway Starter Corp"
   :cards [{:qty 3 :card "Offworld Office"}
           {:qty 2 :card "Send a Message"}
           {:qty 2 :card "Orbital Superiority"}
           {:qty 2 :card "Predictive Planogram"}
           {:qty 2 :card "Public Trail"}
           {:qty 1 :card "Retribution"}
           {:qty 1 :card "AMAZE Amusements"}
           {:qty 2 :card "Funhouse"}
           {:qty 2 :card "Superconducting Hub"}
           {:qty 2 :card "Nico Campaign"}
           {:qty 2 :card "Regolith Mining License"}
           {:qty 2 :card "Urtica Cipher"}
           {:qty 2 :card "Government Subsidy"}
           {:qty 3 :card "Hedge Fund"}
           {:qty 2 :card "Seamless Launch"}
           {:qty 1 :card "Manegarm Skunkworks"}
           {:qty 2 :card "Brân 1.0"}
           {:qty 3 :card "Palisade"}
           {:qty 2 :card "Diviner"}
           {:qty 2 :card "Whitespace"}
           {:qty 2 :card "Karunā"}
           {:qty 2 :card "Tithe"}]})

(def gateway-beginner-runner
  {:format "system-gateway"
   :identity {:title "The Catalyst: Convention Breaker" :side "Runner" :code 30076}
   :name "System Gateway Starter Runner"
   :cards [{:qty 2 :card "Creative Commission"}
           {:qty 3 :card "Jailbreak"}
           {:qty 2 :card "Overclock"}
           {:qty 3 :card "Sure Gamble"}
           {:qty 2 :card "Tread Lightly"}
           {:qty 2 :card "VRcation"}
           {:qty 1 :card "Docklands Pass"}
           {:qty 1 :card "Pennyshaver"}
           {:qty 1 :card "Red Team"}
           {:qty 2 :card "Smartware Distributor"}
           {:qty 2 :card "Telework Contract"}
           {:qty 1 :card "Verbal Plasticity"}
           {:qty 2 :card "Carmen"}
           {:qty 2 :card "Cleaver"}
           {:qty 2 :card "Mayfly"}
           {:qty 2 :card "Unity"}]})

(def gateway-intermediate-runner
  {:format "system-gateway"
   :identity {:title "The Catalyst: Convention Breaker" :side "Runner" :code 30076}
   :name "System Gateway Starter Runner"
   :cards [{:qty 2 :card "Creative Commission"}
           {:qty 3 :card "Jailbreak"}
           {:qty 2 :card "Overclock"}
           {:qty 2 :card "Mutual Favor"}
           {:qty 2 :card "Wildcat Strike"}
           {:qty 2 :card "DZMZ Optimizer"}
           {:qty 2 :card "Conduit"}
           {:qty 2 :card "Leech"}
           {:qty 3 :card "Sure Gamble"}
           {:qty 2 :card "Tread Lightly"}
           {:qty 2 :card "VRcation"}
           {:qty 1 :card "Docklands Pass"}
           {:qty 1 :card "Pennyshaver"}
           {:qty 1 :card "Red Team"}
           {:qty 2 :card "Smartware Distributor"}
           {:qty 2 :card "Telework Contract"}
           {:qty 1 :card "Verbal Plasticity"}
           {:qty 2 :card "Carmen"}
           {:qty 2 :card "Cleaver"}
           {:qty 2 :card "Mayfly"}
           {:qty 2 :card "Unity"}]})

(def system-gateway-beginner
  (matchup
    [:lobby.gateway-format.beginner "Beginner"]
    [:lobby.gateway-format.beginner "Beginner"]
    [:lobby.gateway-format.beginner-info
     "This lobby is using the System Gateway beginner decks for the Corporation and Runner. These decks are recommended for your first games. Games are played to 6 agenda points."]
    gateway-beginner-corp
    gateway-beginner-runner))

(def system-gateway-intermediate
  (matchup
    [:lobby.gateway-format.intermediate "Intermediate"]
    [:lobby.gateway-format.intermediate "Intermediate"]
    [:lobby.gateway-format.intermediate-info
     "This lobby is using the System Gateway intermediate decks for the Corporation and Runner. These decks have slightly more range than the beginner decks. Games are played to 7 agenda points."]
    gateway-intermediate-corp
    gateway-intermediate-runner))

;; Worlds 2012: Ben Marsh vs. Jeremy Z

(def worlds-2012-ben-corps
  (matchup
    [:preconstructed.worlds-2012-b "Worlds 2012: Ben Marsh (C) vs. Jeremy Z (R)"]
    [:preconstructed.worlds-2012-b-tag "Ben Marsh (C) vs. Jeremy Z (R)"]
    [:preconstructed.worlds-2012-info "TODO"]
    (precon "Ben Marsh - 2012: ETF"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code 1054}
            [{:qty 3 :card "Enigma"}
             {:qty 2 :card "Heimdall 1.0"}
             {:qty 2 :card "Tollbooth"}
             {:qty 3 :card "Victor 1.0"}
             {:qty 2 :card "Ichi 1.0"}
             {:qty 3 :card "Rototurret"}
             {:qty 2 :card "Archer"}
             {:qty 1 :card "Ice Wall"}
             {:qty 3 :card "Wall of Static"}
             {:qty 3 :card "Adonis Campaign"}
             {:qty 3 :card "Pad Campaign"}
             {:qty 2 :card "Project Junebug"}
             {:qty 2 :card "Aggressive Secretary"}
             {:qty 2 :card "Snare!"}
             {:qty 1 :card "Experiential Data"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Biotic Labor"}
             {:qty 3 :card "Private Security Force"}
             {:qty 3 :card "Accelerated Beta Test"}
             {:qty 3 :card "Priority Requisition"}])
    (precon "Jeremy Z - 2012: Criminal"
            {:title "Gabriel Santiago: Consummate Professional" :side "Runner" :code 1017}
            [{:qty 2 :card "Corroder"}
             {:qty 1 :card "Yog.0"}
             {:qty 1 :card "Ninja"}
             {:qty 2 :card "Femme Fatale"}
             {:qty 1 :card "Sneakdoor Beta"}
             {:qty 3 :card "Parasite"}
             {:qty 3 :card "Datasucker"}
             {:qty 3 :card "Desperado"}
             {:qty 2 :card "Bank Job"}
             {:qty 2 :card "Crash Space"}
             {:qty 3 :card "Armitage Codebusting"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Infiltration"}
             {:qty 3 :card "Easy Mark"}
             {:qty 3 :card "Account Siphon"}
             {:qty 3 :card "Inside Job"}
             {:qty 3 :card "Special Order"}
             {:qty 3 :card "Forged Activation Orders"}
             {:qty 1 :card "Stimhack"}])))

(def worlds-2012-ben-runs
  (matchup
    [:preconstructed.worlds-2012-a "Worlds 2012: Jeremy Z (C) vs. Ben Marsh (R)"]
    [:preconstructed.worlds-2012-a-tag "Jeremy Z (C) vs. Ben Marsh (R)"]
    [:preconstructed.worlds-2012-info "TODO"]
    (precon "Jeremy Z - 2012: Weyland"
            {:title "Weyland Consortium: Building a Better World" :side "Corp" :code 1093}
            [{:qty 3 :card "Priority Requisition."}
             {:qty 3 :card "Private Security Force"}
             {:qty 3 :card "Hostile Takeover"}
             {:qty 2 :card "Posted Bounty"}
             {:qty 3 :card "Ice Wall"}
             {:qty 3 :card "Wall of Static"}
             {:qty 3 :card "Enigma"}
             {:qty 3 :card "Shadow"}
             {:qty 3 :card "Archer"}
             {:qty 3 :card "Data Raven"}
             {:qty 3 :card "Hadrian's Wall"}
             {:qty 3 :card "Melange M. Corp"}
             {:qty 1 :card "Corporate Troubleshooter"}
             {:qty 2 :card "Snare!"}
             {:qty 2 :card "Archived Memories"}
             {:qty 3 :card "Beanstalk Royalties"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Scorched Earth"}])
    (precon "Ben Marsh - 2012: Gabe"
            {:title "Gabriel Santiago: Consummate Professional" :side "Runner" :code 1017}
            [{:qty 3 :card "Sneakdoor Beta"}
             {:qty 1 :card "Gordian Blade"}
             {:qty 2 :card "Corroder"}
             {:qty 2 :card "Ninja"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 1 :card "Crypsis"}
             {:qty 1 :card "Yog.0"}
             {:qty 3 :card "Forged Activation Orders"}
             {:qty 2 :card "Easy Mark"}
             {:qty 3 :card "Infiltration"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Special Order"}
             {:qty 3 :card "Diesel"}
             {:qty 3 :card "Account Siphon"}
             {:qty 3 :card "Inside Job"}
             {:qty 2 :card "Desperado"}
             {:qty 2 :card "Lemuria Codecraker"}
             {:qty 2 :card "Decoy"}
             {:qty 1 :card "Crashspace"}
             {:qty 3 :card "Armitage Codebusting"}
             {:qty 2 :card "Bank Job"}])))

;; Utility

(defn matchup-by-key
  [key]
  (condp = key
    ;; not indexed
    :beginner system-gateway-beginner
    :intermediate system-gateway-intermediate
    ;; indexed
    :worlds-2012-a worlds-2012-ben-runs
    :worlds-2012-b worlds-2012-ben-corps))

(def all-matchups
  "A set of all preconstructed matchups (by key).
  The frontend uses this to build a matchup table for play"
  #{:worlds-2012-a :worlds-2012-b})
