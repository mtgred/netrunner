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
    [:preconstructed.worlds-2012-a "Worlds 2012: Ben Marsh (C) vs. Jeremy Z (R)"]
    [:preconstructed.worlds-2012-a-tag "Ben Marsh (C) vs. Jeremy Z (R)"]
    [:preconstructed.worlds-2012-info "TODO"]
    (precon "Ben Marsh - 2012: Weyland"
            {:title "Weyland Consortium: Building a Better World" :side "Corp" :code 1093}
            [{:qty 3 :card "Priority Requisition"}
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
             {:qty 3 :card "Melange Mining Corp."}
             {:qty 1 :card "Corporate Troubleshooter"}
             {:qty 2 :card "Snare!"}
             {:qty 2 :card "Archived Memories"}
             {:qty 3 :card "Beanstalk Royalties"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Scorched Earth"}])
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
    [:preconstructed.worlds-2012-b "Worlds 2012: Jeremy Z (C) vs. Ben Marsh (R)"]
    [:preconstructed.worlds-2012-b-tag "Jeremy Z (C) vs. Ben Marsh (R)"]
    [:preconstructed.worlds-2012-info "TODO"]
    (precon "Jeremy Z - 2012: ETF"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code 1054}
            [{:qty 3 :card "Enigma"}
             {:qty 2 :card "Heimdall 1.0"}
             {:qty 2 :card "Tollbooth"}
             {:qty 3 :card "Viktor 1.0"}
             {:qty 2 :card "Ichi 1.0"}
             {:qty 3 :card "Rototurret"}
             {:qty 2 :card "Archer"}
             {:qty 1 :card "Ice Wall"}
             {:qty 3 :card "Wall of Static"}
             {:qty 3 :card "Adonis Campaign"}
             {:qty 3 :card "PAD Campaign"}
             {:qty 2 :card "Project Junebug"}
             {:qty 2 :card "Aggressive Secretary"}
             {:qty 2 :card "Snare!"}
             {:qty 1 :card "Experiential Data"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Biotic Labor"}
             {:qty 3 :card "Private Security Force"}
             {:qty 3 :card "Accelerated Beta Test"}
             {:qty 3 :card "Priority Requisition"}])
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
             {:qty 2 :card "Lemuria Codecracker"}
             {:qty 2 :card "Decoy"}
             {:qty 1 :card "Crashspace"}
             {:qty 3 :card "Armitage Codebusting"}
             {:qty 2 :card "Bank Job"}])))

;; worlds 2013: Jens Erickson (1) vs. Andrew Veen (2)
(def worlds-2013-jens-corps
  (matchup
    [:preconstructed.worlds-2013-a "Worlds 2013: Jens Erickson (C) vs. Andrew Veen (R)"]
    [:preconstructed.worlds-2013-a-tag "Jens Erickson (C) vs. Andrew Veen (R)"]
    [:preconstructed.worlds-2013-info "166 players attended worlds in 2013. The tournament was held in Minneapolis, MN, USA, and consisted of 6 swiss rounds into a top 32 cut. The legal cardpool consisted of cards up to Opening Moves."]
    (precon "Jens Erickson - 2013: Engineering the Future"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code 1054}
            [{:qty 3 :card "Accelerated Beta Test"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 3 :card "Efficiency Committee"}
             {:qty 1 :card "Director Haas' Pet Project"}
             {:qty 1 :card "Gila Hands Arcology"}
             {:qty 3 :card "Adonis Campaign"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 2 :card "Ash 2X3ZB9CY"}
             {:qty 2 :card "SanSan City Grid"}
             {:qty 3 :card "Biotic Labor"}
             {:qty 2 :card "Green Level Clearance"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 1 :card "Heimdall 1.0"}
             {:qty 2 :card "Wall of Static"}
             {:qty 2 :card "Ice Wall"}
             {:qty 1 :card "Viper"}
             {:qty 1 :card "Viktor 2.0"}
             {:qty 2 :card "Enigma"}
             {:qty 2 :card "Pop-up Window"}
             {:qty 1 :card "Tollbooth"}
             {:qty 3 :card "Rototurret"}
             {:qty 1 :card "Ichi 1.0"}
             {:qty 1 :card "Grim"}])
    (precon "Andrew Veen - 2013: Kate McCaffrey"
            {:title "Kate \"Mac\" McCaffrey: Digital Tinker" :side "Runner" :code 01033}
            [{:qty 3 :card "Modded"}
             {:qty 3 :card "Test Run"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Clone Chip"}
             {:qty 3 :card "R&D Interface"}
             {:qty 3 :card "Plascrete Carapace"}
             {:qty 3 :card "Desperado"}
             {:qty 3 :card "Atman"}
             {:qty 1 :card "Deus Ex"}
             {:qty 1 :card "Femme Fetale"}
             {:qty 3 :card "Self-modifying code"}
             {:qty 3 :card "Datasucker"}
             {:qty 1 :card "Parasite"}
             {:qty 3 :card "Kati Jones"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Professional Contacts"}])))

(def worlds-2013-jens-runs
  (matchup
    [:preconstructed.worlds-2013-b "Worlds 2013: Andrew Veen (C) vs. Jens Erickson (R)"]
    [:preconstructed.worlds-2013-b-tag "Andrew Veen (C) vs. Jens Erickson (R)"]
    [:preconstructed.worlds-2013-info "166 players attended worlds in 2013. The tournament was held in Minneapolis, MN, USA, and consisted of 6 swiss rounds into a top 32 cut. The legal cardpool consisted of cards up to Opening Moves."]
    (precon "Andrew Veen - 2013: Making News"
            {:title "NBN: Making News" :side "Corp" :code 25104}
            [{:qty 3 :card "AstroScript Pilot Program"}
             {:qty 3 :card "Project Beale"}
             {:qty 3 :card "Breaking News"}
             {:qty 3 :card "Character Assassination"}
             {:qty 1 :card "Gila Hands Arcology"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 2 :card "Bernice Mai"}
             {:qty 3 :card "SanSan City Grid"}
             {:qty 3 :card "Closed Accounts"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Green Level Clearance"}
             {:qty 3 :card "Beanstalk Royalties"}
             {:qty 2 :card "TMI"}
             {:qty 1 :card "Wall of Static"}
             {:qty 2 :card "Ice Wall"}
             {:qty 3 :card "Pop-up Window"}
             {:qty 3 :card "Enigma"}
             {:qty 2 :card "Dracō"}
             {:qty 1 :card "Rototurret"}
             {:qty 3 :card "Caduceus"}])
    (precon "Jens Erickson - 2013: Andromeda"
            {:title "Andromeda: Dispossessed Ristie" :side "Runner" :code 2083}
            [{:qty 3 :card "Account Siphon"}
             {:qty 3 :card "Inside Job"}
             {:qty 3 :card "Special Order"}
             {:qty 3 :card "Emergency Shutdown"}
             {:qty 2 :card "Forged Activation Orders"}
             {:qty 2 :card "Easy Mark"}
             {:qty 2 :card "Hostage"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Desperado"}
             {:qty 2 :card "Plascrete Carapace"}
             {:qty 2 :card "R&D Interface"}
             {:qty 2 :card "Faerie"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 1 :card "Crypsis"}
             {:qty 2 :card "Corroder"}
             {:qty 1 :card "Mimic"}
             {:qty 1 :card "Yog.0"}
             {:qty 3 :card "Datasucker"}
             {:qty 1 :card "Kati Jones"}
             {:qty 1 :card "John Masanori"}
             {:qty 1 :card "Professional Contacts"}])))

;; worlds 2014: Dan D'Argenio vs. Minh Tran
(def worlds-2014-dan-d-corps
  (matchup
    [:preconstructed.worlds-2014-a "Worlds 2014: Dan D'Argenio (C) vs. Minh Tran (R)"]
    [:preconstructed.worlds-2014-a-tag "Dan D'Argenio (C) vs. Minh Tran (R)"]
    [:preconstructed.worlds-2014-info "238 players attended worlds in 2014. The tournament was held in Minneapolis, MN, USA, and consisted of 7 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Up and Over."]
    (precon "Dan D'Argenio - 2014: Honor and Perfection"
            {:title "Jinteki: Replicating Perfection" :side "Corp" :code 2031}
            [{:qty 3 :card "NAPD Contract"}
             {:qty 3 :card "Nisei MK II"}
             {:qty 3 :card "The Future Perfect"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 3 :card "Mental Health Clinic"}
             {:qty 3 :card "Sundew"}
             {:qty 3 :card "Celebrity Gift"}
             {:qty 2 :card "Enhanced Login Protocol"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Interns"}
             {:qty 1 :card "Ash 2X3ZB9CY"}
             {:qty 3 :card "Caprice Nisei"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 1 :card "Himitsu-Bako"}
             {:qty 1 :card "Wall of Thorns"}
             {:qty 1 :card "Wraparound"}
             {:qty 1 :card "Lotus Field"}
             {:qty 2 :card "Quandary"}
             {:qty 1 :card "Tollbooth"}
             {:qty 2 :card "Komainu"}
             {:qty 3 :card "Pup"}
             {:qty 3 :card "Tsurugi"}])
    (precon "Minh Tran - 2014: Andromedium"
            {:title "Andromeda: Dispossessed Ristie" :side "Runner" :code 2083}
            [{:qty 3 :card "Account Siphon"}
             {:qty 2 :card "Inside Job"}
             {:qty 2 :card "Special Order"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Emergency Shutdown"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 1 :card "Legwork"}
             {:qty 3 :card "Desperado"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 1 :card "HQ Interface"}
             {:qty 1 :card "Feedback Filter"}
             {:qty 2 :card "Corroder"}
             {:qty 1 :card "Mimic"}
             {:qty 1 :card "Yog.0"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 2 :card "Faerie"}
             {:qty 2 :card "Passport"}
             {:qty 2 :card "Datasucker"}
             {:qty 2 :card "Medium"}
             {:qty 1 :card "Sneakdoor Beta"}
             {:qty 2 :card "Bank Job"}
             {:qty 1 :card "Mr. Li"}
             {:qty 3 :card "Daily Casts"}
             {:qty 2 :card "John Masanori"}
             {:qty 2 :card "Security Testing"}])))

(def worlds-2014-dan-d-runs
  (matchup
    [:preconstructed.worlds-2014-b "Worlds 2014: Minh Tran (C) vs. Dan D'Argenio (R)"]
    [:preconstructed.worlds-2014-b-tag "Minh Tran (C) vs. Dan D'Argenio (R)"]
    [:preconstructed.worlds-2014-info "238 players attended worlds in 2014. The tournament was held in Minneapolis, MN, USA, and consisted of 7 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Up and Over."]
    (precon "Minh Tran - 2014: Personal Evolution"
            {:title "Jinteki: Personal Evolution" :side "Corp" :code 1067}
            [{:qty 3 :card "Fetal AI"}
             {:qty 3 :card "Gila Hands Arcology"}
             {:qty 3 :card "House of Knives"}
             {:qty 1 :card "Philotic Entanglement"}
             {:qty 2 :card "The Future Perfect"}
             {:qty 1 :card "Project Junebug"}
             {:qty 3 :card "Snare!"}
             {:qty 3 :card "Ronin"}
             {:qty 2 :card "Jackson Howard"}
             {:qty 3 :card "Psychic Field"}
             {:qty 1 :card "Shattered Remains"}
             {:qty 1 :card "Neural Katana"}
             {:qty 2 :card "Enigma"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 2 :card "Komainu"}
             {:qty 1 :card "Yagura"}
             {:qty 2 :card "Pup"}
             {:qty 3 :card "Neural EMP"}
             {:qty 1 :card "Scorched Earth"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Mushin No Shin"}
             {:qty 3 :card "Sweeps Week"}])
    (precon "Dan D'Argenio - 2014: Daily QT Andy"
            {:title "Andromeda: Dispossessed Ristie" :side "Runner" :code 2083}
            [{:qty 3 :card "Account Siphon"}
             {:qty 1 :card "Inside Job"}
             {:qty 3 :card "Special Order"}
             {:qty 1 :card "Infiltration"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Emergency Shutdown"}
             {:qty 2 :card "Quality Time"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 2 :card "Legwork"}
             {:qty 1 :card "Express Delivery"}
             {:qty 3 :card "Desperado"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 2 :card "R&D Interface"}
             {:qty 2 :card "Corroder"}
             {:qty 1 :card "Mimic"}
             {:qty 1 :card "Yog.0"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 3 :card "Faerie"}
             {:qty 1 :card "Passport"}
             {:qty 3 :card "Datasucker"}
             {:qty 2 :card "Kati Jones"}
             {:qty 1 :card "Same Old Thing"}
             {:qty 1 :card "Daily Casts"}
             {:qty 3 :card "Security Testing"}])))

;; Worlds 2015: Dan D'Argenio vs. Timmy Wong

(def worlds-2015-dan-d-corps
  (matchup
    [:preconstructed.worlds-2015-a "Worlds 2015: Dan D'Argenio (C) vs. Timmy Wong (R)"]
    [:preconstructed.worlds-2015-a-tag "Dan D'Argenio (C) vs. Timmy Wong (R)"]
    [:preconstructed.worlds-2015-info "269 players attended worlds in 2015. The tournament was held in Minneapolis, MN, USA, and consisted of 8 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Data and Destiny."]
    (precon "Dan D'Argenio - 2015: Foodcoatshop"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code 1054}
            [{:qty 3 :card "Accelerated Beta Test"}
             {:qty 1 :card "Project Vitruvius"}
             {:qty 3 :card "NAPD Contract"}
             {:qty 2 :card "Global Food Initiative"}
             {:qty 3 :card "Adonis Campaign"}
             {:qty 3 :card "Eve Campaign"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 3 :card "Ichi 1.0"}
             {:qty 1 :card "Tollbooth"}
             {:qty 2 :card "Enigma"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 2 :card "Ichi 2.0"}
             {:qty 3 :card "Architect"}
             {:qty 3 :card "Turing"}
             {:qty 2 :card "Archived Memories"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Ash 2X3ZB9CY"}
             {:qty 2 :card "Caprice Nisei"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "Breaker Bay Grid"}])
    (precon "Timmy Wong - 2015: The Morning After"
            {:title "Whizzard: Master Gamer" :side "Runner" :code 2001}
            [{:qty 2 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Test Run"}
             {:qty 3 :card "Inject"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 2 :card "Career Fair"}
             {:qty 3 :card "Clone Chip"}
             {:qty 2 :card "Turntable"}
             {:qty 2 :card "Corroder"}
             {:qty 1 :card "Mimic"}
             {:qty 1 :card "Yog.0"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 1 :card "Atman"}
             {:qty 1 :card "Medium"}
             {:qty 3 :card "Parasite"}
             {:qty 2 :card "Imp"}
             {:qty 2 :card "D4v1d"}
             {:qty 3 :card "Liberated Account"}
             {:qty 1 :card "Kati Jones"}
             {:qty 3 :card "Daily Casts"}
             {:qty 2 :card "Earthrise Hotel"}
             {:qty 3 :card "Street Peddler"}])))

(def worlds-2015-dan-d-runs
  (matchup
    [:preconstructed.worlds-2015-b "Worlds 2015: Timmy Wong (C) vs. Dan D'Argenio (R)"]
    [:preconstructed.worlds-2015-b-tag "Dan D'Argenio (C) vs. Timmy Wong (R)"]
    [:preconstructed.worlds-2015-info "269 players attended worlds in 2015. The tournament was held in Minneapolis, MN, USA, and consisted of 8 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Data and Destiny."]
    (precon "Timmy Wong - 2015: Yellow Shell"
            {:title "Near-Earth Hub: Broadcast Center" :side "Corp" :code 6005}
            [{:qty 3 :card "AstroScript Pilot Program"}
             {:qty 3 :card "Breaking News"}
             {:qty 2 :card "Project Beale"}
             {:qty 1 :card "TGTBT"}
             {:qty 3 :card "Explode-a-palooza"}
             {:qty 2 :card "Snare!"}
             {:qty 3 :card "PAD Campaign"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 1 :card "Psychic Field"}
             {:qty 1 :card "Shattered Remains"}
             {:qty 2 :card "Lily Lockwell"}
             {:qty 3 :card "News Team"}
             {:qty 1 :card "Data Raven"}
             {:qty 3 :card "Pop-up Window"}
             {:qty 1 :card "Wraparound"}
             {:qty 1 :card "Archangel"}
             {:qty 1 :card "Turnpike"}
             {:qty 1 :card "Closed Accounts"}
             {:qty 1 :card "Psychographics"}
             {:qty 1 :card "SEA Source"}
             {:qty 3 :card "Scorched Earth"}
             {:qty 2 :card "Midseason Replacements"}
             {:qty 2 :card "Sweeps Week"}
             {:qty 1 :card "SanSan City Grid"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "Product Placement"}])
    (precon "Dan D'Argenio - 2015: Radisson Cheese Plate"
            {:title "Valencia Estevez: The Angel of Cayambe" :side "Runner" :code 7030}
            [{:qty 2 :card "Account Siphon"}
             {:qty 3 :card "Blackmail"}
             {:qty 2 :card "Queen's Gambit"}
             {:qty 3 :card "Inject"}
             {:qty 1 :card "Turntable"}
             {:qty 3 :card "Faust"}
             {:qty 2 :card "D4v1d"}
             {:qty 3 :card "Joshua B."}
             {:qty 3 :card "Scrubber"}
             {:qty 3 :card "Same Old Thing"}
             {:qty 3 :card "Data Leak Reversal"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Fall Guy"}
             {:qty 1 :card "Hades Shard"}
             {:qty 3 :card "Street Peddler"}
             {:qty 3 :card "Off-Campus Apartment"}
             {:qty 3 :card "Drug Dealer"}
             {:qty 3 :card "Paparazzi"}
             {:qty 3 :card "Wireless Net Pavilion"}])))

;; Worlds 2016: Chris Dyer vs. Ben Ni

(def worlds-2016-chris-dyer-corps
  (matchup
    [:preconstructed.worlds-2016-a "Worlds 2016: Chris Dyer (C) vs. Benjamin Ni (R)"]
    [:preconstructed.worlds-2016-a-tag "Chris Dyer (C) vs. Benjamin Ni (R)"]
    [:preconstructed.worlds-2016-info "278 players attended worlds in 2016. The tournament was held in Minneapolis, MN, USA, and consisted of 9 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Escalation."]
    (precon "Chris Dyer - 2016: Snekbite"
            {:title "NBN: Controlling the Message" :side "Corp" :code 11017}
            [{:qty 1 :card "AstroScript Pilot Program"}
             {:qty 3 :card "Breaking News"}
             {:qty 3 :card "Project Beale"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 1 :card "PAD Campaign"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 3 :card "Sensie Actors Union"}
             {:qty 2 :card "Commercial Bankers Group"}
             {:qty 2 :card "Tollbooth"}
             {:qty 1 :card "Enigma"}
             {:qty 2 :card "Pop-up Window"}
             {:qty 2 :card "Archangel"}
             {:qty 3 :card "Resistor"}
             {:qty 2 :card "Turnpike"}
             {:qty 1 :card "Cobra"}
             {:qty 2 :card "Closed Accounts"}
             {:qty 1 :card "Psychographics"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 2 :card "Hard-Hitting News"}
             {:qty 2 :card "Exchange of Information"}
             {:qty 2 :card "SanSan City Grid"}
             {:qty 2 :card "Mumbad Virtual Tour"}])
    (precon "Benjamin Ni - 2016: Minh MaxX++"
            {:title "MaxX: Maximum Punk Rock" :side "Runner" :code 7029}
            [{:qty 3 :card "Déjà Vu"}
             {:qty 2 :card "Account Siphon"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Retrieval Run"}
             {:qty 1 :card "Levy AR Lab Access"}
             {:qty 2 :card "Inject"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 1 :card "Hacktivist Meeting"}
             {:qty 1 :card "Rumor Mill"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 2 :card "Eater"}
             {:qty 1 :card "Paperclip"}
             {:qty 2 :card "Joshua B."}
             {:qty 3 :card "Scrubber"}
             {:qty 3 :card "Same Old Thing"}
             {:qty 3 :card "Data Leak Reversal"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Fall Guy"}
             {:qty 2 :card "Paparazzi"}
             {:qty 2 :card "DDoS"}
             {:qty 3 :card "Wireless Net Pavilion"}])))

(def worlds-2016-chris-dyer-runs
  (matchup
    [:preconstructed.worlds-2016-b "Worlds 2016: Benjamin Ni (R) vs. Chris Dyer (C)"]
    [:preconstructed.worlds-2016-b-tag "Benjamin Ni (R) vs. Chris Dyer (C)"]
    [:preconstructed.worlds-2016-info "278 players attended worlds in 2016. The tournament was held in Minneapolis, MN, USA, and consisted of 9 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Escalation."]
    (precon "Benjamin Ni - 2016: Fiery Info"
            {:title "SYNC: Everything, Everywhere" :side "Corp" :code 9001}
            [{:qty 3 :card "Breaking News"}
             {:qty 1 :card "NAPD Contract"}
             {:qty 1 :card "15 Minutes"}
             {:qty 3 :card "Quantum Predictive Model"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 3 :card "Data Raven"}
             {:qty 2 :card "Pop-up Window"}
             {:qty 1 :card "Gutenberg"}
             {:qty 1 :card "Archangel"}
             {:qty 3 :card "Resistor"}
             {:qty 2 :card "Turnpike"}
             {:qty 1 :card "Archived Memories"}
             {:qty 2 :card "Closed Accounts"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 2 :card "24/7 News Cycle"}
             {:qty 2 :card "Hard-Hitting News"}
             {:qty 1 :card "Exchange of Information"}
             {:qty 2 :card "Observe and Destroy"}
             {:qty 2 :card "BOOM!"}])
    (precon "Chris Dyer - 2016: Papa Smurf"
            {:title "Whizzard: Master Gamer" :side "Runner" :code 2001}
            [{:qty 1 :card "Déjà Vu"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 1 :card "Retrieval Run"}
             {:qty 2 :card "Inject"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 2 :card "Employee Strike"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 1 :card "Net-Ready Eyes"}
             {:qty 2 :card "Obelus"}
             {:qty 2 :card "Mimic"}
             {:qty 2 :card "Yog.0"}
             {:qty 2 :card "Paperclip"}
             {:qty 2 :card "Datasucker"}
             {:qty 2 :card "Medium"}
             {:qty 3 :card "Parasite"}
             {:qty 1 :card "Progenitor"}
             {:qty 1 :card "Ice Carver"}
             {:qty 1 :card "Liberated Account"}
             {:qty 3 :card "Daily Casts"}
             {:qty 1 :card "Earthrise Hotel"}
             {:qty 3 :card "Street Peddler"}
             {:qty 3 :card "Temüjin Contract"}])))

;; Worlds 2017: ChaosJuggler vs. Gregory Tongue
(def worlds-2017-wilfy-corps
  (matchup
    [:preconstructed.worlds-2017-a "Worlds 2017: Wilfy Horig (C) vs. Gregory Tongue (R)"]
    [:preconstructed.worlds-2017-a-tag "Wilfy Horig (C) vs. Gregory Tongue (R)"]
    [:preconstructed.worlds-2017-info "233 players attended worlds in 2017. The tournament was held in Minneapolis, MN, USA, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Revised Core set."]
    (precon "ChaosJuggler - 2017: Sinson Reversed CI"
            {:title "Cerebral Imaging: Infinite Frontiers" :side "Corp" :code 3001}
            [{:qty 1 :card "Corporate Sales Team"}
             {:qty 3 :card "Efficiency Committee"}
             {:qty 2 :card "Elective Upgrade"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 3 :card "MCA Austerity Policy"}
             {:qty 3 :card "Reversed Accounts"}
             {:qty 3 :card "Biotic Labor"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 2 :card "IPO"}
             {:qty 1 :card "Scarcity of Resources"}
             {:qty 2 :card "Shipment from Tennin"}
             {:qty 3 :card "Ultraviolet Clearance"}
             {:qty 3 :card "Violet Level Clearance"}
             {:qty 2 :card "Bryan Stinson"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 1 :card "Bastion"}
             {:qty 2 :card "Vanilla"}
             {:qty 1 :card "Enigma"}
             {:qty 1 :card "Fairchild 2.0"}
             {:qty 3 :card "Fairchild 3.0"}
             {:qty 2 :card "Architect"}
             {:qty 1 :card "Ichi 1.0"}])
    (precon "Gregory Tongue - 2017: Aesops Hayley"
            {:title "Hayley Kaplan: Universal Scholar" :side "Runner" :code 8025}
            [{:qty 2 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Astrolabe"}
             {:qty 1 :card "Heartbeat"}
             {:qty 3 :card "Aesop's Pawnshop"}
             {:qty 1 :card "Councilman"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Professional Contacts"}
             {:qty 3 :card "Sacrificial Construct"}
             {:qty 2 :card "The Turning Wheel"}
             {:qty 1 :card "Brahman"}
             {:qty 1 :card "Cerberus \"Lady\" H1"}
             {:qty 1 :card "Cyber-Cypher"}
             {:qty 1 :card "Gordian Blade"}
             {:qty 1 :card "Inti"}
             {:qty 1 :card "Na'Not'K"}
             {:qty 3 :card "Cache"}
             {:qty 1 :card "Clot"}
             {:qty 1 :card "DaVinci"}
             {:qty 1 :card "Dhegdheer"}
             {:qty 3 :card "Hyperdriver"}
             {:qty 1 :card "Misdirection"}
             {:qty 2 :card "Paricia"}
             {:qty 3 :card "Self-modifying Code"}
             {:qty 1 :card "Trope"}])))

(def worlds-2017-wilfy-runs
  (matchup
    [:preconstructed.worlds-2017-b "Worlds 2017: Gregory Tongue (C) vs. Wilfy Horig (R)"]
    [:preconstructed.worlds-2017-b-tag "Gregory Tongue (C) vs. Wilfy Horig (R)"]
    [:preconstructed.worlds-2017-info "233 players attended worlds in 2017. The tournament was held in Minneapolis, MN, USA, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Revised Core set."]
    (precon "Gregory Tongue - 2017: No-Show Rewiring CI"
            {:title "Cerebral Imaging: Infinite Frontiers" :side "Corp" :code 3001}
            [{:qty 2 :card "Brain Rewiring"}
             {:qty 3 :card "Efficiency Committee"}
             {:qty 2 :card "Global Food Initiative"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 1 :card "Contract Killer"}
             {:qty 2 :card "Executive Boot Camp"}
             {:qty 1 :card "Jeeves Model Bioroids"}
             {:qty 3 :card "Archived Memories"}
             {:qty 1 :card "Audacity"}
             {:qty 2 :card "Best Defense"}
             {:qty 3 :card "Biotic Labor"}
             {:qty 1 :card "Consulting Visit"}
             {:qty 2 :card "Enforced Curfew"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Shipment from Kaguya"}
             {:qty 2 :card "Shipment from MirrorMorph"}
             {:qty 3 :card "Ultraviolet Clearance"}
             {:qty 3 :card "Violet Level Clearance"}
             {:qty 2 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "Vanilla"}
             {:qty 1 :card "Excalibur"}
             {:qty 1 :card "Loki"}
             {:qty 2 :card "Mother Goddess"}])
    (precon "ChaosJuggler - 2017: Laguna Lock Hayley"
            {:title "Hayley Kaplan: Universal Scholar" :side "Runner" :code 8025}
            [{:qty 3 :card "Indexing"}
             {:qty 1 :card "Information Sifting"}
             {:qty 1 :card "Levy AR Lab Access"}
             {:qty 1 :card "Mad Dash"}
             {:qty 3 :card "Peace in Our Time"}
             {:qty 1 :card "Scavenge"}
             {:qty 1 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Astrolabe"}
             {:qty 1 :card "Feedback Filter"}
             {:qty 1 :card "Artist Colony"}
             {:qty 1 :card "Beth Kilrain-Chang"}
             {:qty 1 :card "Citadel Sanctuary"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Fan Site"}
             {:qty 1 :card "Hunting Grounds"}
             {:qty 3 :card "Laguna Velasco District"}
             {:qty 3 :card "Sacrificial Construct"}
             {:qty 1 :card "The Shadow Net"}
             {:qty 1 :card "Atman"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 1 :card "Gordian Blade"}
             {:qty 1 :card "Inti"}
             {:qty 1 :card "Na'Not'K"}
             {:qty 1 :card "Clot"}
             {:qty 1 :card "Misdirection"}
             {:qty 2 :card "Self-modifying Code"}
             {:qty 2 :card "Tapwrm"}])))

;; Worlds 2018: Joe Schupp vs. Chris Dyer
(def worlds-2018-joe-corps
  (matchup
    [:preconstructed.worlds-2018-a "Worlds 2018: Joe Schupp (C) vs. Chris Dyer (R)"]
    [:preconstructed.worlds-2018-a-tag "Joe Schupp (C) vs. Chris Dyer (R)"]
    [:preconstructed.worlds-2018-info "403(!) players attended worlds in 2018. This is the final worlds championship to be run by FFG. The tournament was held in Minneapolis, MN, USA, and consisted of 9(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Reign and Reverie"]
    (precon "Joe Schupp - 2018: AMERICA CtM"
            {:title "NBN: Controlling the Message" :side "Corp" :code 11017}
            [{:qty 1 :card "15 Minutes"}
             {:qty 3 :card "AR-Enhanced Security"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Project Beale"}
             {:qty 1 :card "Quantum Predictive Model"}
             {:qty 3 :card "Commercial Bankers Group"}
             {:qty 3 :card "Daily Business Show"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Team Sponsorship"}
             {:qty 1 :card "Exchange of Information"}
             {:qty 3 :card "Hard-Hitting News"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Market Forces"}
             {:qty 1 :card "Preemptive Action"}
             {:qty 1 :card "Psychographics"}
             {:qty 3 :card "Calibration Testing"}
             {:qty 3 :card "Mumbad Virtual Tour"}
             {:qty 3 :card "IP Block"}
             {:qty 2 :card "Resistor"}
             {:qty 3 :card "Enigma"}
             {:qty 1 :card "Tollbooth"}
             {:qty 1 :card "Turnpike"}])
    (precon "Chris Dyer - 2018: Gooseberry MaxX"
            {:title "MaxX: Maximum Punk Rock" :side "Runner" :code 7029}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Hacktivist Meeting"}
             {:qty 1 :card "Indexing"}
             {:qty 3 :card "Inject"}
             {:qty 1 :card "Knifed"}
             {:qty 1 :card "Legwork"}
             {:qty 1 :card "Levy AR Lab Access"}
             {:qty 1 :card "Rebirth"}
             {:qty 1 :card "Spooned"}
             {:qty 3 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Patchwork"}
             {:qty 3 :card "Daily Casts"}
             {:qty 1 :card "Ice Carver"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "Same Old Thing"}
             {:qty 3 :card "The Turning Wheel"}
             {:qty 3 :card "Aumakua"}
             {:qty 2 :card "Black Orchestra"}
             {:qty 1 :card "MKUltra"}
             {:qty 2 :card "Paperclip"}])))

(def worlds-2018-joe-runs
  (matchup
    [:preconstructed.worlds-2018-b "Worlds 2018: Chris Dyer (C) vs. Joe Schupp (R)"]
    [:preconstructed.worlds-2018-b-tag "Chris Dyer (C) vs. Joe Schupp (R)"]
    [:preconstructed.worlds-2018-info "403(!) players attended worlds in 2018. This is the final worlds championship to be run by FFG. The tournament was held in Minneapolis, MN, USA, and consisted of 9(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Reign and Reverie"]
    (precon "Chris Dyer - 2018: Trust the Process"
            {:title "NBN: Controlling the Message" :side "Corp" :code 11017}
            [{:qty 1 :card "15 Minutes"}
             {:qty 3 :card "AR-Enhanced Security"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Project Beale"}
             {:qty 1 :card "Quantum Predictive Model"}
             {:qty 1 :card "Amani Senai"}
             {:qty 3 :card "Commercial Bankers Group"}
             {:qty 2 :card "Daily Business Show"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Team Sponsorship"}
             {:qty 1 :card "Closed Accounts"}
             {:qty 1 :card "Exchange of Information"}
             {:qty 3 :card "Hard-Hitting News"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Preemptive Action"}
             {:qty 1 :card "Psychographics"}
             {:qty 1 :card "SEA Source"}
             {:qty 2 :card "Calibration Testing"}
             {:qty 3 :card "Mumbad Virtual Tour"}
             {:qty 2 :card "IP Block"}
             {:qty 3 :card "Resistor"}
             {:qty 3 :card "Enigma"}
             {:qty 1 :card "Tollbooth"}
             {:qty 1 :card "Turnpike"}])
    (precon "Joe Schupp - 2018: Dan D'Argenio KoS Val"
            {:title "Valencia Estevez: The Angel of Cayambe" :side "Runner" :code 7030}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Employee Strike"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 2 :card "Indexing"}
             {:qty 3 :card "Inject"}
             {:qty 3 :card "Mining Accident"}
             {:qty 1 :card "Rebirth"}
             {:qty 3 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Turntable"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Earthrise Hotel"}
             {:qty 1 :card "Ice Carver"}
             {:qty 3 :card "Liberated Account"}
             {:qty 1 :card "No One Home"}
             {:qty 2 :card "The Turning Wheel"}
             {:qty 2 :card "Aumakua"}
             {:qty 3 :card "Black Orchestra"}
             {:qty 2 :card "MKUltra"}
             {:qty 3 :card "Paperclip"}
             {:qty 1 :card "D4v1d"}])))

;; Utility

(defn matchup-by-key
  [key]
  (condp = key
    ;; not indexed
    :beginner system-gateway-beginner
    :intermediate system-gateway-intermediate
    ;; indexed
    :worlds-2012-a worlds-2012-ben-corps
    :worlds-2012-b worlds-2012-ben-runs
    :worlds-2013-a worlds-2013-jens-corps
    :worlds-2013-b worlds-2013-jens-runs
    :worlds-2014-a worlds-2014-dan-d-corps
    :worlds-2014-b worlds-2014-dan-d-runs
    :worlds-2015-a worlds-2015-dan-d-corps
    :worlds-2015-b worlds-2015-dan-d-runs
    :worlds-2016-a worlds-2016-chris-dyer-corps
    :worlds-2016-b worlds-2016-chris-dyer-runs
    :worlds-2017-a worlds-2017-wilfy-corps
    :worlds-2017-b worlds-2017-wilfy-runs
    :worlds-2018-a worlds-2018-joe-corps
    :worlds-2018-b worlds-2018-joe-runs))

(def all-matchups
  "A set of all preconstructed matchups (by key).
  The frontend uses this to build a matchup table for play"
  #{:worlds-2012-a :worlds-2012-b
    :worlds-2013-a :worlds-2013-b
    :worlds-2014-a :worlds-2014-b
    :worlds-2015-a :worlds-2015-b
    :worlds-2016-a :worlds-2016-b
    :worlds-2017-a :worlds-2017-b
    :worlds-2018-a :worlds-2018-b})
