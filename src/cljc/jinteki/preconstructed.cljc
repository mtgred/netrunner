(ns jinteki.preconstructed)

(defn precon
  ([name id deck] (precon name id deck nil))
  ([name id deck decklist]
   {:identity id
    :name name
    :format "Preconstructed"
    :cards deck
    :decklist decklist}))

(defn matchup
  [tr-inner tr-tag tr-desc tr-underline corp runner]
  {:tr-inner tr-inner
   :tr-tag tr-tag
   :tr-desc tr-desc
   :tr-underline tr-underline
   :corp corp
   :runner runner})

;; Concept: Worlds matchups/decks

;; System Gateway preconstructed decks
(def gateway-beginner-corp
  {:format "system-gateway"
   :identity {:title "The Syndicate: Profit over Principle" :side "Corp" :code "30077"}
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
   :identity {:title "The Syndicate: Profit over Principle" :side "Corp" :code "30077"}
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
   :identity {:title "The Catalyst: Convention Breaker" :side "Runner" :code "30076"}
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
   :identity {:title "The Catalyst: Convention Breaker" :side "Runner" :code "30076"}
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
    [:lobby.gateway-format.beginner-ul "System Gateway - Beginner Teaching Decks"]
    gateway-beginner-corp
    gateway-beginner-runner))

(def system-gateway-intermediate
  (matchup
    [:lobby.gateway-format.intermediate "Intermediate"]
    [:lobby.gateway-format.intermediate "Intermediate"]
    [:lobby.gateway-format.intermediate-info
     "This lobby is using the System Gateway intermediate decks for the Corporation and Runner. These decks have slightly more range than the beginner decks. Games are played to 7 agenda points."]
    [:lobby.gateway-format.intermediate-ul "System Gateway - Intermediate Teaching Decks"]
    gateway-intermediate-corp
    gateway-intermediate-runner))

;; Worlds 2012: Ben Marsh vs. Jeremy Z
(def worlds-2012-ben-corps
  (matchup
    [:preconstructed.worlds-2012-a "Worlds 2012: Ben Marsh (C) vs. Jeremy Zwirn (R)"]
    [:preconstructed.worlds-2012-a-tag "Ben Marsh (C) vs. Jeremy Zwirn (R)"]
    [:preconstructed.worlds-2012-info "Worlds 2012 was played with (up to 3 copies of) the Core Set as the only legal product. Jeremy Zwirn (Building a Better World, Gabriel Santiago) took first place against Ben Marsh (Engineering the Future, Gabriel Santiago) in the first ever Netrunner World Championship."]
    [:preconstructed.worlds-2012-a-ul "Worlds 2012: Weyland vs. Criminal"]
    (precon "Ben Marsh - 2012: ETF"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code "01054"}
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
    (precon "Jeremy Z - 2012: Criminal"
            {:title "Gabriel Santiago: Consummate Professional" :side "Runner" :code "01017"}
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
    [:preconstructed.worlds-2012-info "Worlds 2012 was played with (up to 3 copies of) the Core Set as the only legal product. Jeremy Zwirn (Building a Better World, Gabriel Santiago) took first place against Ben Marsh (Engineering the Future, Gabriel Santiago) in the first ever Netrunner World Championship."]
    [:preconstructed.worlds-2012-b-ul "Worlds 2012: Haas-Bioroid vs. Criminal"]
    (precon "Jeremy Z - 2012: Weyland"
            {:title "Weyland Consortium: Building a Better World" :side "Corp" :code "01093"}
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
    (precon "Ben Marsh - 2012: Gabe"
            {:title "Gabriel Santiago: Consummate Professional" :side "Runner" :code "01017"}
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
             {:qty 1 :card "Crash Space"}
             {:qty 3 :card "Armitage Codebusting"}
             {:qty 2 :card "Bank Job"}])))

;; worlds 2013: Jens Erickson (1) vs. Andrew Veen (2)
(def worlds-2013-jens-corps
  (matchup
    [:preconstructed.worlds-2013-a "Worlds 2013: Jens Erickson (C) vs. Andrew Veen (R)"]
    [:preconstructed.worlds-2013-a-tag "Jens Erickson (C) vs. Andrew Veen (R)"]
    [:preconstructed.worlds-2013-info "166 players attended worlds in 2013. The tournament was held in Minneapolis, MN, USA, and consisted of 6 swiss rounds into a top 32 cut. The legal cardpool consisted of cards up to Opening Moves. Jens Erickson (Andromeda, ETR) took first place against Andrew Veen (Kate, NBN: Making News)"]
    [:preconstructed.worlds-2013-a-ul "Worlds 2013: HB FastAdv vs. Shaper Katman"]
    (precon "Jens Erickson - 2013: Engineering the Future"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code "01054"}
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
            {:title "Kate \"Mac\" McCaffrey: Digital Tinker" :side "Runner" :code "01033"}
            [{:qty 3 :card "Modded"}
             {:qty 3 :card "Test Run"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Clone Chip"}
             {:qty 3 :card "R&D Interface"}
             {:qty 3 :card "Plascrete Carapace"}
             {:qty 3 :card "Desperado"}
             {:qty 3 :card "Atman"}
             {:qty 1 :card "Deus X"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 3 :card "Self-modifying Code"}
             {:qty 3 :card "Datasucker"}
             {:qty 1 :card "Parasite"}
             {:qty 3 :card "Kati Jones"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Professional Contacts"}])))

(def worlds-2013-jens-runs
  (matchup
    [:preconstructed.worlds-2013-b "Worlds 2013: Andrew Veen (C) vs. Jens Erickson (R)"]
    [:preconstructed.worlds-2013-b-tag "Andrew Veen (C) vs. Jens Erickson (R)"]
    [:preconstructed.worlds-2013-info "166 players attended worlds in 2013. The tournament was held in Minneapolis, MN, USA, and consisted of 6 swiss rounds into a top 32 cut. The legal cardpool consisted of cards up to Opening Moves. Jens Erickson (Andromeda, ETR) took first place against Andrew Veen (Kate, NBN: Making News)"]
    [:preconstructed.worlds-2013-b-ul "Worlds 2013: NBN Fast Adv vs. Andy Sucker"]
    (precon "Andrew Veen - 2013: Making News"
            {:title "NBN: Making News" :side "Corp" :code "25104"}
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
            {:title "Andromeda: Dispossessed Ristie" :side "Runner" :code "02083"}
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
    [:preconstructed.worlds-2014-a-ul "Worlds 2014: Honor and Perfection vs. Andromedium"]
    (precon "Dan D'Argenio - 2014: Honor and Perfection"
            {:title "Jinteki: Replicating Perfection" :side "Corp" :code "02031"}
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
            {:title "Andromeda: Dispossessed Ristie" :side "Runner" :code "02083"}
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
    [:preconstructed.worlds-2014-b-ul "Worlds 2014: Personal Evolution vs. Daily QT Andy"]
    (precon "Minh Tran - 2014: Personal Evolution"
            {:title "Jinteki: Personal Evolution" :side "Corp" :code "01067"}
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
            {:title "Andromeda: Dispossessed Ristie" :side "Runner" :code "02083"}
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
    [:preconstructed.worlds-2015-a-ul "Worlds 2015: Foodcoatshop vs. The Morning After"]
    (precon "Dan D'Argenio - 2015: Foodcoatshop"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code "01054"}
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
            {:title "Whizzard: Master Gamer" :side "Runner" :code "02001"}
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
    [:preconstructed.worlds-2015-b-ul "Worlds 2015: Yellow Shell vs. Radisson Cheese Plate"]
    (precon "Timmy Wong - 2015: Yellow Shell"
            {:title "Near-Earth Hub: Broadcast Center" :side "Corp" :code "06005"}
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
            {:title "Valencia Estevez: The Angel of Cayambe" :side "Runner" :code "07030"}
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
    [:preconstructed.worlds-2016-a-ul "Worlds 2016: Snekbite vs. Minh MaxX++"]
    (precon "Chris Dyer - 2016: Snekbite"
            {:title "NBN: Controlling the Message" :side "Corp" :code "11017"}
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
            {:title "MaxX: Maximum Punk Rock" :side "Runner" :code "07029"}
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
    [:preconstructed.worlds-2016-b-ul "Worlds 2016: Fiery Info vs. Papa Smurf"]
    (precon "Benjamin Ni - 2016: Fiery Info"
            {:title "SYNC: Everything, Everywhere" :side "Corp" :code "09001"}
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
            {:title "Whizzard: Master Gamer" :side "Runner" :code "02001"}
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

;; Worlds 2017: ChaosJuggler vs. Grey Tongue
(def worlds-2017-jess-corps
  (matchup
    [:preconstructed.worlds-2017-a "Worlds 2017: Jess Horig (C) vs. Grey Tongue (R)"]
    [:preconstructed.worlds-2017-a-tag "Jess Horig (C) vs. Grey Tongue (R)"]
    [:preconstructed.worlds-2017-info "233 players attended worlds in 2017. The tournament was held in Minneapolis, MN, USA, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Revised Core set."]
    [:preconstructed.worlds-2017-a-ul "Worlds 2017: Stinson Reversed CI vs. Aesops Hayley"]
    (precon "ChaosJuggler - 2017: Sinson Reversed CI"
            {:title "Cerebral Imaging: Infinite Frontiers" :side "Corp" :code "03001"}
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
    (precon "Grey Tongue - 2017: Aesops Hayley"
            {:title "Hayley Kaplan: Universal Scholar" :side "Runner" :code "08025"}
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

(def worlds-2017-jess-runs
  (matchup
    [:preconstructed.worlds-2017-b "Worlds 2017: Grey Tongue (C) vs. Jess Horig (R)"]
    [:preconstructed.worlds-2017-b-tag "Grey Tongue (C) vs. Jess Horig (R)"]
    [:preconstructed.worlds-2017-info "233 players attended worlds in 2017. The tournament was held in Minneapolis, MN, USA, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Revised Core set."]
    [:preconstructed.worlds-2017-b-ul "Worlds 2017: No-Show Rewiring CI vs. Laguna Lock Hayley"]
    (precon "Grey Tongue - 2017: No-Show Rewiring CI"
            {:title "Cerebral Imaging: Infinite Frontiers" :side "Corp" :code "03001"}
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
            {:title "Hayley Kaplan: Universal Scholar" :side "Runner" :code "08025"}
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
    [:preconstructed.worlds-2018-a-ul "Worlds 2018: AMERICA CtM vs. Gooseberry MaxX"]
    (precon "Joe Schupp - 2018: AMERICA CtM"
            {:title "NBN: Controlling the Message" :side "Corp" :code "11017"}
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
            {:title "MaxX: Maximum Punk Rock" :side "Runner" :code "07029"}
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
    [:preconstructed.worlds-2018-b-ul "Worlds 2018: Trust the Process vs. Dan D'Argenio KoS Val"]
    (precon "Chris Dyer - 2018: Trust the Process"
            {:title "NBN: Controlling the Message" :side "Corp" :code "11017"}
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
            {:title "Valencia Estevez: The Angel of Cayambe" :side "Runner" :code "07030"}
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

;; Worlds 2019: Pinsel vs. Testrunning
(def worlds-2019-pinsel-corps
  (matchup
    [:preconstructed.worlds-2019-a "Worlds 2019: Pinsel (C) vs. Testrunning (R)"]
    [:preconstructed.worlds-2019-a-tag "Pinsel (C) vs. Testrunning (R)"]
    [:preconstructed.worlds-2019-info "256 players played in the first even Project NISEI Netrunner World Championship in 2019. This tournament was held in Rotterdam, NL, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Uprising Booster Pack"]
    [:preconstructed.worlds-2019-a-ul "Worlds 2019: Fully dedicated to efficiency vs. Trash Panda"]
    (precon "Pinsel - 2019: Fully dedicated to efficiency"
            {:title "Asa Group: Security Through Vigilance" :side "Corp" :code "21009"}
            [{:qty 3 :card "Efficiency Committee"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 3 :card "Cybernetics Court"}
             {:qty 2 :card "Executive Boot Camp"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 1 :card "Lakshmi Smartfabrics"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 1 :card "Reconstruction Contract"}
             {:qty 2 :card "Archived Memories"}
             {:qty 2 :card "Biotic Labor"}
             {:qty 2 :card "Dedication Ceremony"}
             {:qty 3 :card "Fully Operational"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Violet Level Clearance"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 2 :card "Hagen"}
             {:qty 3 :card "Vanilla"}
             {:qty 3 :card "Gatekeeper"}
             {:qty 3 :card "Architect"}])
    (precon "Testrunning - 2019: Trash Panda"
            {:title "Freedom Khumalo: Crypto-Anarchist" :side "Runner" :code "21081"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 3 :card "Inject"}
             {:qty 3 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Clone Chip"}
             {:qty 3 :card "Hippo"}
             {:qty 2 :card "Knobkierie"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "Street Peddler"}
             {:qty 2 :card "Black Orchestra"}
             {:qty 1 :card "MKUltra"}
             {:qty 2 :card "Paperclip"}
             {:qty 2 :card "Yusuf"}
             {:qty 1 :card "Consume"}
             {:qty 2 :card "D4v1d"}
             {:qty 1 :card "Imp"}
             {:qty 1 :card "Pelangi"}
             {:qty 1 :card "Self-modifying Code"}
             {:qty 1 :card "Stargate"}])))

(def worlds-2019-pinsel-runs
  (matchup
    [:preconstructed.worlds-2019-b "Worlds 2019: Testrunning (C) vs. Pinsel (R)"]
    [:preconstructed.worlds-2019-b-tag "Testrunning (C) vs. Pinsel (R)"]
    [:preconstructed.worlds-2019-info "256 players played in the first even Project NISEI Netrunner World Championship in 2019. This tournament was held in Rotterdam, NL, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Uprising Booster Pack"]
    [:preconstructed.worlds-2019-b-ul "Worlds 2019: 2 Grid for 2 Place vs. Trash Panda"]
    (precon "Testrunning - 2019: 2 Grid for 2 Place"
            {:title "Pālanā Foods: Sustainable Growth" :side "Corp" :code "10030"}
            [{:qty 3 :card "Nisei MK II"}
             {:qty 3 :card "Obokata Protocol"}
             {:qty 1 :card "Philotic Entanglement"}
             {:qty 1 :card "SSL Endorsement"}
             {:qty 3 :card "NGO Front"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 2 :card "Celebrity Gift"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "IPO"}
             {:qty 3 :card "Scarcity of Resources"}
             {:qty 3 :card "Bio Vault"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "La Costa Grid"}
             {:qty 3 :card "Border Control"}
             {:qty 3 :card "IP Block"}
             {:qty 1 :card "DNA Tracker"}
             {:qty 3 :card "Slot Machine"}
             {:qty 2 :card "Thimblerig"}
             {:qty 2 :card "Excalibur"}
             {:qty 3 :card "Anansi"}])
    (precon "Pinsel - 2019: Trash Panda"
            {:title "Freedom Khumalo: Crypto-Anarchist" :side "Runner" :code "21081"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 3 :card "Inject"}
             {:qty 3 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Clone Chip"}
             {:qty 3 :card "Hippo"}
             {:qty 2 :card "Knobkierie"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "Street Peddler"}
             {:qty 2 :card "Black Orchestra"}
             {:qty 1 :card "MKUltra"}
             {:qty 2 :card "Paperclip"}
             {:qty 2 :card "Yusuf"}
             {:qty 1 :card "Consume"}
             {:qty 2 :card "D4v1d"}
             {:qty 1 :card "Imp"}
             {:qty 1 :card "Pelangi"}
             {:qty 1 :card "Self-modifying Code"}
             {:qty 1 :card "Stargate"}])))

;; Worlds 2020: Limes vs. tf34
(def worlds-2020-limes-corps
  (matchup
    [:preconstructed.worlds-2020-a "Worlds 2020: Limes (C) vs. tf34 (R)"]
    [:preconstructed.worlds-2020-a-tag "Limes (C) vs. tf34 (R)"]
    [:preconstructed.worlds-2020-info "294 players played in the first ever online world championship for Netrunner, run by Project NISEI 2020. Due to travel restrictions at the start of the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to Uprising."]
    [:preconstructed.worlds-2020-a-ul "Worlds 2020: I don't like this deck vs. Engolo Freedom"]
    (precon "Limes - 2020: I don't like this deck"
            {:title "Sportsmetal: Go Big or Go Home" :side "Corp" :code "22026"}
            [{:qty 2 :card "False Lead"}
             {:qty 3 :card "Hyperloop Extension"}
             {:qty 3 :card "Megaprix Qualifier"}
             {:qty 3 :card "Project Vacheron"}
             {:qty 3 :card "The Future is Now"}
             {:qty 1 :card "CSR Campaign"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 3 :card "Marilyn Campaign"}
             {:qty 3 :card "News Team"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Team Sponsorship"}
             {:qty 3 :card "Archived Memories"}
             {:qty 1 :card "BOOM!"}
             {:qty 2 :card "Fast Break"}
             {:qty 3 :card "Game Changer"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Preemptive Action"}
             {:qty 3 :card "Stock Buy-Back"}
             {:qty 3 :card "Meridian"}])
    (precon "tf34 - 2020: Engolo Freedom"
            {:title "Freedom Khumalo: Crypto-Anarchist" :side "Runner" :code "21081"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 1 :card "Rebirth"}
             {:qty 1 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Boomerang"}
             {:qty 3 :card "Hippo"}
             {:qty 2 :card "Keiko"}
             {:qty 3 :card "Zer0"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "Paladin Poemu"}
             {:qty 3 :card "Street Peddler"}
             {:qty 2 :card "The Turning Wheel"}
             {:qty 1 :card "Trickster Taka"}
             {:qty 1 :card "Virus Breeding Ground"}
             {:qty 2 :card "Aumakua"}
             {:qty 2 :card "Engolo"}
             {:qty 3 :card "Yusuf"}
             {:qty 2 :card "Datasucker"}])))

(def worlds-2020-limes-runs
  (matchup
    [:preconstructed.worlds-2020-b "Worlds 2020: tf34 (R) vs. Limes (C)"]
    [:preconstructed.worlds-2020-b-tag "tf34 (R) vs. Limes (C)"]
    [:preconstructed.worlds-2020-info "294 players played in the first ever online world championship for Netrunner, run by Project NISEI 2020. Due to travel restrictions at the start of the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to Uprising."]
    [:preconstructed.worlds-2020-b-ul "Worlds 2020: Malia CTM vs. Imp-pressive Hoshiko"]
    (precon "tf34 - 2020: Malia CTM"
            {:title "NBN: Controlling the Message" :side "Corp" :code "11017"}
            [{:qty 3 :card "Bellona"}
             {:qty 1 :card "Degree Mill"}
             {:qty 3 :card "Project Beale"}
             {:qty 1 :card "Remastered Edition"}
             {:qty 3 :card "Commercial Bankers Group"}
             {:qty 3 :card "Daily Business Show"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 3 :card "Malia Z0L0K4"}
             {:qty 3 :card "Marilyn Campaign"}
             {:qty 3 :card "Mumba Temple"}
             {:qty 1 :card "NASX"}
             {:qty 1 :card "PAD Campaign"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Hard-Hitting News"}
             {:qty 2 :card "Market Forces"}
             {:qty 2 :card "Psychographics"}
             {:qty 2 :card "Mumbad Virtual Tour"}
             {:qty 3 :card "IP Block"}
             {:qty 2 :card "Wraparound"}
             {:qty 2 :card "Enigma"}
             {:qty 1 :card "Tollbooth"}
             {:qty 1 :card "F2P"}])
    (precon "Limes - 2020: Imp-pressive Hoshiko"
            {:title "Hoshiko Shiro: Untold Protagonist" :side "Runner" :code "26066"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 2 :card "I've Had Worse"}
             {:qty 2 :card "Labor Rights"}
             {:qty 1 :card "Rebirth"}
             {:qty 2 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Friday Chip"}
             {:qty 3 :card "Hippo"}
             {:qty 2 :card "HQ Interface"}
             {:qty 3 :card "Patchwork"}
             {:qty 2 :card "DreamNet"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "Paladin Poemu"}
             {:qty 3 :card "Black Orchestra"}
             {:qty 2 :card "MKUltra"}
             {:qty 3 :card "Paperclip"}
             {:qty 2 :card "Consume"}
             {:qty 3 :card "Imp"}
             {:qty 1 :card "Stargate"}])))

;; Worlds 2021: Patrick Gower vs. Jonas
(def worlds-2021-patrick-corps
  (matchup
    [:preconstructed.worlds-2021-a "Worlds 2021: Patrick Gower (C) vs. Jonas (R)"]
    [:preconstructed.worlds-2021-a-tag "Patrick Gower (C) vs. Jonas (R)"]
    [:preconstructed.worlds-2021-info "201 players played in the second online world championship for Netrunner, run by Project NISEI in 2021. Due to the ongoing disruption caused by the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to System Gateway."]
    [:preconstructed.worlds-2021-a-ul "Worlds 2021: 44 Card PD vs. Watch Me Drip, Watch Me Maemi"]
    (precon "Patrick Gower - 2021: 44 card PD"
            {:title "Haas-Bioroid: Precision Design" :side "Corp" :code "30035"}
            [{:qty 3 :card "Cyberdex Sandbox"}
             {:qty 2 :card "Global Food Initiative"}
             {:qty 1 :card "Luminal Transubstantiation"}
             {:qty 2 :card "Offworld Office"}
             {:qty 3 :card "Advanced Assembly Lines"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 2 :card "Spin Doctor"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Seamless Launch"}
             {:qty 1 :card "Anoetic Void"}
             {:qty 1 :card "Crisium Grid"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 2 :card "Manegarm Skunkworks"}
             {:qty 3 :card "Tranquility Home Grid"}
             {:qty 2 :card "Border Control"}
             {:qty 1 :card "Hagen"}
             {:qty 3 :card "Gatekeeper"}
             {:qty 1 :card "Macrophage"}
             {:qty 2 :card "Magnet"}
             {:qty 2 :card "Ansel 1.0"}
             {:qty 3 :card "Drafter"}])
    (precon "Jonas - 2021: Watch Me Drip, Watch Me Maemi"
            {:title "MaxX: Maximum Punk Rock" :side "Runner" :code "07029"}
            [{:qty 3 :card "Deuces Wild"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 1 :card "Falsified Credentials"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 3 :card "Labor Rights"}
             {:qty 3 :card "Mining Accident"}
             {:qty 3 :card "Moshing"}
             {:qty 2 :card "Overclock"}
             {:qty 1 :card "Rebirth"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Gachapon"}
             {:qty 3 :card "Hippo"}
             {:qty 2 :card "Maw"}
             {:qty 2 :card "Zer0"}
             {:qty 1 :card "Citadel Sanctuary"}
             {:qty 3 :card "Daily Casts"}
             {:qty 1 :card "DJ Fenris"}
             {:qty 3 :card "Liberated Account"}
             {:qty 2 :card "Mystic Maemi"}
             {:qty 3 :card "PAD Tap"}
             {:qty 3 :card "Paladin Poemu"}
             {:qty 2 :card "Political Operative"}
             {:qty 1 :card "Trickster Taka"}
             {:qty 3 :card "Black Orchestra"}
             {:qty 3 :card "MKUltra"}
             {:qty 3 :card "Paperclip"}
             {:qty 2 :card "Botulus"}
             {:qty 3 :card "Rezeki"}
             {:qty 2 :card "Stargate"}])))

(def worlds-2021-patrick-runs
  (matchup
    [:preconstructed.worlds-2021-b "Worlds 2021: Jonas (C) vs. Patrick Gower (R)"]
    [:preconstructed.worlds-2021-b-tag "Jonas (C) vs. Patrick Gower (R)"]
    [:preconstructed.worlds-2021-info "201 players played in the second online world championship for Netrunner, run by Project NISEI in 2021. Due to the ongoing disruption caused by the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to System Gateway."]
    [:preconstructed.worlds-2021-b-ul "Worlds 2021: Is Gagarin Good? vs. Medium to Large Maxx"]
    (precon "Jonas - 2021: Is Gagarin Good?"
            {:title "Gagarin Deep Space: Expanding the Horizon" :side "Corp" :code "07002"}
            [{:qty 1 :card "Above the Law"}
             {:qty 2 :card "Global Food Initiative"}
             {:qty 2 :card "Hostile Takeover"}
             {:qty 2 :card "Offworld Office"}
             {:qty 3 :card "Project Atlas"}
             {:qty 3 :card "Commercial Bankers Group"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 3 :card "Marilyn Campaign"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Reconstruction Contract"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 3 :card "Wall to Wall"}
             {:qty 1 :card "Audacity"}
             {:qty 1 :card "Consulting Visit"}
             {:qty 3 :card "Dedication Ceremony"}
             {:qty 3 :card "Economic Warfare"}
             {:qty 2 :card "Hard-Hitting News"}
             {:qty 1 :card "High-Profile Target"}
             {:qty 3 :card "Border Control"}
             {:qty 2 :card "Magnet"}
             {:qty 1 :card "Mausolus"}
             {:qty 1 :card "Rototurret"}])
    (precon "Patrick Gower - 2021: Medium to Large MaxX"
            {:title "MaxX: Maximum Punk Rock" :side "Runner" :code "07029"}
            [{:qty 3 :card "Deuces Wild"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 3 :card "Labor Rights"}
             {:qty 1 :card "Mad Dash"}
             {:qty 3 :card "Moshing"}
             {:qty 3 :card "Overclock"}
             {:qty 1 :card "Rebirth"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Hippo"}
             {:qty 3 :card "Maw"}
             {:qty 3 :card "Aeneas Informant"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "PAD Tap"}
             {:qty 2 :card "Paladin Poemu"}
             {:qty 1 :card "Political Operative"}
             {:qty 1 :card "Salsette Slums"}
             {:qty 1 :card "The Turning Wheel"}
             {:qty 3 :card "Black Orchestra"}
             {:qty 3 :card "MKUltra"}
             {:qty 3 :card "Paperclip"}
             {:qty 1 :card "Botulus"}
             {:qty 3 :card "Rezeki"}
             {:qty 1 :card "Stargate"}])))

;; Worlds 2022: William Huang vs. skry
(def worlds-2022-sokka-corps
  (matchup
    [:preconstructed.worlds-2022-a "Worlds 2022: William Huang (C) vs. skry (R)"]
    [:preconstructed.worlds-2022-a-tag "William Huang (C) vs. skry (R)"]
    [:preconstructed.worlds-2022-info "158 players played in the first world championship run by Null Signal Games (formerly Project NISEI), which was the first Netrunner world championship to be run in-person since the start of the COVID-19 pandemic. The tournament was held in Toronto, Canada, and consisted of 8(?) rounds into a top 16 cut. The legal cardpool consisted of cards up to Midnight Sun."]
    [:preconstructed.worlds-2022-a-ul "Worlds 2022: SNACS vs. Liberté, Égalité, Humidité"]
    (precon "William Huang - 2022: SNACS"
            {:title "Sportsmetal: Go Big or Go Home" :side "Corp" :code "22026"}
            [{:qty 1 :card "Élivágar Bifurcation"}
             {:qty 2 :card "Global Food Initiative"}
             {:qty 1 :card "Luminal Transubstantiation"}
             {:qty 3 :card "Megaprix Qualifier"}
             {:qty 1 :card "Offworld Office"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 2 :card "Audacity"}
             {:qty 2 :card "Biotic Labor"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "NEXT Activation Command"}
             {:qty 3 :card "Red Level Clearance"}
             {:qty 3 :card "Seamless Launch"}
             {:qty 2 :card "Mavirus"}
             {:qty 3 :card "Tranquility Home Grid"}
             {:qty 1 :card "Hagen"}
             {:qty 3 :card "Fairchild 3.0"}
             {:qty 3 :card "Magnet"}
             {:qty 1 :card "Loki"}
             {:qty 2 :card "Ansel 1.0"}
             {:qty 1 :card "Drafter"}])
    (precon "skry - 2022: Liberté, Égalité, Humidité"
            {:title "Freedom Khumalo: Crypto-Anarchist" :side "Runner" :code "21081"}
            [{:qty 3 :card "Deuces Wild"}
             {:qty 2 :card "Dirty Laundry"}
             {:qty 1 :card "Mad Dash"}
             {:qty 3 :card "Moshing"}
             {:qty 2 :card "Pinhole Threading"}
             {:qty 3 :card "Steelskin Scarring"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Endurance"}
             {:qty 3 :card "Gachapon"}
             {:qty 3 :card "Hippo"}
             {:qty 2 :card "Zer0"}
             {:qty 1 :card "Citadel Sanctuary"}
             {:qty 1 :card "DreamNet"}
             {:qty 1 :card "Liberated Account"}
             {:qty 1 :card "Mystic Maemi"}
             {:qty 2 :card "Paladin Poemu"}
             {:qty 2 :card "Black Orchestra"}
             {:qty 1 :card "MKUltra"}
             {:qty 1 :card "Paperclip"}
             {:qty 2 :card "Yusuf"}
             {:qty 1 :card "Botulus"}
             {:qty 3 :card "Fermenter"}
             {:qty 2 :card "Stargate"}])))

(def worlds-2022-sokka-runs
  (matchup
    [:preconstructed.worlds-2022-b "Worlds 2022: skry (C) vs. William Huang (R)"]
    [:preconstructed.worlds-2022-b-tag "skry (C) vs. William Huang (R)"]
    [:preconstructed.worlds-2022-info "158 players played in the first world championship run by Null Signal Games (formerly Project NISEI), which was the first Netrunner world championship to be run in-person since the start of the COVID-19 pandemic. The tournament was held in Toronto, Canada, and consisted of 8(?) rounds into a top 16 cut. The legal cardpool consisted of cards up to Midnight Sun."]
    [:preconstructed.worlds-2022-b-ul "Worlds 2022: Dies to Doom Blade vs. ApocoLat"]
    (precon "skry - 2022: Dies to Doomblade"
            {:title "AgInfusion: New Miracles for a New World" :side "Corp" :code "12052"}
            [{:qty 1 :card "Longevity Serum"}
             {:qty 3 :card "Obokata Protocol"}
             {:qty 3 :card "Send a Message"}
             {:qty 2 :card "NGO Front"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Regolith Mining License"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 2 :card "Trieste Model Bioroids"}
             {:qty 2 :card "Divert Power"}
             {:qty 3 :card "Hansei Review"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Subliminal Messaging"}
             {:qty 2 :card "Bio Vault"}
             {:qty 2 :card "Chiyashi"}
             {:qty 2 :card "DNA Tracker"}
             {:qty 3 :card "Wave"}
             {:qty 1 :card "Konjin"}
             {:qty 1 :card "Loki"}
             {:qty 3 :card "Anansi"}
             {:qty 3 :card "Anemone"}
             {:qty 3 :card "Mlinzi"}])
    (precon "William Huang - 2022: ApocoLat"
            {:title "Lat: Ethical Freelancer" :side "Runner" :code "26019"}
            [{:qty 3 :card "Apocalypse"}
             {:qty 3 :card "Creative Commission"}
             {:qty 1 :card "Deuces Wild"}
             {:qty 3 :card "Diesel"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 2 :card "Into the Depths"}
             {:qty 2 :card "Mad Dash"}
             {:qty 2 :card "Pinhole Threading"}
             {:qty 3 :card "Rigging Up"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Endurance"}
             {:qty 1 :card "Aesop's Pawnshop"}
             {:qty 1 :card "Beth Kilrain-Chang"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Stoneship Chart Room"}
             {:qty 2 :card "Telework Contract"}
             {:qty 2 :card "Engolo"}
             {:qty 1 :card "Ika"}
             {:qty 1 :card "Propeller"}
             {:qty 1 :card "Misdirection"}
             {:qty 1 :card "Self-modifying Code"}
             {:qty 1 :card "Stargate"}])))

;; Worlds 2022: William Huang vs. cableCarnage
(def worlds-2023-sokka-corps
  (matchup
    [:preconstructed.worlds-2023-a "Worlds 2023: William Huang (C) vs. cableCarnage (R)"]
    [:preconstructed.worlds-2023-a-tag "William Huang (C) vs. cableCarnage (R)"]
    [:preconstructed.worlds-2023-info "254 players played in the second Netrunner world championship run by Null Signal Games. The tournament was held in Barcelona, Spain, and consisted of 8 rounds into a top 16 cut. The legal cardpool consisted of cards up to The Automata Initiative."]
    [:preconstructed.worlds-2023-a-ul "Worlds 2023: The Worlds Grid vs. sableCarnage"]
    (precon "William Huang - 2023: The Worlds Grind"
            {:title "Weyland Consortium: Built to Last" :side "Corp" :code "30059"}
            [{:qty 1 :card "Above the Law"}
             {:qty 3 :card "SDS Drone Deployment"}
             {:qty 3 :card "Send a Message"}
             {:qty 2 :card "Clearinghouse"}
             {:qty 3 :card "NGO Front"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 3 :card "Wall to Wall"}
             {:qty 3 :card "Government Subsidy"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Secure and Protect"}
             {:qty 2 :card "Subliminal Messaging"}
             {:qty 1 :card "Trojan Horse"}
             {:qty 3 :card "Bio Vault"}
             {:qty 3 :card "Akhet"}
             {:qty 1 :card "Border Control"}
             {:qty 2 :card "Pharos"}
             {:qty 3 :card "Tree Line"}
             {:qty 2 :card "Hortum"}
             {:qty 2 :card "Mestnichestvo"}
             {:qty 1 :card "Rime"}
             {:qty 1 :card "Sadaka"}
             {:qty 3 :card "Winchester"}])
    (precon "cableCarnage - 2023: sableCarnage"
            {:title "Nyusha \"Sable\" Sintashta: Symphonic Prodigy" :side "Runner" :code "33011"}
            [{:qty 1 :card "Bahia Bands"}
             {:qty 3 :card "Bravado"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Diversion of Funds"}
             {:qty 1 :card "Inside Job"}
             {:qty 1 :card "Mutual Favor"}
             {:qty 2 :card "Pinhole Threading"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Boomerang"}
             {:qty 1 :card "Flip Switch"}
             {:qty 2 :card "Hermes"}
             {:qty 1 :card "WAKE Implant v2A-JRJ"}
             {:qty 3 :card "Daily Casts"}
             {:qty 2 :card "Earthrise Hotel"}
             {:qty 2 :card "Info Bounty"}
             {:qty 1 :card "Miss Bones"}
             {:qty 1 :card "Mystic Maemi"}
             {:qty 2 :card "No Free Lunch"}
             {:qty 1 :card "Paladin Poemu"}
             {:qty 3 :card "The Class Act"}
             {:qty 2 :card "The Twinning"}
             {:qty 1 :card "Aumakua"}
             {:qty 1 :card "Curupira"}
             {:qty 1 :card "Echelon"}
             {:qty 1 :card "Unity"}
             {:qty 1 :card "Cezve"}])))

(def worlds-2023-sokka-runs
  (matchup
    [:preconstructed.worlds-2023-b "Worlds 2023: cableCarnage (C) vs. William Huang (R)"]
    [:preconstructed.worlds-2023-b-tag "cableCarnage (C) vs. William Huang (R)"]
    [:preconstructed.worlds-2023-info "254 players played in the second Netrunner world championship run by Null Signal Games. The tournament was held in Barcelona, Spain, and consisted of 8 rounds into a top 16 cut. The legal cardpool consisted of cards up to The Automata Initiative."]
    [:preconstructed.worlds-2023-b-ul "Worlds 2023: tableCarnage vs. You *do* always come back!"]
    (precon "cableCarnage - 2023: tableCarnage"
            {:title "Near-Earth Hub: Broadcast Center" :side "Corp" :code "06005"}
            [{:qty 3 :card "Bellona"}
             {:qty 2 :card "Degree Mill"}
             {:qty 3 :card "False Lead"}
             {:qty 1 :card "Tomorrow's Headline"}
             {:qty 3 :card "Behold!"}
             {:qty 2 :card "Chekist Scion"}
             {:qty 3 :card "Federal Fundraising"}
             {:qty 2 :card "Gaslight"}
             {:qty 1 :card "Malia Z0L0K4"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 2 :card "Reaper Function"}
             {:qty 3 :card "Regolith Mining License"}
             {:qty 1 :card "SIU"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 3 :card "Wage Workers"}
             {:qty 2 :card "Attitude Adjustment"}
             {:qty 1 :card "End of the Line"}
             {:qty 2 :card "Hedge Fund"}
             {:qty 1 :card "Mindscaping"}
             {:qty 3 :card "Oppo Research"}
             {:qty 2 :card "Your Digital Life"}
             {:qty 1 :card "Ping"}
             {:qty 1 :card "Virtual Service Agent"}
             {:qty 1 :card "Unsmiling Tsarevna"}])
    (precon "William Huang - 2023: You *do* always come back!"
            {:title "Hoshiko Shiro: Untold Protagonist" :side "Runner" :code "26066"}
            [{:qty 2 :card "Diesel"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Moshing"}
             {:qty 3 :card "Raindrops Cut Stone"}
             {:qty 3 :card "Steelskin Scarring"}
             {:qty 3 :card "Strike Fund"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Boomerang"}
             {:qty 2 :card "Maw"}
             {:qty 3 :card "Daily Casts"}
             {:qty 1 :card "DJ Fenris"}
             {:qty 1 :card "Miss Bones"}
             {:qty 1 :card "Mystic Maemi"}
             {:qty 1 :card "Paladin Poemu"}
             {:qty 2 :card "The Twinning"}
             {:qty 3 :card "Tsakhia \"Bankhar\" Gantulga"}
             {:qty 2 :card "Buzzsaw"}
             {:qty 2 :card "Carmen"}
             {:qty 2 :card "Cleaver"}
             {:qty 2 :card "Fermenter"}
             {:qty 1 :card "Leech"}])))

(def worlds-2024-deer-runs
    (matchup
      [:preconstructed.worlds-2024-a "Worlds 2024: Alex Boyd (C) vs. Dee Ruttenberg (R)"]
      [:preconstructed.worlds-2024-a-tag "Alex Boyd (C) vs. Dee Ruttenberg (R)"]
      [:preconstructed.worlds-2024-info "204 players played in the third Netrunner world championship run by Null Signal Games. In this tournament, Alex Boyd AKA Aruzan (Arissana, Reality Plus) won the title of Netrunner World Champion in a final game Against Dee Ruttenberg AKA DeeR (Lat, PE), with Aruzan going entirely undefeated in the top cut. The tournament was held at the San Francisco Embarcadero Waterfront Hotel on 19th and 20th of October, and consisted of 14 rounds of Single-Sided Swiss into a top 16 cut. The legal cardpool consisted of cards up to Rebellion Without Rehearsal."]
      [:preconstructed.worlds-2024-a-ul "Worlds 2024: Kill R+ vs. Good Stuff Lat"]
      (precon "Aruzan - 2024: Kill R+"
            {:title "NBN: Reality Plus" :side "Corp" :code "30051"}
            [{:qty 2 :card "Degree Mill"}
             {:qty 1 :card "Oracle Thinktank"}
             {:qty 3 :card "Project Beale"}
             {:qty 1 :card "Tomorrow's Headline"}
             {:qty 3 :card "False Lead"}
             {:qty 1 :card "Orbital Superiority"}
             {:qty 3 :card "Behold!"}
             {:qty 1 :card "Gaslight"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 1 :card "Lady Liberty"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Regolith Mining License"}
             {:qty 3 :card "Ping"}
             {:qty 3 :card "Unsmiling Tsarevna"}
             {:qty 2 :card "Virtual Service Agent"}
             {:qty 3 :card "Oppo Research"}
             {:qty 1 :card "Predictive Planogram"}
             {:qty 2 :card "Your Digital Life"}
             {:qty 1 :card "Sprint"}
             {:qty 2 :card "Mindscaping"}
             {:qty 2 :card "End of the Line"}
             {:qty 1 :card "Pivot"}
             {:qty 2 :card "AMAZE Amusements"}
             {:qty 2 :card "The Holo Man"}])
      (precon "DeeR - 2024: Deep Dive Lat"
              {:title "Lat: Ethical Freelancer" :side "Runner" :code "26019"}
              [{:qty 3 :card "Creative Commission"}
               {:qty 2 :card "Deep Dive"}
               {:qty 2 :card "Diesel"}
               {:qty 3 :card "Trick Shot"}
               {:qty 1 :card "Bahia Bands"}
               {:qty 3 :card "Dirty Laundry"}
               {:qty 3 :card "Overclock"}
               {:qty 2 :card "Sure Gamble"}
               {:qty 2 :card "Pinhole Threading"}
               {:qty 3 :card "Simulchip"}
               {:qty 2 :card "Swift"}
               {:qty 1 :card "Echelon"}
               {:qty 1 :card "Euler"}
               {:qty 1 :card "Gauss"}
               {:qty 1 :card "K2CP Turbine"}
               {:qty 1 :card "Paricia"}
               {:qty 1 :card "Propeller"}
               {:qty 3 :card "Self-modifying Code"}
               {:qty 1 :card "Fermenter"}
               {:qty 1 :card "Cupellation"}
               {:qty 1 :card "Revolver"}
               {:qty 1 :card "Dr. Nuka Vrolyck"}
               {:qty 3 :card "Stoneship Chart Room"}
               {:qty 2 :card "Telework Contract"}
               {:qty 1 :card "DJ Fenris"}])))

(def worlds-2024-deer-corps
  (matchup
    [:preconstructed.worlds-2024-b "Worlds 2024: Dee Ruttenberg (C) vs Alex Boyd (R)"]
    [:preconstructed.worlds-2024-b-tag "Dee Ruttenberg (C) vs. Alex Boyd (R)"]
    [:preconstructed.worlds-2024-info "204 players played in the third Netrunner world championship run by Null Signal Games. In this tournament, Alex Boyd AKA Aruzan (Arissana, Reality Plus) won the title of Netrunner World Champion in a final game Against Dee Ruttenberg AKA DeeR (Lat, PE), with Aruzan going entirely undefeated in the top cut. The tournament was held at the San Francisco Embarcadero Waterfront Hotel on 19th and 20th of October, and consisted of 14 rounds of Single-Sided Swiss into a top 16 cut. The legal cardpool consisted of cards up to Rebellion Without Rehearsal."]
    [:preconstructed.worlds-2024-b-ul "Worlds 2024: Loud PE vs. Deep Dive Arissana"]
    (precon "DeeR - 2024: Loud PE"
            {:title "Jinteki: Personal Evolution" :side "Corp" :code "01067"}
            [{:qty 1 :card "Blood in the Water"}
             {:qty 3 :card "Fujii Asset Retrieval"}
             {:qty 2 :card "House of Knives"}
             {:qty 2 :card "Hybrid Release"}
             {:qty 2 :card "Regenesis"}
             {:qty 3 :card "Sting!"}
             {:qty 3 :card "Cohort Guidance Program"}
             {:qty 2 :card "Moon Pool"}
             {:qty 3 :card "Prāna Condenser"}
             {:qty 1 :card "Snare!"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 1 :card "Wage Workers"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 2 :card "Anansi"}
             {:qty 2 :card "Anemone"}
             {:qty 3 :card "Tatu-Bola"}
             {:qty 3 :card "Vampyronassa"}
             {:qty 2 :card "Data Loop"}
             {:qty 3 :card "Mindscaping"}
             {:qty 1 :card "Hedge Fund"}
             {:qty 1 :card "Mavirus"}
             {:qty 1 :card "Tranquility Home Grid"}
             {:qty 1 :card "The Holo Man"}
             {:qty 1 :card "Crisium Grid"}])
    (precon "Aruzan - 2024: Spree Arissana"
            {:title "Arissana Rocha Nahu: Street Artist" :side "Runner" :code "34020"}
            [{:qty 2 :card "Burner"}
             {:qty 3 :card "Creative Commission"}
             {:qty 3 :card "Deep Dive"}
             {:qty 3 :card "Diesel"}
             {:qty 3 :card "Spec Work"}
             {:qty 2 :card "Spree"}
             {:qty 3 :card "Trick Shot"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Pinhole Threading"}
             {:qty 2 :card "Aniccam"}
             {:qty 3 :card "Simulchip"}
             {:qty 1 :card "Coalescence"}
             {:qty 1 :card "Euler"}
             {:qty 1 :card "Gauss"}
             {:qty 1 :card "Ika"}
             {:qty 3 :card "Muse"}
             {:qty 1 :card "Paricia"}
             {:qty 1 :card "Pichação"}
             {:qty 1 :card "Propeller"}
             {:qty 1 :card "Self-modifying Code"}
             {:qty 1 :card "Botulus"}
             {:qty 2 :card "Fermenter"}
             {:qty 1 :card "Physarum Entangler"}
             {:qty 2 :card "Environmental Testing"}
             {:qty 2 :card "Daily Casts"}
             {:qty 1 :card "DJ Fenris"}
             {:qty 1 :card "Hannah \"Wheels\" Pilintra"}])))

(def worlds-2025-zomzraft-runs
  (matchup
    [:preconstructed.worlds-2025-a "Worlds 2025: davz131 (C) vs. ZomZraft (C)"]
    [:preconstructed.worlds-2025-a-tag "davz131 (R) vs. ZomZraft (C)"]
    [:preconstructed.worlds-2025-info "361 players played in the fourth Netrunner world championship run by Null Signal Games. In this tournament, ZomZraft (Epiphany, Hoshiko) won the title of Netrunner World Champion in a final game Against davz131 (Au Co, Esa). The tournament was held at Dovecot Studios in Edenburg on the 18th and 19th of October, and consisted of 14 rounds of Single-Sided Swiss into a top 16 cut. The legal cardpool consisted of cards up to Elevation."]
    [:preconstructed.worlds-2025-a-ul "Worlds 2025: SBT Bytes vs. Vampire"]
    (precon "davz131 - 2025: SBT Bytes"
            {:title "AU Co.: The Gold Standard in Clones" :side "Corp" :code "35046"}
            [{:qty 3 :card "Fujii Asset Retrieval"}
             {:qty 1 :card "Longevity Serum"}
             {:qty 3 :card "See How They Run"}
             {:qty 3 :card "False Lead"}
             {:qty 3 :card "Bladderwort"}
             {:qty 2 :card "Byte!"}
             {:qty 3 :card "Cohort Guidance Program"}
             {:qty 3 :card "Moon Pool"}
             {:qty 3 :card "Phật Gioan Baotixita"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 3 :card "Anemone"}
             {:qty 1 :card "Diviner"}
             {:qty 1 :card "Phoneutria"}
             {:qty 3 :card "Semak-samun"}
             {:qty 1 :card "Flyswatter"}
             {:qty 3 :card "Hansei Review"}
             {:qty 2 :card "Mindscaping"}
             {:qty 1 :card "Hedge Fund"}
             {:qty 3 :card "Petty Cash"}
             {:qty 2 :card "Oppo Research"}
             {:qty 2 :card "End of the Line"}])
    (precon "ZomZraft - 2025: Vampire"
            {:title "Hoshiko Shiro: Untold Protagonist" :side "Runner" :code "26066"}
            [{:qty 3 :card "Steelskin Scarring"}
             {:qty 3 :card "Strike Fund"}
             {:qty 3 :card "The Price"}
             {:qty 1 :card "Ashen Epilogue"}
             {:qty 3 :card "Pinhole Threading"}
             {:qty 1 :card "Illumination"}
             {:qty 3 :card "Bling"}
             {:qty 3 :card "Devil Charm"}
             {:qty 3 :card "Gachapon"}
             {:qty 1 :card "Solidarity Badge"}
             {:qty 1 :card "Buffer Drive"}
             {:qty 3 :card "Simulchip"}
             {:qty 2 :card "Audrey v2"}
             {:qty 3 :card "Botulus"}
             {:qty 3 :card "Chisel"}
             {:qty 3 :card "Fermenter"}
             {:qty 3 :card "Gourmand"}
             {:qty 2 :card "Cookbook"}
             {:qty 1 :card "Fencer Fueno"}
             {:qty 2 :card "Lago Paranoá Shelter"}
             {:qty 2 :card "Paladin Poemu"}
             {:qty 2 :card "The Twinning"}])))

(def worlds-2025-zomzraft-corps
  (matchup
    [:preconstructed.worlds-2025-b "Worlds 2025: ZomZraft (C) vs davz131 (R)"]
    [:preconstructed.worlds-2025-b-tag "ZomZraft (C) vs. davz131 (R)"]
    [:preconstructed.worlds-2025-info "361 players played in the fourth Netrunner world championship run by Null Signal Games. In this tournament, ZomZraft (Epiphany, Hoshiko) won the title of Netrunner World Champion in a final game Against davz131 (Au Co, Esa). The tournament was held at Dovecot Studios in Edenburg on the 18th and 19th of October, and consisted of 14 rounds of Single-Sided Swiss into a top 16 cut. The legal cardpool consisted of cards up to Elevation."]
    [:preconstructed.worlds-2025-b-ul "Worlds 2025: Constrictor vs. Esâ me, I'm back"]
    (precon "ZomZraft - 2025: Constrictor"
            {:title "Epiphany Analytica: Nations Undivided" :side "Corp" :code "34048"}
            [{:qty 3 :card "Artificial Cryptocrash"}
             {:qty 2 :card "Freedom of Information"}
             {:qty 3 :card "Stoke the Embers"}
             {:qty 1 :card "Tomorrow's Headline"}
             {:qty 1 :card "Offworld Office"}
             {:qty 3 :card "Balanced Coverage"}
             {:qty 1 :card "Chekist Scion"}
             {:qty 3 :card "Federal Fundraising"}
             {:qty 1 :card "Public Access Plaza"}
             {:qty 3 :card "Spin Doctor"}
             {:qty 3 :card "B-1001"}
             {:qty 2 :card "The Powers That Be"}
             {:qty 1 :card "Wage Workers"}
             {:qty 2 :card "Warm Reception"}
             {:qty 3 :card "Working Prototype"}
             {:qty 1 :card "Mestnichestvo"}
             {:qty 3 :card "Ping"}
             {:qty 1 :card "Starlit Knight"}
             {:qty 3 :card "Virtual Service Agent"}
             {:qty 1 :card "Bigger Picture"}
             {:qty 1 :card "IP Enforcement"}
             {:qty 3 :card "Oppo Research"}
             {:qty 1 :card "Shipment from Vladisibirsk"}
             {:qty 1 :card "Retribution"}
             {:qty 2 :card "The Holo Man"}])
    (precon "davz131 - 2025: Esâ me, I'm back"
            {:title "Esâ Afontov: Eco-Insurrectionist" :side "Runner" :code "33001"}
            [{:qty 2 :card "Chastushka"}
             {:qty 2 :card "Finality"}
             {:qty 1 :card "Katorga Breakout"}
             {:qty 3 :card "Raindrops Cut Stone"}
             {:qty 2 :card "Running Hot"}
             {:qty 3 :card "Steelskin Scarring"}
             {:qty 3 :card "Strike Fund"}
             {:qty 3 :card "Wildcat Strike"}
             {:qty 1 :card "Ashen Epilogue"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Pinhole Threading"}
             {:qty 1 :card "Ritual"}
             {:qty 3 :card "Ghosttongue"}
             {:qty 3 :card "Marrow"}
             {:qty 1 :card "Hippocampic Mechanocytes"}
             {:qty 2 :card "Begemot"}
             {:qty 1 :card "Fermenter"}
             {:qty 5 :card "Matryoshka"}
             {:qty 1 :card "Cupellation"}
             {:qty 1 :card "Mystic Maemi"}
             {:qty 3 :card "Dr. Nuka Vrolyck"}])))

(def classique-blurb
  "The classique format is a very accessible way for newer players to try some examples of classic netrunner decks.")

;; Classique 2022
(def classique-2022-foodcoats-vs-book-of-kate
  (matchup
    [:preconstructed.classique-2022-a "Classique 2022: Foodcoats (C) vs. Book of Kate (R)"]
    [:preconstructed.classique-2022-a-tag "Foodcoats (C) vs. Book of Kate (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2022-a-ul "Classique 2022: Foodcoats vs. Book of Kate"]
    (precon "Classique 2022: Foodcoats"
            {:title "Haas-Bioroid: Engineering the Future" :side "Corp" :code "01054"}
            [{:qty 3 :card "Accelerated Beta Test"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "NAPD Contract"}
             {:qty 3 :card "Adonis Campaign"}
             {:qty 3 :card "Eve Campaign"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 3 :card "Ash 2X3ZB9CY"}
             {:qty 3 :card "Breaker Bay Grid"}
             {:qty 2 :card "Caprice Nisei"}
             {:qty 1 :card "Crisium Grid"}
             {:qty 2 :card "Archived Memories"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Architect"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 2 :card "Enigma"}
             {:qty 3 :card "Ichi 1.0"}
             {:qty 2 :card "Ichi 2.0"}
             {:qty 3 :card "Turing"}
             {:qty 1 :card "Wall of Static"}]
            "https://netrunnerdb.com/en/decklist/3f40bbd4-ac95-4afc-9a45-b6a623cc0049")
    (precon "Classique 2022: Book of Kate"
            {:title "Kate \"Mac\" McCaffrey: Digital Tinker" :side "Runner" :code "01033"}
            [{:qty 3 :card "Diesel"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 1 :card "Legwork"}
             {:qty 1 :card "Levy AR Lab Access"}
             {:qty 3 :card "Lucky Find"}
             {:qty 2 :card "Quality Time"}
             {:qty 1 :card "Scavenge"}
             {:qty 1 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "The Maker's Eye"}
             {:qty 2 :card "Astrolabe"}
             {:qty 2 :card "Clone Chip"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 3 :card "Prepaid VoicePAD"}
             {:qty 1 :card "R&D Interface"}
             {:qty 3 :card "Daily Casts"}
             {:qty 1 :card "Film Critic"}
             {:qty 1 :card "Same Old Thing"}
             {:qty 1 :card "Utopia Shard"}
             {:qty 1 :card "Atman"}
             {:qty 2 :card "Cerberus \"Lady\" H1"}
             {:qty 1 :card "D4v1d"}
             {:qty 1 :card "Gordian Blade"}
             {:qty 1 :card "Mimic"}
             {:qty 3 :card "Self-modifying Code"}]
            "https://netrunnerdb.com/en/decklist/520c62fb-38f3-4a5f-91e4-1050689730fa")))

(def classique-2022-grail-neh-vs-endless-waltz
  (matchup
    [:preconstructed.classique-2022-b "Classique 2022: Grail NEH (C) vs. Endless Waltz (R)"]
    [:preconstructed.classique-2022-b-tag "Grail NEH (C) vs. Endless Waltz (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2022-b-ul "Classique 2022: Grail NEH vs. Endless Waltz"]
    (precon "Classique 2022: Grail NEH"
            {:title "Near-Earth Hub: Broadcast Center" :side "Corp" :code "06005"}
            [{:qty 1 :card "AstroScript Pilot Program"}
             {:qty 2 :card "Breaking News"}
             {:qty 3 :card "NAPD Contract"}
             {:qty 3 :card "Project Beale"}
             {:qty 2 :card "Remastered Edition"}
             {:qty 1 :card "Blacklist"}
             {:qty 2 :card "Daily Business Show"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 3 :card "PAD Campaign"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "SanSan City Grid"}
             {:qty 2 :card "Biotic Labor"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 2 :card "Shipment from SanSan"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 3 :card "Galahad"}
             {:qty 3 :card "Lancelot"}
             {:qty 3 :card "Merlin"}
             {:qty 3 :card "Pop-up Window"}
             {:qty 3 :card "Wraparound"}]
            "https://netrunnerdb.com/en/decklist/002dd16d-b350-4f67-80f4-6d5462fe184c")
    (precon "Classique 2022: Endless Waltz"
            {:title "Leela Patel: Trained Pragmatist" :side "Runner" :code "06095"}
            [{:qty 3 :card "Account Siphon"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 1 :card "Emergency Shutdown"}
             {:qty 2 :card "Inside Job"}
             {:qty 1 :card "Legwork"}
             {:qty 3 :card "Special Order"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Desperado"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 3 :card "R&D Interface"}
             {:qty 3 :card "Bank Job"}
             {:qty 2 :card "John Masanori"}
             {:qty 1 :card "Kati Jones"}
             {:qty 3 :card "Security Testing"}
             {:qty 1 :card "Utopia Shard"}
             {:qty 2 :card "Corroder"}
             {:qty 3 :card "Faerie"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 1 :card "Mimic"}
             {:qty 1 :card "Passport"}
             {:qty 2 :card "Sneakdoor Beta"}
             {:qty 1 :card "Yog.0"}
             {:qty 1 :card "ZU.13 Key Master"}]
            "https://netrunnerdb.com/en/decklist/2cae2c77-333f-44ed-a964-46e968357feb")))

(def classique-2022-ctm-vs-reg-whizz
  (matchup
    [:preconstructed.classique-2022-c "Classique 2022: CtM (C) vs. Reg Whizz (R)"]
    [:preconstructed.classique-2022-c-tag "CtM (C) vs. Reg Whizz (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2022-c-ul "Classique 2022: CtM vs. Reg Whizz"]
    (precon "Classique 2022: CtM"
            {:title "NBN: Controlling the Message" :side "Corp" :code "11017"}
            [{:qty 1 :card "AstroScript Pilot Program"}
             {:qty 3 :card "Breaking News"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Project Beale"}
             {:qty 2 :card "Commercial Bankers Group"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 1 :card "PAD Campaign"}
             {:qty 3 :card "Sensie Actors Union"}
             {:qty 2 :card "Mumbad Virtual Tour"}
             {:qty 2 :card "SanSan City Grid"}
             {:qty 2 :card "Closed Accounts"}
             {:qty 2 :card "Exchange of Information"}
             {:qty 2 :card "Hard-Hitting News"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Psychographics"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 2 :card "Archangel"}
             {:qty 1 :card "Cobra"}
             {:qty 1 :card "Enigma"}
             {:qty 2 :card "Pop-up Window"}
             {:qty 3 :card "Resistor"}
             {:qty 2 :card "Tollbooth"}
             {:qty 2 :card "Turnpike"}]
            "https://netrunnerdb.com/en/decklist/bd4dd590-6191-4c29-a195-4473059bf63b")
    (precon "Classique 2022: Reg Whizz"
            {:title "Whizzard: Master Gamer" :side "Runner" :code "02001"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 2 :card "Déjà Vu"}
             {:qty 2 :card "Employee Strike"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 2 :card "Inject"}
             {:qty 1 :card "Retrieval Run"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Net-Ready Eyes"}
             {:qty 2 :card "Obelus"}
             {:qty 3 :card "Daily Casts"}
             {:qty 1 :card "Earthrise Hotel"}
             {:qty 1 :card "Ice Carver"}
             {:qty 1 :card "Liberated Account"}
             {:qty 3 :card "Street Peddler"}
             {:qty 3 :card "Temüjin Contract"}
             {:qty 2 :card "Datasucker"}
             {:qty 2 :card "Medium"}
             {:qty 2 :card "Mimic"}
             {:qty 2 :card "Paperclip"}
             {:qty 3 :card "Parasite"}
             {:qty 1 :card "Progenitor"}
             {:qty 2 :card "Yog.0"}]
            "https://netrunnerdb.com/en/decklist/b14c2b27-ebca-4d88-9e84-569ef1e11e54")))

(def classique-2022-supermodernism-argus-vs-crowdfunding-val
  (matchup
    [:preconstructed.classique-2022-d "Classique 2022: Supermodernism Argus (C) vs. Crowdfunding Val (R)"]
    [:preconstructed.classique-2022-d-tag "Supermodernism Argus (C) vs. Crowdfunding Val (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2022-d-ul "Classique 2022: Supermodernism Argus vs. Crowdfunding Val"]
    (precon "Classique 2022: Supermodernism Argus"
            {:title "Argus Security: Protection Guaranteed" :side "Corp" :code "07001"}
            [{:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Hostile Takeover"}
             {:qty 1 :card "Oaktown Renovation"}
             {:qty 3 :card "Project Atlas"}
             {:qty 3 :card "NGO Front"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Prisec"}
             {:qty 1 :card "Audacity"}
             {:qty 1 :card "Consulting Visit"}
             {:qty 2 :card "Economic Warfare"}
             {:qty 1 :card "Fast Track"}
             {:qty 3 :card "Hard-Hitting News"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 2 :card "High-Profile Target"}
             {:qty 1 :card "IPO"}
             {:qty 2 :card "Too Big to Fail"}
             {:qty 2 :card "Archer"}
             {:qty 3 :card "Border Control"}
             {:qty 3 :card "Data Raven"}
             {:qty 2 :card "Hortum"}
             {:qty 1 :card "Ice Wall"}
             {:qty 3 :card "Mausolus"}]
            "https://netrunnerdb.com/en/decklist/fa0cb0ad-5d70-4e90-832a-c1101f9254b6")
    (precon "Classique 2022: Crowdfunding Val"
            {:title "Valencia Estevez: The Angel of Cayambe" :side "Runner" :code "07030"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Hacktivist Meeting"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 3 :card "Inject"}
             {:qty 2 :card "Mining Accident"}
             {:qty 1 :card "Rebirth"}
             {:qty 3 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Hippo"}
             {:qty 1 :card "Turntable"}
             {:qty 3 :card "Crowdfunding"}
             {:qty 3 :card "Daily Casts"}
             {:qty 2 :card "Earthrise Hotel"}
             {:qty 2 :card "Liberated Account"}
             {:qty 3 :card "The Turning Wheel"}
             {:qty 2 :card "Aumakua"}
             {:qty 3 :card "Black Orchestra"}
             {:qty 2 :card "Datasucker"}
             {:qty 2 :card "MKUltra"}
             {:qty 3 :card "Paperclip"}]
            "https://netrunnerdb.com/en/decklist/2fa90df8-0d4e-435c-8556-5271ba5b1c8e")))

;; Classique 2023
(def classique-2023-panic-palana-vs-noise-shop
  (matchup
    [:preconstructed.classique-2023-a "Classique 2023: Panic Palana (C) vs. Noise Shop (R)"]
    [:preconstructed.classique-2023-a-tag "Panic Palana (C) vs. Noise Shop (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2023-a-ul "Classique 2023: Panic Palana vs. Noise Shop"]
    (precon "Classique 2023: Panic Palana"
            {:title "Pālanā Foods: Sustainable Growth" :side "Corp" :code "10030"}
            [{:qty 3 :card "Corporate Sales Team"}
             {:qty 2 :card "Global Food Initiative"}
             {:qty 3 :card "Nisei MK II"}
             {:qty 1 :card "Philotic Entanglement"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 2 :card "Launch Campaign"}
             {:qty 3 :card "Caprice Nisei"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "Marcus Batty"}
             {:qty 3 :card "Celebrity Gift"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Interns"}
             {:qty 3 :card "Restructure"}
             {:qty 1 :card "Assassin"}
             {:qty 1 :card "Cobra"}
             {:qty 2 :card "Crick"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 2 :card "Lotus Field"}
             {:qty 3 :card "Pup"}
             {:qty 2 :card "Swordsman"}
             {:qty 3 :card "Viper"}
             {:qty 1 :card "Wraparound"}]
            "https://netrunnerdb.com/en/decklist/cb0de9b9-bd6a-4a2b-beab-2b2bd1586b43")
    (precon "Classique 2023: Noise Shop"
            {:title "Noise: Hacker Extraordinaire" :side "Runner" :code "01001"}
            [{:qty 3 :card "Déjà Vu"}
             {:qty 3 :card "Inject"}
             {:qty 1 :card "Levy AR Lab Access"}
             {:qty 2 :card "Grimoire"}
             {:qty 2 :card "Adjusted Chronotype"}
             {:qty 3 :card "Aesop's Pawnshop"}
             {:qty 3 :card "Street Peddler"}
             {:qty 3 :card "Wyldside"}
             {:qty 3 :card "Cache"}
             {:qty 2 :card "Clot"}
             {:qty 2 :card "D4v1d"}
             {:qty 3 :card "Datasucker"}
             {:qty 2 :card "Faust"}
             {:qty 3 :card "Imp"}
             {:qty 3 :card "Lamprey"}
             {:qty 2 :card "Medium"}
             {:qty 1 :card "Mimic"}
             {:qty 3 :card "Parasite"}
             {:qty 1 :card "Scheherazade"}]
            "https://netrunnerdb.com/en/decklist/9feb53a6-d470-46fe-ad58-68b54e431b77")))

(def classique-2023-tablet-asa-vs-core-set-waltz
  (matchup
    [:preconstructed.classique-2023-b "Classique 2023: Tablet Asa (C) vs. Core Set Waltz (R)"]
    [:preconstructed.classique-2023-b-tag "Tablet Asa (C) vs. Core Set Waltz (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2023-b-ul "Classique 2023: Tablet Asa vs. Core Set Waltz"]
    (precon "Classique 2023: Tablet Asa"
            {:title "Asa Group: Security Through Vigilance" :side "Corp" :code "21009"}
            [{:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 3 :card "Successful Field Test"}
             {:qty 3 :card "Cybernetics Court"}
             {:qty 2 :card "Daily Quest"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 2 :card "Lakshmi Smartfabrics"}
             {:qty 2 :card "MCA Austerity Policy"}
             {:qty 3 :card "Marilyn Campaign"}
             {:qty 3 :card "Mumba Temple"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 1 :card "Biotic Labor"}
             {:qty 3 :card "Fully Operational"}
             {:qty 3 :card "Violet Level Clearance"}
             {:qty 1 :card "Drafter"}
             {:qty 1 :card "Fairchild 3.0"}
             {:qty 3 :card "Gatekeeper"}
             {:qty 3 :card "Hagen"}
             {:qty 3 :card "Tour Guide"}]
            "https://netrunnerdb.com/en/decklist/7cec66ed-30f5-45bd-90ab-c9aee62742e7")
    (precon "Classique 2023: Core Set Waltz"
            {:title "Leela Patel: Trained Pragmatist" :side "Runner" :code "06095"}
            [{:qty 3 :card "Bravado"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Diversion of Funds"}
             {:qty 2 :card "Embezzle"}
             {:qty 2 :card "Inside Job"}
             {:qty 1 :card "Legwork"}
             {:qty 1 :card "Special Order"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "The Maker's Eye"}
             {:qty 2 :card "Aniccam"}
             {:qty 3 :card "Boomerang"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "PAD Tap"}
             {:qty 1 :card "Political Operative"}
             {:qty 2 :card "The Class Act"}
             {:qty 2 :card "The Turning Wheel"}
             {:qty 2 :card "Amina"}
             {:qty 2 :card "Bukhgalter"}
             {:qty 1 :card "Paperclip"}
             {:qty 2 :card "Rezeki"}
             {:qty 1 :card "Sneakdoor Beta"}
             {:qty 2 :card "Tapwrm"}]
            "https://netrunnerdb.com/en/decklist/5da194aa-0647-4e38-9597-86c47da4f9a9")))

(def classique-2023-fastrobiotics-vs-ppvp-kate
  (matchup
    [:preconstructed.classique-2023-c "Classique 2023: Fastrobiotics (C) vs. PPVP Kate (R)"]
    [:preconstructed.classique-2023-c-tag "Fastrobiotics (C) vs. PPVP Kate (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2023-c-ul "Classique 2023: Fastrobiotics vs. PPVP Kate"]
    (precon "Classique 2023: Fastrobiotics"
            {:title "Near-Earth Hub: Broadcast Center" :side "Corp" :code "06005"}
            [{:qty 1 :card "AstroScript Pilot Program"}
             {:qty 2 :card "Breaking News"}
             {:qty 2 :card "Evidence Collection"}
             {:qty 3 :card "NAPD Contract"}
             {:qty 3 :card "Project Beale"}
             {:qty 1 :card "Daily Business Show"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 3 :card "PAD Campaign"}
             {:qty 2 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "SanSan City Grid"}
             {:qty 2 :card "Biotic Labor"}
             {:qty 1 :card "Fast Track"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 2 :card "Shipment from SanSan"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 2 :card "Architect"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 2 :card "Enigma"}
             {:qty 1 :card "Ichi 1.0"}
             {:qty 3 :card "Pop-up Window"}
             {:qty 2 :card "Tollbooth"}
             {:qty 2 :card "Wraparound"}]
            "https://netrunnerdb.com/en/decklist/66db5f78-2158-48a8-ad9a-840d802f4227")
    (precon "Classique 2023: PPVP Kate"
            {:title "Kate \"Mac\" McCaffrey: Digital Tinker" :side "Runner" :code "01033"}
            [{:qty 3 :card "Diesel"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 1 :card "Legwork"}
             {:qty 1 :card "Levy AR Lab Access"}
             {:qty 3 :card "Lucky Find"}
             {:qty 2 :card "Quality Time"}
             {:qty 1 :card "Scavenge"}
             {:qty 1 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "The Maker's Eye"}
             {:qty 2 :card "Astrolabe"}
             {:qty 3 :card "Clone Chip"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 3 :card "Prepaid VoicePAD"}
             {:qty 1 :card "R&D Interface"}
             {:qty 1 :card "Same Old Thing"}
             {:qty 1 :card "Atman"}
             {:qty 2 :card "Cerberus \"Lady\" H1"}
             {:qty 1 :card "Clot"}
             {:qty 1 :card "Cyber-Cypher"}
             {:qty 1 :card "Datasucker"}
             {:qty 1 :card "Mimic"}
             {:qty 1 :card "Parasite"}
             {:qty 3 :card "Self-modifying Code"}
             {:qty 1 :card "Sharpshooter"}
             {:qty 1 :card "ZU.13 Key Master"}]
            "https://netrunnerdb.com/en/decklist/2ecb0879-e262-4f08-a77f-ccfbde83c090")))

(def classique-2023-supermodernism-argus-vs-crowdfunding-val
  (matchup
    [:preconstructed.classique-2023-d "Classique 2023: Supermodernism Argus (C) vs. Crowdfunding Val (R)"]
    [:preconstructed.classique-2023-d-tag "Supermodernism Argus (C) vs. Crowdfunding Val (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2023-d-ul "Classique 2023: Supermodernism Argus vs. Crowdfunding Val"]
    (precon "Classique 2023: Supermodernism Argus"
            {:title "Argus Security: Protection Guaranteed" :side "Corp" :code "07001"}
            [{:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Hostile Takeover"}
             {:qty 1 :card "Oaktown Renovation"}
             {:qty 3 :card "Project Atlas"}
             {:qty 3 :card "NGO Front"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Prisec"}
             {:qty 1 :card "Audacity"}
             {:qty 1 :card "Consulting Visit"}
             {:qty 2 :card "Economic Warfare"}
             {:qty 1 :card "Fast Track"}
             {:qty 3 :card "Hard-Hitting News"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 2 :card "High-Profile Target"}
             {:qty 1 :card "IPO"}
             {:qty 2 :card "Too Big to Fail"}
             {:qty 2 :card "Archer"}
             {:qty 3 :card "Border Control"}
             {:qty 3 :card "Data Raven"}
             {:qty 2 :card "Hortum"}
             {:qty 1 :card "Ice Wall"}
             {:qty 3 :card "Mausolus"}]
            "https://netrunnerdb.com/en/decklist/a23906a9-d08a-4739-a123-8d18a4145f52")
    (precon "Classique 2023: Crowdfunding Val"
            {:title "Valencia Estevez: The Angel of Cayambe" :side "Runner" :code "07030"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Hacktivist Meeting"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 3 :card "Inject"}
             {:qty 2 :card "Mining Accident"}
             {:qty 1 :card "Rebirth"}
             {:qty 3 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Hippo"}
             {:qty 1 :card "Turntable"}
             {:qty 3 :card "Crowdfunding"}
             {:qty 3 :card "Daily Casts"}
             {:qty 2 :card "Earthrise Hotel"}
             {:qty 2 :card "Liberated Account"}
             {:qty 3 :card "The Turning Wheel"}
             {:qty 2 :card "Aumakua"}
             {:qty 3 :card "Black Orchestra"}
             {:qty 2 :card "Datasucker"}
             {:qty 2 :card "MKUltra"}
             {:qty 3 :card "Paperclip"}]
            "https://netrunnerdb.com/en/decklist/54af876d-89f1-4dc9-a6d8-6d1e5a6e40a5")))

;; Classique 2025
(def classique-2025-cambridge-pe-vs-classic-andy
  (matchup
    [:preconstructed.classique-2025-a "Classique 2025: Cambridge PE (C) vs. Classic Andy (R)"]
    [:preconstructed.classique-2025-a-tag "Cambridge PE (C) vs. Classic Andy (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2025-a-ul "Classique 2025: Cambridge PE vs. Classic Andy"]
    (precon "Classique 2025: Cambridge PE"
            {:title "Jinteki: Personal Evolution" :side "Corp" :code "01067"}
            [{:qty 3 :card "Fetal AI"}
             {:qty 3 :card "Gila Hands Arcology"}
             {:qty 3 :card "House of Knives"}
             {:qty 1 :card "Philotic Entanglement"}
             {:qty 2 :card "The Future Perfect"}
             {:qty 2 :card "Jackson Howard"}
             {:qty 1 :card "Project Junebug"}
             {:qty 3 :card "Psychic Field"}
             {:qty 3 :card "Ronin"}
             {:qty 1 :card "Shattered Remains"}
             {:qty 3 :card "Snare!"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "Mushin No Shin"}
             {:qty 3 :card "Neural EMP"}
             {:qty 1 :card "Scorched Earth"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 3 :card "Eli 1.0"}
             {:qty 2 :card "Enigma"}
             {:qty 2 :card "Komainu"}
             {:qty 1 :card "Neural Katana"}
             {:qty 2 :card "Pup"}
             {:qty 1 :card "Yagura"}]
            "https://netrunnerdb.com/en/decklist/cbe92073-0b92-4365-87b5-a237a5298654")
    (precon "Classique 2025: Classic Andy"
            {:title "Andromeda: Dispossessed Ristie" :side "Runner" :code "02083"}
            [{:qty 3 :card "Account Siphon"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 1 :card "Emergency Shutdown"}
             {:qty 1 :card "Express Delivery"}
             {:qty 1 :card "Infiltration"}
             {:qty 1 :card "Inside Job"}
             {:qty 2 :card "Legwork"}
             {:qty 2 :card "Quality Time"}
             {:qty 3 :card "Special Order"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 3 :card "Desperado"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 2 :card "R&D Interface"}
             {:qty 1 :card "Daily Casts"}
             {:qty 2 :card "Kati Jones"}
             {:qty 1 :card "Same Old Thing"}
             {:qty 3 :card "Security Testing"}
             {:qty 2 :card "Corroder"}
             {:qty 3 :card "Datasucker"}
             {:qty 3 :card "Faerie"}
             {:qty 1 :card "Femme Fatale"}
             {:qty 1 :card "Mimic"}
             {:qty 1 :card "Passport"}
             {:qty 1 :card "Yog.0"}]
            "https://netrunnerdb.com/en/decklist/dfe43bba-b1ef-46a7-8094-8a755bd99525")))

(def classique-2025-ctm-vs-reg-whizz
  (matchup
    [:preconstructed.classique-2025-b "Classique 2025: CtM (C) vs. Reg Whizz (R)"]
    [:preconstructed.classique-2025-b-tag "CtM (C) vs. Reg Whizz (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2025-b-ul "Classique 2025: CtM vs. Reg Whizz"]
    (precon "Classique 2025: CtM"
            {:title "NBN: Controlling the Message" :side "Corp" :code "11017"}
            [{:qty 1 :card "AstroScript Pilot Program"}
             {:qty 3 :card "Breaking News"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Project Beale"}
             {:qty 2 :card "Commercial Bankers Group"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 1 :card "PAD Campaign"}
             {:qty 3 :card "Sensie Actors Union"}
             {:qty 2 :card "Mumbad Virtual Tour"}
             {:qty 2 :card "SanSan City Grid"}
             {:qty 2 :card "Closed Accounts"}
             {:qty 2 :card "Exchange of Information"}
             {:qty 2 :card "Hard-Hitting News"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Psychographics"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 2 :card "Archangel"}
             {:qty 1 :card "Cobra"}
             {:qty 1 :card "Enigma"}
             {:qty 2 :card "Pop-up Window"}
             {:qty 3 :card "Resistor"}
             {:qty 2 :card "Tollbooth"}
             {:qty 2 :card "Turnpike"}]
            "https://netrunnerdb.com/en/decklist/55b1c17a-58d5-4cc7-bf06-ab5783e8f19d")
    (precon "Classique 2025: Reg Whizz"
            {:title "Whizzard: Master Gamer" :side "Runner" :code "02001"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 2 :card "Déjà Vu"}
             {:qty 2 :card "Employee Strike"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 2 :card "Inject"}
             {:qty 1 :card "Retrieval Run"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Net-Ready Eyes"}
             {:qty 2 :card "Obelus"}
             {:qty 3 :card "Daily Casts"}
             {:qty 1 :card "Earthrise Hotel"}
             {:qty 1 :card "Ice Carver"}
             {:qty 1 :card "Liberated Account"}
             {:qty 3 :card "Street Peddler"}
             {:qty 3 :card "Temüjin Contract"}
             {:qty 2 :card "Datasucker"}
             {:qty 2 :card "Medium"}
             {:qty 2 :card "Mimic"}
             {:qty 2 :card "Paperclip"}
             {:qty 3 :card "Parasite"}
             {:qty 1 :card "Progenitor"}
             {:qty 2 :card "Yog.0"}]
            "https://netrunnerdb.com/en/decklist/21a89387-2395-4022-9b97-64b6b0a97309")))

(def classique-2025-reversed-stinson-ci-vs-german-geist
  (matchup
    [:preconstructed.classique-2025-c "Classique 2025: Reversed Stinson CI (C) vs. German Geist (R)"]
    [:preconstructed.classique-2025-c-tag "Reversed Stinson CI (C) vs. German Geist (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2025-c-ul "Classique 2025: Reversed Stinson CI vs. German Geist"]
    (precon "Classique 2025: Reversed Stinson CI"
            {:title "Cerebral Imaging: Infinite Frontiers" :side "Corp" :code "03001"}
            [{:qty 1 :card "Corporate Sales Team"}
             {:qty 3 :card "Efficiency Committee"}
             {:qty 2 :card "Elective Upgrade"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 3 :card "MCA Austerity Policy"}
             {:qty 3 :card "Reversed Accounts"}
             {:qty 2 :card "Bryan Stinson"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "Biotic Labor"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "IPO"}
             {:qty 2 :card "Shipment from Tennin"}
             {:qty 3 :card "Ultraviolet Clearance"}
             {:qty 3 :card "Violet Level Clearance"}
             {:qty 2 :card "Architect"}
             {:qty 1 :card "Bastion"}
             {:qty 1 :card "Enigma"}
             {:qty 1 :card "Fairchild 2.0"}
             {:qty 3 :card "Fairchild 3.0"}
             {:qty 1 :card "Ichi 1.0"}
             {:qty 2 :card "Vanilla"}]
            "https://netrunnerdb.com/en/decklist/37fe16e0-0035-4bfe-9c1c-04cf72f305af")
    (precon "Classique 2025: German Geist"
            {:title "Armand \"Geist\" Walker: Tech Lord" :side "Runner" :code "08063"}
            [{:qty 1 :card "\"Freedom Through Equality\""}
             {:qty 3 :card "Calling in Favors"}
             {:qty 1 :card "Information Sifting"}
             {:qty 1 :card "Legwork"}
             {:qty 2 :card "Levy AR Lab Access"}
             {:qty 1 :card "On the Lam"}
             {:qty 1 :card "Forger"}
             {:qty 3 :card "Sports Hopper"}
             {:qty 5 :card "Spy Camera"}
             {:qty 2 :card "Dean Lister"}
             {:qty 1 :card "Drug Dealer"}
             {:qty 3 :card "Fall Guy"}
             {:qty 2 :card "Maxwell James"}
             {:qty 3 :card "Off-Campus Apartment"}
             {:qty 1 :card "Political Operative"}
             {:qty 1 :card "Same Old Thing"}
             {:qty 3 :card "Street Peddler"}
             {:qty 3 :card "Tech Trader"}
             {:qty 3 :card "Underworld Contact"}
             {:qty 1 :card "Abagnale"}
             {:qty 2 :card "Aumakua"}
             {:qty 1 :card "Mongoose"}
             {:qty 1 :card "Paperclip"}]
            "https://netrunnerdb.com/en/decklist/bd685b50-492e-4a77-97c4-1ae7d757f416")))

(def classique-2025-seamusmodernism-vs-pitchfork-hayley
  (matchup
    [:preconstructed.classique-2025-d "Classique 2025: Seamusmodernism (C) vs. Pitchfork Hayley (R)"]
    [:preconstructed.classique-2025-d-tag "Seamusmodernism (C) vs. Pitchfork Hayley (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2025-d-ul "Classique 2025: Seamusmodernism vs. Pitchfork Hayley"]
    (precon "Classique 2025: Seamusmodernism"
            {:title "Argus Security: Protection Guaranteed" :side "Corp" :code "07001"}
            [{:qty 2 :card "Geothermal Fracking"}
             {:qty 3 :card "Hostile Takeover"}
             {:qty 3 :card "Oaktown Renovation"}
             {:qty 1 :card "Posted Bounty"}
             {:qty 3 :card "Project Atlas"}
             {:qty 2 :card "Jackson Howard"}
             {:qty 1 :card "Shattered Remains"}
             {:qty 3 :card "Snare!"}
             {:qty 1 :card "Crisium Grid"}
             {:qty 1 :card "Cyberdex Virus Suite"}
             {:qty 3 :card "Beanstalk Royalties"}
             {:qty 1 :card "Casting Call"}
             {:qty 1 :card "Fast Track"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Restructure"}
             {:qty 1 :card "SEA Source"}
             {:qty 3 :card "Scorched Earth"}
             {:qty 2 :card "Archer"}
             {:qty 1 :card "Changeling"}
             {:qty 1 :card "Cobra"}
             {:qty 3 :card "Enigma"}
             {:qty 1 :card "Grim"}
             {:qty 1 :card "Ice Wall"}
             {:qty 1 :card "Meru Mati"}
             {:qty 1 :card "Mother Goddess"}
             {:qty 2 :card "Spiderweb"}
             {:qty 2 :card "Swordsman"}
             {:qty 1 :card "Wraparound"}]
            "https://netrunnerdb.com/en/decklist/f80137ab-1027-4937-8270-6addf7d5c6ce")
    (precon "Classique 2025: Pitchfork Hayley"
            {:title "Hayley Kaplan: Universal Scholar" :side "Runner" :code "08025"}
            [{:qty 1 :card "Levy AR Lab Access"}
             {:qty 3 :card "Scavenge"}
             {:qty 1 :card "Stimhack"}
             {:qty 2 :card "Astrolabe"}
             {:qty 3 :card "Clone Chip"}
             {:qty 1 :card "Plascrete Carapace"}
             {:qty 3 :card "R&D Interface"}
             {:qty 2 :card "Aesop's Pawnshop"}
             {:qty 2 :card "Artist Colony"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Fan Site"}
             {:qty 1 :card "Film Critic"}
             {:qty 1 :card "Hunting Grounds"}
             {:qty 3 :card "Professional Contacts"}
             {:qty 1 :card "Same Old Thing"}
             {:qty 3 :card "Technical Writer"}
             {:qty 1 :card "Atman"}
             {:qty 3 :card "Cache"}
             {:qty 1 :card "Cerberus \"Lady\" H1"}
             {:qty 1 :card "Chameleon"}
             {:qty 1 :card "Clot"}
             {:qty 2 :card "Cyber-Cypher"}
             {:qty 1 :card "D4v1d"}
             {:qty 2 :card "Self-modifying Code"}]
            "https://netrunnerdb.com/en/decklist/4028e1eb-5da2-4e32-9278-c0235cd5926a")))

;; Classique 2026
(def classique-2026-come-on-and-slam-vs-clanaxx
  (matchup
    [:preconstructed.classique-2026-a "Classique 2026: Come On And Slam (C) vs. ClanaxX (R)"]
    [:preconstructed.classique-2026-a-tag "Come On And Slam (C) vs. ClanaxX (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2026-a-ul "Classique 2026: Come On And Slam vs. ClanaxX"]
    (precon "Classique 2026: Come On And Slam"
            {:title "Sportsmetal: Go Big or Go Home" :side "Corp" :code "22026"}
            [{:qty 1 :card "Corporate Sales Team"}
             {:qty 3 :card "Hyperloop Extension"}
             {:qty 1 :card "Ikawah Project"}
             {:qty 3 :card "Project Vitruvius"}
             {:qty 3 :card "Remote Enforcement"}
             {:qty 3 :card "Jeeves Model Bioroids"}
             {:qty 3 :card "NGO Front"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 3 :card "Arella Salvatore"}
             {:qty 1 :card "Ark Lockdown"}
             {:qty 3 :card "Biotic Labor"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "IPO"}
             {:qty 2 :card "Ultraviolet Clearance"}
             {:qty 3 :card "Architect"}
             {:qty 1 :card "Enigma"}
             {:qty 3 :card "Fairchild 3.0"}
             {:qty 3 :card "Gatekeeper"}
             {:qty 2 :card "IP Block"}
             {:qty 1 :card "Surveyor"}
             {:qty 1 :card "Tollbooth"}
             {:qty 2 :card "Vanilla"}]
            "https://netrunnerdb.com/en/decklist/f04d44e1-573b-4d54-9969-2ff5cc38e87b")
    (precon "Classique 2026: ClanaxX"
            {:title "MaxX: Maximum Punk Rock" :side "Runner" :code "07029"}
            [{:qty 3 :card "Fisk Investment Seminar"}
             {:qty 3 :card "Hacktivist Meeting"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 2 :card "Levy AR Lab Access"}
             {:qty 1 :card "Mad Dash"}
             {:qty 2 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Patchwork"}
             {:qty 1 :card "Titanium Ribs"}
             {:qty 3 :card "Zer0"}
             {:qty 3 :card "Clan Vengeance"}
             {:qty 2 :card "DDoS"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "Same Old Thing"}
             {:qty 2 :card "Black Orchestra"}
             {:qty 2 :card "D4v1d"}
             {:qty 2 :card "MKUltra"}
             {:qty 2 :card "Paperclip"}]
            "https://netrunnerdb.com/en/decklist/0ea9b4a4-b18f-44e0-a666-170ae9856b28")))

(def classique-2026-battyshop-blue-sun-vs-trash-panda-freedom
  (matchup
    [:preconstructed.classique-2026-b "Classique 2026: Battyshop Blue Sun (C) vs. Trash Panda Freedom (R)"]
    [:preconstructed.classique-2026-b-tag "Battyshop Blue Sun (C) vs. Trash Panda Freedom (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2026-b-ul "Classique 2026: Battyshop Blue Sun vs. Trash Panda Freedom"]
    (precon "Classique 2026: Battyshop Blue Sun"
            {:title "Blue Sun: Powering the Future" :side "Corp" :code "25123"}
            [{:qty 2 :card "Hostile Takeover"}
             {:qty 3 :card "SDS Drone Deployment"}
             {:qty 3 :card "SSL Endorsement"}
             {:qty 3 :card "NGO Front"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 1 :card "Crisium Grid"}
             {:qty 3 :card "Marcus Batty"}
             {:qty 3 :card "Building Blocks"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "IPO"}
             {:qty 1 :card "Preemptive Action"}
             {:qty 3 :card "Punitive Counterstrike"}
             {:qty 2 :card "Afshar"}
             {:qty 3 :card "Border Control"}
             {:qty 3 :card "Chiyashi"}
             {:qty 1 :card "Hortum"}
             {:qty 2 :card "Mausolus"}
             {:qty 1 :card "Orion"}
             {:qty 2 :card "Sapper"}
             {:qty 3 :card "Surveyor"}
             {:qty 1 :card "Tithonium"}]
            "https://netrunnerdb.com/en/decklist/cbde6e9e-1139-4b3e-bbd3-61973573ee80")
    (precon "Classique 2026: Trash Panda Freedom"
            {:title "Freedom Khumalo: Crypto-Anarchist" :side "Runner" :code "21081"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "I've Had Worse"}
             {:qty 3 :card "Inject"}
             {:qty 3 :card "Stimhack"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Clone Chip"}
             {:qty 3 :card "Hippo"}
             {:qty 2 :card "Knobkierie"}
             {:qty 3 :card "Daily Casts"}
             {:qty 3 :card "Liberated Account"}
             {:qty 3 :card "Street Peddler"}
             {:qty 2 :card "Black Orchestra"}
             {:qty 1 :card "Consume"}
             {:qty 2 :card "D4v1d"}
             {:qty 1 :card "Imp"}
             {:qty 1 :card "MKUltra"}
             {:qty 2 :card "Paperclip"}
             {:qty 1 :card "Pelangi"}
             {:qty 1 :card "Self-modifying Code"}
             {:qty 1 :card "Stargate"}
             {:qty 2 :card "Yusuf"}]
            "https://netrunnerdb.com/en/decklist/4097ef38-8ae0-4dae-a53e-0a5974f53c7a")))

(def classique-2026-post-scarcity-palana-vs-whiteblade-liza
  (matchup
    [:preconstructed.classique-2026-c "Classique 2026: Post-Scarcity Palana (C) vs. Whiteblade Liza (R)"]
    [:preconstructed.classique-2026-c-tag "Post-Scarcity Palana (C) vs. Whiteblade Liza (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2026-c-ul "Classique 2026: Post-Scarcity Palana vs. Whiteblade Liza"]
    (precon "Classique 2026: Post-Scarcity Palana"
            {:title "Pālanā Foods: Sustainable Growth" :side "Corp" :code "10030"}
            [{:qty 3 :card "Nisei MK II"}
             {:qty 3 :card "Obokata Protocol"}
             {:qty 1 :card "SSL Endorsement"}
             {:qty 1 :card "Timely Public Release"}
             {:qty 3 :card "NGO Front"}
             {:qty 3 :card "Rashida Jaheem"}
             {:qty 2 :card "Snare!"}
             {:qty 2 :card "Bio Vault"}
             {:qty 2 :card "Crisium Grid"}
             {:qty 3 :card "La Costa Grid"}
             {:qty 1 :card "Archived Memories"}
             {:qty 1 :card "Celebrity Gift"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 3 :card "IPO"}
             {:qty 2 :card "Aiki"}
             {:qty 3 :card "Anansi"}
             {:qty 3 :card "Border Control"}
             {:qty 2 :card "Excalibur"}
             {:qty 2 :card "IP Block"}
             {:qty 3 :card "Kakugo"}
             {:qty 1 :card "Macrophage"}
             {:qty 2 :card "Thimblerig"}]
            "https://netrunnerdb.com/en/decklist/05aa8a53-d7ad-43c5-8b11-34e6beffcfcb")
    (precon "Classique 2026: Whiteblade Liza"
            {:title "Liza Talking Thunder: Prominent Legislator" :side "Runner" :code "22008"}
            [{:qty 3 :card "Dirty Laundry"}
             {:qty 3 :card "Diversion of Funds"}
             {:qty 3 :card "Hot Pursuit"}
             {:qty 2 :card "Legwork"}
             {:qty 1 :card "Special Order"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 1 :card "Lucky Charm"}
             {:qty 3 :card "Paragon"}
             {:qty 1 :card "\"Baklan\" Bochkin"}
             {:qty 2 :card "Counter Surveillance"}
             {:qty 1 :card "Drug Dealer"}
             {:qty 3 :card "Dummy Box"}
             {:qty 3 :card "PAD Tap"}
             {:qty 3 :card "Paparazzi"}
             {:qty 3 :card "Rogue Trading"}
             {:qty 3 :card "Street Peddler"}
             {:qty 3 :card "The Class Act"}
             {:qty 3 :card "Wireless Net Pavilion"}
             {:qty 3 :card "Aumakua"}
             {:qty 3 :card "Femme Fatale"}]
            "https://netrunnerdb.com/en/decklist/5efe66b5-a80d-489b-818e-7ebd6d783153")))

(def classique-2026-death-rattle-ctm-vs-internet-famous-smoke
  (matchup
    [:preconstructed.classique-2026-d "Classique 2026: Death Rattle CtM (C) vs. Internet Famous Smoke (R)"]
    [:preconstructed.classique-2026-d-tag "Death Rattle CtM (C) vs. Internet Famous Smoke (R)"]
    [:preconstructed.classique-info classique-blurb]
    [:preconstructed.classique-2026-d-ul "Classique 2026: Death Rattle CtM vs. Internet Famous Smoke"]
    (precon "Classique 2026: Death Rattle CtM"
            {:title "NBN: Controlling the Message" :side "Corp" :code "11017"}
            [{:qty 1 :card "AstroScript Pilot Program"}
             {:qty 3 :card "Breaking News"}
             {:qty 3 :card "Global Food Initiative"}
             {:qty 3 :card "Project Beale"}
             {:qty 3 :card "Jackson Howard"}
             {:qty 2 :card "PAD Campaign"}
             {:qty 3 :card "Sensie Actors Union"}
             {:qty 2 :card "Mumbad Virtual Tour"}
             {:qty 2 :card "SanSan City Grid"}
             {:qty 1 :card "Biotic Labor"}
             {:qty 2 :card "Closed Accounts"}
             {:qty 2 :card "Exchange of Information"}
             {:qty 2 :card "Hard-Hitting News"}
             {:qty 3 :card "Hedge Fund"}
             {:qty 1 :card "Psychographics"}
             {:qty 3 :card "Sweeps Week"}
             {:qty 2 :card "Archangel"}
             {:qty 1 :card "Cobra"}
             {:qty 1 :card "Data Ward"}
             {:qty 1 :card "IP Block"}
             {:qty 2 :card "Pop-up Window"}
             {:qty 1 :card "Quandary"}
             {:qty 2 :card "Resistor"}
             {:qty 1 :card "Tollbooth"}
             {:qty 1 :card "Turnpike"}
             {:qty 1 :card "Wraparound"}]
            "https://netrunnerdb.com/en/decklist/ae53c577-1b0b-4360-a6ab-1b310bdc3ae3")
    (precon "Classique 2026: Internet Famous Smoke"
            {:title "Ele \"Smoke\" Scovak: Cynosure of the Net" :side "Runner" :code "11066"}
            [{:qty 1 :card "\"Freedom Through Equality\""}
             {:qty 3 :card "Diesel"}
             {:qty 3 :card "Dirty Laundry"}
             {:qty 2 :card "Indexing"}
             {:qty 3 :card "Sure Gamble"}
             {:qty 2 :card "Astrolabe"}
             {:qty 2 :card "Clone Chip"}
             {:qty 1 :card "R&D Interface"}
             {:qty 1 :card "Beth Kilrain-Chang"}
             {:qty 2 :card "Daily Casts"}
             {:qty 1 :card "Film Critic"}
             {:qty 1 :card "Ghost Runner"}
             {:qty 1 :card "Hunting Grounds"}
             {:qty 2 :card "Net Mercur"}
             {:qty 2 :card "New Angeles City Hall"}
             {:qty 1 :card "Patron"}
             {:qty 3 :card "Temüjin Contract"}
             {:qty 2 :card "Cloak"}
             {:qty 1 :card "Clot"}
             {:qty 1 :card "Corroder"}
             {:qty 1 :card "Houdini"}
             {:qty 3 :card "Self-modifying Code"}
             {:qty 1 :card "Switchblade"}]
            "https://netrunnerdb.com/en/decklist/1c7ffb1d-f12e-4b4f-bb7e-22d972683cf1")))

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
    :worlds-2017-a worlds-2017-jess-corps
    :worlds-2017-b worlds-2017-jess-runs
    :worlds-2018-a worlds-2018-joe-corps
    :worlds-2018-b worlds-2018-joe-runs
    :worlds-2019-a worlds-2019-pinsel-corps
    :worlds-2019-b worlds-2019-pinsel-runs
    :worlds-2020-a worlds-2020-limes-corps
    :worlds-2020-b worlds-2020-limes-runs
    :worlds-2021-a worlds-2021-patrick-corps
    :worlds-2021-b worlds-2021-patrick-runs
    :worlds-2022-a worlds-2022-sokka-corps
    :worlds-2022-b worlds-2022-sokka-runs
    :worlds-2023-a worlds-2023-sokka-corps
    :worlds-2023-b worlds-2023-sokka-runs
    :worlds-2024-a worlds-2024-deer-runs
    :worlds-2024-b worlds-2024-deer-corps
    :worlds-2025-a worlds-2025-zomzraft-runs
    :worlds-2025-b worlds-2025-zomzraft-corps
    :classique-2022-a classique-2022-foodcoats-vs-book-of-kate
    :classique-2022-b classique-2022-grail-neh-vs-endless-waltz
    :classique-2022-c classique-2022-ctm-vs-reg-whizz
    :classique-2022-d classique-2022-supermodernism-argus-vs-crowdfunding-val
    :classique-2023-a classique-2023-panic-palana-vs-noise-shop
    :classique-2023-b classique-2023-tablet-asa-vs-core-set-waltz
    :classique-2023-c classique-2023-fastrobiotics-vs-ppvp-kate
    :classique-2023-d classique-2023-supermodernism-argus-vs-crowdfunding-val
    :classique-2025-a classique-2025-cambridge-pe-vs-classic-andy
    :classique-2025-b classique-2025-ctm-vs-reg-whizz
    :classique-2025-c classique-2025-reversed-stinson-ci-vs-german-geist
    :classique-2025-d classique-2025-seamusmodernism-vs-pitchfork-hayley
    :classique-2026-a classique-2026-come-on-and-slam-vs-clanaxx
    :classique-2026-b classique-2026-battyshop-blue-sun-vs-trash-panda-freedom
    :classique-2026-c classique-2026-post-scarcity-palana-vs-whiteblade-liza
    :classique-2026-d classique-2026-death-rattle-ctm-vs-internet-famous-smoke))

(def all-matchups
  "A set of all preconstructed matchups (by key).
  The frontend uses this to build a matchup table for play"
  #{:worlds-2012-a :worlds-2012-b
    :worlds-2013-a :worlds-2013-b
    :worlds-2014-a :worlds-2014-b
    :worlds-2015-a :worlds-2015-b
    :worlds-2016-a :worlds-2016-b
    :worlds-2017-a :worlds-2017-b
    :worlds-2018-a :worlds-2018-b
    :worlds-2019-a :worlds-2019-b
    :worlds-2020-a :worlds-2020-b
    :worlds-2021-a :worlds-2021-b
    :worlds-2022-a :worlds-2022-b
    :worlds-2023-a :worlds-2023-b
    :worlds-2024-a :worlds-2024-b
    :worlds-2025-a :worlds-2025-b
    :classique-2022-a :classique-2022-b
    :classique-2022-c :classique-2022-d
    :classique-2023-a :classique-2023-b
    :classique-2023-c :classique-2023-d
    :classique-2025-a :classique-2025-b
    :classique-2025-c :classique-2025-d
    :classique-2026-a :classique-2026-b
    :classique-2026-c :classique-2026-d})
