(ns i18n.en
  (:require
   [clojure.string :refer [join split starts-with? ends-with?] :as s]
   [i18n.defs :refer [render-map try-catchall pprint-to-string] :include-macros true]))

(def translations
  {:missing ":en missing text"
   :side {:corp "Corp"
          :runner "Runner"
          :any-side "Any Side"
          :all "All"}
   :faction {:all "All"
             :any-faction "Any Faction"
             :anarch "Anarch"
             :criminal "Criminal"
             :shaper "Shaper"
             :adam "Adam"
             :apex "Apex"
             :sunny-lebeau "Sunny Lebeau"
             :jinteki "Jinteki"
             :haas-bioroid "Haas-Bioroid"
             :nbn "NBN"
             :weyland-consortium "Weyland Consortium"
             :neutral "Neutral"}
   :format {:all "All"
            :any-format "Any Format"
            :standard "Standard"
            :eternal "Eternal"
            :core-experience "Core Experience"
            :snapshot "Snapshot"
            :snapshot-plus "Snapshot Plus"
            :socr "SOCR"
            :sunset "Sunset"
            :throwback "Throwback"
            :neo "Neo"
            :preconstructed "Preconstructed"
            :classic "Classic"
            :casual "Casual"
            :system-gateway "System Gateway"
            :startup "Startup"}
   :preconstructed {:worlds-2012-a "Worlds 2012: Ben Marsh (C) vs. Jeremy Zwirn (R)"
                    :worlds-2012-a-tag "Ben Marsh (C) vs. Jeremy Zwirn (R)"
                    :worlds-2012-a-ul "Worlds 2012: Weyland vs. Criminal"
                    :worlds-2012-b "Worlds 2012: Jeremy Zwirn (C) vs. Ben Marsh (R)"
                    :worlds-2012-b-tag "Jeremy Zwirn (C) vs. Ben Marsh (R)"
                    :worlds-2012-b-ul "Worlds 2012: Haas-Bioroid vs. Criminal"
                    :worlds-2012-info "Worlds 2012 was played with (up to 3 copies of) the Core Set as the only legal product. Jeremy Zwirn (Building a Better World, Gabriel Santiago) took first place against Ben Marsh (Engineering the Future, Gabriel Santiago) in the first ever Netrunner World Championship."
                    :worlds-2013-a "Worlds 2013: Jens Erickson (C) vs. Andrew Veen (R)"
                    :worlds-2013-a-tag "Jens Erickson (C) vs. Andrew Veen (R)"
                    :worlds-2013-a-ul "Worlds 2013: HB FastAdv vs. Shaper Katman"
                    :worlds-2013-b "Worlds 2013: Andrew Veen (C) vs. Jens Erickson (R)"
                    :worlds-2013-b-tag "Andrew Veen (C) vs. Jens Erickson (R)"
                    :worlds-2013-b-ul "Worlds 2013: NBN Fast Adv vs. Andy Sucker"
                    :worlds-2013-info "166 players attended worlds in 2013. The tournament was held in Minneapolis, MN, USA, and consisted of 6 swiss rounds into a top 32 cut. The legal cardpool consisted of cards up to Opening Moves."
                    :worlds-2014-a "Worlds 2014: Dan D'Argenio (C) vs. Minh Tran (R)"
                    :worlds-2014-a-tag "Dan D'Argenio (C) vs. Minh Tran (R)"
                    :worlds-2014-a-ul "Worlds 2014: Honor and Perfection vs. Andromedium"
                    :worlds-2014-b "Worlds 2014: Minh Tran (C) vs. Dan D'Argenio (R)"
                    :worlds-2014-b-tag "Minh Tran (C) vs. Dan D'Argenio (R)"
                    :worlds-2014-b-ul "Worlds 2014: Personal Evolution vs. Daily QT Andy"
                    :worlds-2014-info "238 players attended worlds in 2014. The tournament was held in Minneapolis, MN, USA, and consisted of 7 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Up and Over."
                    :worlds-2015-a "Worlds 2015: Dan D'Argenio (C) vs. Timmy Wong (R)"
                    :worlds-2015-a-tag "Dan D'Argenio (C) vs. Timmy Wong (R)"
                    :worlds-2015-a-ul "Worlds 2015: Foodcoatshop vs. The Morning After"
                    :worlds-2015-b "Worlds 2015: Timmy Wong (C) vs. Dan D'Argenio (R)"
                    :worlds-2015-b-tag "Timmy Wong (C) vs. Dan D'Argenio (R)"
                    :worlds-2015-b-ul "Worlds 2015: Yellow Shell vs. Radisson Cheese Plate"
                    :worlds-2015-info "269 players attended worlds in 2015. The tournament was held in Minneapolis, MN, USA, and consisted of 8 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Data and Destiny."
                    :worlds-2016-a "Worlds 2016: Chris Dyer (C) vs. Benjamin Ni (R)"
                    :worlds-2016-a-tag "Chris Dyer (C) vs. Benjamin Ni (R)"
                    :worlds-2016-a-ul "Worlds 2016: Snekbite vs. Minh MaxX++"
                    :worlds-2016-b "Worlds 2016: Benjamin Ni (R) vs. Chris Dyer (C)"
                    :worlds-2016-b-tag "Benjamin Ni (R) vs. Chris Dyer (C)"
                    :worlds-2016-b-ul "Worlds 2016: Fiery Info vs. Papa Smurf"
                    :worlds-2016-info "278 players attended worlds in 2016. The tournament was held in Minneapolis, MN, USA, and consisted of 9 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Escalation."
                    :worlds-2017-a "Worlds 2017: Jess Horig (C) vs. Grey Tongue (R)"
                    :worlds-2017-a-tag "Jess Horig (C) vs. Grey Tongue (R)"
                    :worlds-2017-a-ul "Worlds 2017: Stinson Reversed CI vs. Aesops Hayley"
                    :worlds-2017-b "Worlds 2017: Grey Tongue (C) vs. Jess Horig (R)"
                    :worlds-2017-b-tag "Grey Tongue (C) vs. Jess Horig (R)"
                    :worlds-2017-b-ul "Worlds 2017: No-Show Rewiring CI vs. Laguna Lock Hayley"
                    :worlds-2017-info "233 players attended worlds in 2017. The tournament was held in Minneapolis, MN, USA, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Revised Core set."
                    :worlds-2018-a "Worlds 2018: Joe Schupp (C) vs. Chris Dyer (R)"
                    :worlds-2018-a-tag "Joe Schupp (C) vs. Chris Dyer (R)"
                    :worlds-2018-a-ul "Worlds 2018: AMERICA CtM vs. Gooseberry MaxX"
                    :worlds-2018-b "Worlds 2018: Chris Dyer (C) vs. Joe Schupp (R)"
                    :worlds-2018-b-tag "Chris Dyer (C) vs. Joe Schupp (R)"
                    :worlds-2018-b-ul "Worlds 2018: Trust the Process vs. Dan D'Argenio KoS Val"
                    :worlds-2018-info "403(!) players attended worlds in 2018. This is the final worlds championship to be run by FFG. The tournament was held in Minneapolis, MN, USA, and consisted of 9(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Reign and Reverie"
                    :worlds-2019-a "Worlds 2019: Pinsel (C) vs. Testrunning (R)"
                    :worlds-2019-a-tag "Pinsel (C) vs. Testrunning (R)"
                    :worlds-2019-a-ul "Worlds 2019: Fully dedicated to efficiency vs. Trash Panda"
                    :worlds-2019-b "Worlds 2019: Testrunning (C) vs. Pinsel (R)"
                    :worlds-2019-b-tag "Testrunning (C) vs. Pinsel (R)"
                    :worlds-2019-b-ul "Worlds 2019: 2 Grid for 2 Place vs. Trash Panda"
                    :worlds-2019-info "256 players played in the first ever Project NISEI Netrunner World Championship in 2019. This tournament was held in Rotterdam, NL, and consisted of 9 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Uprising Booster Pack"
                    :worlds-2020-a "Worlds 2020: Limes (C) vs. tf34 (R)"
                    :worlds-2020-a-tag "Limes (C) vs. tf34 (R)"
                    :worlds-2020-a-ul "Worlds 2020: I don't like this deck vs. Engolo Freedom"
                    :worlds-2020-b "Worlds 2020: tf34 (R) vs. Limes (C)"
                    :worlds-2020-b-tag "tf34 (R) vs. Limes (C)"
                    :worlds-2020-b-ul "Worlds 2020: Malia CTM vs. Imp-pressive Hoshiko"
                    :worlds-2020-info "294 players played in the first ever online world championship for Netrunner, run by Project NISEI 2020. Due to travel restrictions at the start of the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to Uprising."
                    :worlds-2021-a "Worlds 2021: Patrick Gower (C) vs. Jonas (R)"
                    :worlds-2021-a-tag "Patrick Gower (C) vs. Jonas (R)"
                    :worlds-2021-a-ul "Worlds 2021: 44 Card PD vs. Watch Me Drip, Watch Me Maemi"
                    :worlds-2021-b "Worlds 2021: Jonas (C) vs. Patrick Gower (R)"
                    :worlds-2021-b-tag "Jonas (C) vs. Patrick Gower (R)"
                    :worlds-2021-b-ul "Worlds 2021: Is Gagarin Good? vs. Medium to Large Maxx"
                    :worlds-2021-info "201 players played in the second online world championship for Netrunner, run by Project NISEI in 2021. Due to the ongoing disruption caused by the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to System Gateway."
                    :worlds-2022-a "Worlds 2022: William Huang (C) vs. skry (R)"
                    :worlds-2022-a-tag "William Huang (C) vs. skry (R)"
                    :worlds-2022-a-ul "Worlds 2022: SNACS vs. Liberté, Égalité, Humidité"
                    :worlds-2022-b "Worlds 2022: skry (C) vs. William Huang (R)"
                    :worlds-2022-b-tag "skry (C) vs. William Huang (R)"
                    :worlds-2022-b-ul "Worlds 2022: Dies to Doom Blade vs. ApocoLat"
                    :worlds-2022-info "158 players played in the first world championship run by Null Signal Games (formerly Project NISEI), which was the first Netrunner world championship to be run in-person since the start of the COVID-19 pandemic. The tournament was held in Toronto, Canada, and consisted of 7 rounds into a top 16 cut. The legal cardpool consisted of cards up to Midnight Sun."
                    :worlds-2023-a "Worlds 2023: William Huang (C) vs. cableCarnage (R)"
                    :worlds-2023-a-tag "William Huang (C) vs. cableCarnage (R)"
                    :worlds-2023-a-ul "Worlds 2023: The Worlds Grind vs. sableCarnage"
                    :worlds-2023-b "Worlds 2023: cableCarnage (C) vs. William Huang (R)"
                    :worlds-2023-b-tag "cableCarnage (C) vs. William Huang (R)"
                    :worlds-2023-b-ul "Worlds 2023: tableCarnage vs. You *do* always come back!"
                    :worlds-2023-info "254 players played in the second Netrunner world championship run by Null Signal Games. The tournament was held in Barcelona, Spain, and consisted of 9 rounds into a top 16 cut. The legal cardpool consisted of cards up to The Automata Initiative."}
   :set {:all "All"
         :draft-cycle "Draft Cycle"
         :draft "Draft"
         :core-set "Core Set"
         :genesis-cycle "Genesis Cycle"
         :what-lies-ahead "What Lies Ahead"
         :trace-amount "Trace Amount"
         :cyber-exodus "Cyber Exodus"
         :a-study-in-static "A Study in Static"
         :humanity-s-shadow "Humanity's Shadow"
         :future-proof "Future Proof"
         :creation-and-control "Creation and Control"
         :spin-cycle "Spin Cycle"
         :opening-moves "Opening Moves"
         :second-thoughts "Second Thoughts"
         :mala-tempora "Mala Tempora"
         :true-colors "True Colors"
         :fear-and-loathing "Fear and Loathing"
         :double-time "Double Time"
         :honor-and-profit "Honor and Profit"
         :lunar-cycle "Lunar Cycle"
         :upstalk "Upstalk"
         :the-spaces-between "The Spaces Between"
         :first-contact "First Contact"
         :up-and-over "Up and Over"
         :all-that-remains "All That Remains"
         :the-source "The Source"
         :order-and-chaos "Order and Chaos"
         :sansan-cycle "SanSan Cycle"
         :the-valley "The Valley"
         :breaker-bay "Breaker Bay"
         :chrome-city "Chrome City"
         :the-underway "The Underway"
         :old-hollywood "Old Hollywood"
         :the-universe-of-tomorrow "The Universe of Tomorrow"
         :data-and-destiny "Data and Destiny"
         :mumbad-cycle "Mumbad Cycle"
         :kala-ghoda "Kala Ghoda"
         :business-first "Business First"
         :democracy-and-dogma "Democracy and Dogma"
         :salsette-island "Salsette Island"
         :the-liberated-mind "The Liberated Mind"
         :fear-the-masses "Fear the Masses"
         :flashpoint-cycle "Flashpoint Cycle"
         :23-seconds "23 Seconds"
         :blood-money "Blood Money"
         :escalation "Escalation"
         :intervention "Intervention"
         :martial-law "Martial Law"
         :quorum "Quorum"
         :red-sand-cycle "Red Sand Cycle"
         :daedalus-complex "Daedalus Complex"
         :station-one "Station One"
         :earth-s-scion "Earth's Scion"
         :blood-and-water "Blood and Water"
         :free-mars "Free Mars"
         :crimson-dust "Crimson Dust"
         :terminal-directive-cycle "Terminal Directive Cycle"
         :terminal-directive-cards "Terminal Directive Cards"
         :terminal-directive-campaign "Terminal Directive Campaign"
         :unreleased "Unreleased"
         :revised-core-set "Revised Core Set"
         :kitara-cycle "Kitara Cycle"
         :sovereign-sight "Sovereign Sight"
         :down-the-white-nile "Down the White Nile"
         :council-of-the-crest "Council of the Crest"
         :the-devil-and-the-dragon "The Devil and the Dragon"
         :whispers-in-nalubaale "Whispers in Nalubaale"
         :kampala-ascendent "Kampala Ascendent"
         :reign-and-reverie "Reign and Reverie"
         :magnum-opus "Magnum Opus"
         :napd-multiplayer "NAPD Multiplayer"
         :system-core-2019 "System Core 2019"
         :ashes-cycle "Ashes Cycle"
         :downfall "Downfall"
         :uprising-booster-pack "Uprising Booster Pack"
         :uprising "Uprising"
         :magnum-opus-reprint "Magnum Opus Reprint"
         :salvaged-memories "Salvaged Memories"
         :system-gateway "System Gateway"
         :system-update-2021 "System Update 2021"
         :borealis-cycle "Borealis Cycle"
         :midnight-sun-booster-pack "Midnight Sun Booster Pack"
         :midnight-sun "Midnight Sun"
         :parhelion "Parhelion"
         :liberation-cycle "Liberation Cycle"
         :the-automata-initiative "The Automata Initiative"
         :rebellion-without-rehearsal "Rebellion Without Rehearsal"
         :alt-art "Alt Art"
         :kysra-alt-arts "Kysra Alt Arts"
         :ntscape-navigator-alt-arts "Ntscape Navigator Alt Arts"
         :plural-and-miniplural-alt-arts "Plural and MiniPlural Alt Arts"
         :alternate "Alternate"
         :world-champion-2015 "World Champion 2015"
         :world-champion-2016 "World Champion 2016"
         :world-champion-2017 "World Champion 2017"
         :championship-2019 "Championship 2019"
         :gnk-2019 "GNK 2019"
         :championship-2020 "Championship 2020"
         :signed-championship-2020 "Signed Championship 2020"
         :previous-versions "Previous Versions"}
   :card-type {:all "All"
               :identity "Identity"
               :agenda "Agenda"
               :asset "Asset"
               :upgrade "Upgrade"
               :operation "Operation"
               :ice "Ice"
               :event "Event"
               :hardware "Hardware"
               :resource "Resource"
               :program "Program"}
   :pronouns {:none "Unspecified"
              :any "Any"
              :myodb "Prefer not to say"
              :blank "[blank]"
              :they "They/them"
              :she "She/her"
              :sheit "She/it"
              :shethey "She/they"
              :he "He/him"
              :heit "He/it"
              :hethey "He/they"
              :faefaer "Fae/Faer"
              :it "It"
              :ne "Ne/nem"
              :ve "Ve/ver"
              :ey "Ey/em"
              :zehir "Ze/hir"
              :zezir "Ze/zir"
              :xe "Xe/xem"
              :xi "Xi/xir"}
   :chat {:title "Play Netrunner in your browser"
          :channels "Channels"
          :send "Send"
          :placeholder "Say something..."
          :delete "Delete Message"
          :delete-all "Delete All Messages From User"
          :block "Block User"
          :cancel "Cancel"
          :message-blocked (fn [[reason-str]] (str "Message Blocked" (when reason-str (str ": " reason-str))))
          :length-exceeded "Length exceeded"
          :rate-exceeded "Rate exceeded"}
   :nav {:welcome "Welcome"
         :chat "Chat"
         :cards "Cards"
         :deck-builder "Deck Builder"
         :play "Play"
         :help "Help"
         :settings "Settings"
         :stats "Stats"
         :about "About"
         :tournaments "Tournaments"
         :admin "Admin"
         :users "Users"
         :features "Features"
         :game-count (fn [[cnt]] (str cnt " Game" (when (not= cnt 1) "s")))}
   :menu {:settings :en.nav/settings
          :logout "Jack out"
          :admin :en.nav/admin
          :moderator "Moderator"
          :donor "Donor"}
   :card-browser {:implementation-note "Implementation Note"
                  :search-hint "Search cards"
                  :sort "Sort by"
                  :format "Format"
                  :set "Set"
                  :side "Side"
                  :faction "Faction"
                  :type "Type"
                  :clear "Clear"
                  :select-art "Select Art"
                  :selected-art "Selected Alt Art"
                  :update-success "Updated Art"
                  :update-failure "Failed to Update Art"
                  :memory "Memory"
                  :cost "Cost"
                  :trash-cost "Trash cost"
                  :strength "Strength"
                  :advancement "Advancement requirement"
                  :agenda-points "Agenda points"
                  :min-deck "Minimum deck size"
                  :inf-limit "Influence Limit"
                  :influence "Influence"
                  :artist-info "Artist Info"
                  :more-info "More Info"
                  :sort-by {:faction "Faction"
                            :name "Name"
                            :type "Type"
                            :influence "Influence"
                            :cost "Cost"
                            :set-number "Set number"}}
   :deck-builder {:loading-msg "Loading deck collection..."
                  :new-corp "New Corp deck"
                  :new-runner "New Runner deck"
                  :import-button "Import deck"
                  :reset "Reset"
                  :import-title "Enter a Public NRDB Deck ID or URL"
                  :import "Import"
                  :cancel "Cancel"
                  :import-placeholder "NRDB ID"
                  :deck-count (fn [[cnt]] (str cnt (if (= 1 cnt) " Deck" " Decks")))
                  :filtered "(filtered)"
                  :save "Save"
                  :confirm-delete "Confirm Delete"
                  :edit "Edit"
                  :delete "Delete"
                  :copy "Copy"
                  :deck-copy-suffix "copy"
                  :clear-stats "Clear Stats"
                  :create-game "Create Game"
                  :new-deck "New Deck"
                  :deck-name "Deck name"
                  :format "Format"
                  :identity "Identity"
                  :deck-notes "Deck notes"
                  :decklist "Decklist"
                  :decklist-inst "(Type or paste a decklist, it will be parsed)"
                  :notes "Notes"
                  :add-to-deck "Add to deck"
                  :add-cards "Add cards"
                  :card-name "Card name"
                  :no-decks "No decks"
                  :cards "cards"
                  :min "minimum"
                  :max "maximum"
                  :influence "Influence"
                  :agenda-points "Agenda points"
                  :deck-points "Deck points"
                  :hash "Tournament hash"
                  :why "Why?"
                  :legal "legal"
                  :illegal "illegal"
                  :games "Games"
                  :completed "Completed"
                  :won "Won"
                  :lost "Lost"}
   :game-prompt {:trash "trash"
                 :advance "advance"
                 :score "score"
                 :rez "rez"
                 :derez "derez"
                 :expend "Expend"
                 :archives "Archives"
                 :hq "HQ"
                 :r-d "R&D"
                 :new-remote "New Remote"
                 :server-1 "Server 1"
                 :server-2 "Server 2"
                 :server-3 "Server 3"
                 :server-4 "Server 4"
                 :server-5 "Server 5"
                 :server-6 "Server 6"
                 :server-7 "Server 7"
                 :server-8 "Server 8"
                 :server-9 "Server 9"
                 :server-10 "Server 10"}
   :lobby {:no-games "No games"
           :tournament "Tournament"
           :competitive "Competitive"
           :casual "Casual"
           :angel-arena "Angel Arena"
           :new-game "New game"
           :reload "Reload list"
           :load-replay "Load replay"
           :start-replay "Start replay"
           :save-replay "Save replay"
           :create "Create"
           :cancel "Cancel"
           :title "Title"
           :side "Side"
           :format "Format"
           :default-game-format "Default game format"
           :gateway-format {:beginner "Beginner"
                            :beginner-info "This lobby is using the System Gateway beginner decks for the Corporation and Runner. These decks are recommended for your first games. Games are played to 6 agenda points."
                            :beginner-ul "System Gateway - Beginner Teaching Decks"
                            :intermediate "Intermediate"
                            :intermediate-info "This lobby is using the System Gateway intermediate decks for the Corporation and Runner. These decks have slightly more range than the beginner decks. Games are played to 7 agenda points."
                            :intermediate-ul "System Gateway - Intermediate Teaching Decks"
                            :constructed "Constructed"}
           :open-decklists "Open Decklists"
           :open-decklists-b "(open decklists)"
           :singleton "Singleton"
           :singleton-b "(singleton)"
           :singleton-details "This will restrict decklists to only those which do not contain any duplicate cards. It is recommended you use the listed singleton-based identities."
           :singleton-example "1) Nova Initiumia: Catalyst & Impetus 2) Ampere: Cybernetics For Anyone"
           :singleton-restriction "This lobby is running in singleton mode. This means decklists will be restricted to only those which do not contain any duplicate cards."
           :options "Options"
           :spectators "Allow spectators"
           :hidden "Make players' hidden information visible to spectators"
           :hidden-details "This will reveal both players' hidden information to ALL spectators of your game, including hand and face-down cards."
           :hidden-password "We recommend using a password to prevent strangers from spoiling the game."
           :password-protected "Password protected"
           :password "Password"
           :timed-game "Start with timer"
           :timer-length "Timer length (minutes)"
           :timed-game-details "Timer is only for convenience: the game will not stop when timer runs out."
           :save-replay-details "This will save a replay file of this match with open information (e.g. open cards in hand). The file is available only after the game is finished."
           :save-replay-unshared "Only your latest 15 unshared games will be kept, so make sure to either download or share the match afterwards."
           :save-replay-beta "BETA Functionality: Be aware that we might need to reset the saved replays, so make sure to download games you want to keep. Also, please keep in mind that we might need to do future changes to the site that might make replays incompatible."
           :replay-link-error "Replay link invalid."
           :replay-invalid-file "Select a valid replay file."
           :corp-perspective "Corp Perspective"
           :runner-perspective "Runner Perspective"
           :both-perspective "Both"
           :start "Start"
           :leave "Leave"
           :swap "Swap sides"
           :waiting "Waiting players deck selection"
           :players "Players"
           :deck-selected "Deck selected"
           :select-deck "Select Deck"
           :chat "Chat"
           :select-error "Cannot select that deck"
           :select-title "Select your deck"
           :spectator-count (fn [[cnt]] (str cnt " Spectator" (when (not= cnt 1) "s")))
           :closed-msg "Game lobby closed due to inactivity"
           :title-error "Please fill a game title."
           :password-error "Please fill a password."
           :too-little-data "Too little data"
           :completion-rate "Game Completion Rate"
           :watch "Watch"
           :join "Join"
           :rejoin "Rejoin"
           :as-corp "As Corp"
           :as-runner "As Runner"
           :private "PRIVATE"
           :reset "Reset Game Name"
           :delete "Delete Game"
           :password-for "Password for"
           :invalid-password "Invalid password"
           :not-allowed "Not allowed"
           :aborted "Connection aborted"
           :game-count (fn [[cnt]] (str cnt (if (= 1 cnt) " Game" " Games")))
           :api-access "Allow API access to game information"
           :api-requires-key "(Requires an API Key in Settings)"
           :api-access-details "This allows access to information about your game to 3rd party extensions. Requires an API Key to be created in Settings."
           :filter "Filter"
           :filtered "(filtered)"}
   :angel-arena {:cancel-match "Cancel match"
                 :claim-victory "Claim victory"
                 :still-here "Need more time"
                 :abandon-run "Abandon run"
                 :active-corp-run "Active Corp run"
                 :active-runner-run "Active Runner run"
                 :are-you-sure "Are you sure?"
                 :are-you-sure-no "no"
                 :are-you-sure-yes "yes"
                 :format "Format"
                 :latest-runs "Latest runs"
                 :no-eligible-decks "No legal decks found for this side and format."
                 :no-games "No games"
                 :queue-for-match "Queue for match"
                 :queueing "Queueing..."
                 :requesting-run-data "Requesting run data..."
                 :select-deck "Select your deck"
                 :start-new-run "Start new run"
                 :wins "wins"}
   :settings {:invalid-password "Invalid login or password"
              :invalid-email "No account with that email address exists"
              :updated "Profile updated - Please refresh your browser"
              :updating "Updating profile..."
              :get-log-width "Get current log width"
              :get-log-top "Get current log top"
              :email-title "Change email address"
              :current-email "Current email"
              :desired-email "Desired email"
              :email-placeholder "Email address"
              :enter-valid "Please enter a valid email address"
              :update "Update"
              :cancel "Cancel"
              :email "Email"
              :change-email "Change email"
              :avatar "Avatar"
              :change-avatar "Change on gravatar.com"
              :pronouns "Pronouns"
              :language "Language"
              :sounds "Sounds"
              :enable-lobby-sounds "Enable lobby sounds"
              :enable-game-sounds "Enable game sounds"
              :volume "Volume"
              :layout-options "Layout options"
              :player-stats-icons "Use icons for player stats"
              :stacked-cards "Card stacking (on by default)"
              :ghost-trojans "Display ghosts for hosted programs"
              :sides-overlap "Runner and Corp board may overlap"
              :runner-layout "Runner layout from Corp perspective"
              :runner-classic "Runner rig layout is classic jnet (Top to bottom: Programs, Hardware, Resources)"
              :runner-reverse "Runner rig layout is reversed (Top to bottom: Resources, Hardware, Programs)"
              :log-size "Log size"
              :log-player-highlight "Log player highlight"
              :log-player-highlight-red-blue "Corp: Blue / Runner: Red"
              :log-player-highlight-none "None"
              :card-preview-zoom "Card preview zoom"
              :card-iamge "Card Image"
              :card-text "Card Text"
              :pin-zoom "Keep zoomed cards on screen"
              :background "Game board background"
              :the-root-bg "The Root"
              :freelancer-bg "Freelancer"
              :mushin-no-shin-bg "Mushin No Shin"
              :traffic-jam-bg "Traffic Jam"
              :rumor-mill-bg "Rumor Mill"
              :find-the-truth-bg "Find The Truth"
              :push-your-luck-bg "Push Your Luck"
              :apex-bg "Apex"
              :worlds2020-bg "Worlds 2020"
              :monochrome-bg "Monochrome"
              :custom-bg "Custom BG"
              :input-url-below "(input URL below)"
              :card-backs "Card backs"
              :game-stats "Game Win/Lose statistics"
              :deck-stats "Deck statistics"
              :always "Always"
              :comp-only "Competitive Lobby Only"
              :none "None"
              :alt-art "Alt arts"
              :show-alt "Show alternate card arts"
              :high-res "Enable high-resolution card images"
              :card-images "Card images"
              :set-all "Set all cards to"
              :set "Set"
              :reset "Reset All to Official Art"
              :blocked "Blocked users"
              :user-name "User name"
              :block "Block user"
              :update-profile "Update Profile"
              :nsg "NSG"
              :ffg "FFG"
              :api-keys "API Keys"
              :delete-api-key "Delete"
              :create-api-key "Create API Key"
              :bespoke-sounds-header "Card-Specific Sounds"
              :bespoke-sounds
              {:archer "Archer"
               :harmonics "Harmonics Suite (Bloop, Echo, Pulse, Wave)"
               :end-of-the-line "End of the Line"}}
   :ingame-settings {:card-stacking "Card settings"
                     :stack-cards "Stack cards"
                     :label-unrezzed-cards "Label unrezzed cards"
                     :label-faceup-cards "Label face up cards"
                     :ghost-trojans "Display hosted trojans in rig"
                     :card-sorting "Sorting"
                     :sort-archives "Sort Archives"
                     :sort-heap "Sort Heap"
                     :runner-board-order "Runner board order"
                     :runner-classic "Classic"
                     :runner-reverse "Reversed"
                     :board-overlap "Board overlap"
                     :sides-overlap "Runner and Corp may overlap"
                     :card-backs "Card backs"
                     :preview-zoom "Card preview zoom"
                     :card-image "Card Image"
                     :card-text "Card Text"
                     :card-images "Card images"
                     :high-res "Enable high resolution card images"
                     :alt-art "Alt arts"
                     :show-alt "Show alternate card arts"
                     :save "Save"}
   :stats {:game-stats "Game Stats"
           :corp-stats "Corp Stats"
           :runner-stats "Runner Stats"
           :clear-stats "Clear Stats"
           :no-log "No log available"
           :view-log "View log"
           :winner "Winner"
           :no-games "No games"
           :all-games "Show all games"
           :shared-games "Only show shared"
           :started "Started"
           :ended "Ended"
           :completed "Completed"
           :not-completed "Not completed"
           :won "Won"
           :lost "Lost"
           :turn-count (fn [[cnt]] (str cnt " turn" (when (not= cnt 1) "s")))
           :lobby "Lobby"
           :format "Format"
           :win-method "Win method"
           :view-games "Return to stats screen"
           :share "Share replay"
           :launch "Launch Replay"
           :download "Download replay"
           :unavailable "Replay unavailable"
           :filtered "(filtered)"
           :log-count (fn [[cnt]] (str cnt " Log" (when (not= cnt 1) "s")))
           :clicks-gained "Clicks Gained"
           :credits-gained "Credits Gained"
           :credits-spent "Credits Spent"
           :credits-click "Credits by the Basic Action"
           :cards-drawn "Cards Drawn"
           :cards-click "Cards Drawn by the Basic Action"
           :damage-done "Damage Done"
           :cards-rezzed "Cards Rezzed"
           :tags-gained "Tags Gained"
           :runs-made "Runs Made"
           :shuffle-count "Shuffle Count"
           :operations-played "Operations Played"
           :cards-sabotaged "Sabotage Count"
           :events-played "Events Played"
           :unique-accesses "Unique Cards Accessed"
           :psi-game-total "Psi Game: Games Played"
           :psi-game-total-wins "Psi Game: Wins"
           :psi-game-total-bid-0 "Psi Game: Bid 0"
           :psi-game-total-bid-1 "Psi Game: Bid 1"
           :psi-game-total-bid-2 "Psi Game: Bid 2"
           :rashida-count "Rashida Count"
           :cards-accessed "Cards Accessed"}
   :log {:game-log "Game Log"
         :settings "Settings"
         :annotating "Annotating"
         :run-timing "Run Timing"
         :turn-timing "Turn Timing"
         :shared "Shared Annotations"
         :remote-annotations-fail "Could not get remote annotations."}
   :annotations {:turn-placeholder "Notes for this turn"
                 :click-placeholder "Notes for this click"
                 :available-annotations "Available annotations"
                 :no-published-annotations "No published annotations."
                 :import-local "Import local annotation file"
                 :load-local "Load"
                 :save-local "Save"
                 :publish "Publish"
                 :clear "Clear local annotations"}
   :diagrams {:turn {:corp-turn "Corporation Turn"
                     :corp-draw-phase "5.6.1: Draw Phase"
                     :corp-draw-phase-a "Corporation gains allotted clicks (default: [click][click][click])"
                     :corp-draw-phase-b "Paid ability window. Corp may rez non-ice cards or score agendas during this window"
                     :corp-draw-phase-c "Corporation recurring credits refill"
                     :corp-draw-phase-d "The turn formally begins. Turn begins events resolve"
                     :corp-draw-phase-e "The corporation performs their mandatory draw"
                     :corp-draw-phase-f "Proceed to the action phase (5.6.2)"
                     :corp-action-phase "5.6.2: Action Phase"
                     :corp-action-phase-a "Paid ability window. Corp may rez non-ice cards or score agendas during this window"
                     :corp-action-phase-b "If the corporation has unspent [Clicks], they take an action"
                     :corp-action-phase-c "If an action occured, return to (a)"
                     :corp-action-phase-d "The action phase is complete. Proceed to the discard phase (5.6.3)"
                     :corp-discard-phase "5.6.3: Discard phase"
                     :corp-discard-phase-a "The corporation discards to maximum hand size, if applicable"
                     :corp-discard-phase-b "Paid ability window. Corp may rez non-ice cards during this window"
                     :corp-discard-phase-c "If the corporation has any [Clicks] remaining, they lose those [Clicks]"
                     :corp-discard-phase-d "The Corporations turn formally ends. Turn end triggers resolve"
                     :corp-discard-phase-e "Proceed to the Runner turn"
                     :runner-turn "Runner Turn"
                     :runner-action-phase "5.7.1: Action Phase"
                     :runner-action-phase-a "Runner gains allotted clicks (default: [click][click][click][click])"
                     :runner-action-phase-b "Paid ability window. Corp may rez non-ice cards"
                     :runner-action-phase-c "Runner recurring credits refill"
                     :runner-action-phase-d "The turn formally begins. Turn begins events resolve"
                     :runner-action-phase-e "Paid ability window. Corp may rez non-ice cards"
                     :runner-action-phase-f "If the Runner has unspent [Clicks], they take an action"
                     :runner-action-phase-g "If an action occured, return to (e)"
                     :runner-action-phase-h "The action phase is complete. Proceed to the discard phase (5.7.2)"
                     :runner-discard-phase "5.7.2: Discard Phase"
                     :runner-discard-phase-a "The runner discards to maximum handsize, if applicable"
                     :runner-discard-phase-b "Paid ability window. Corp may rez non-ice cards"
                     :runner-discard-phase-c "If the runner has any [Clicks] remaining, they lose those [Clicks]"
                     :runner-discard-phase-d "The Runners turn formally ends. Turn end triggers resolve"
                     :runner-discard-phase-e "Proceed to the Corporation turn"}
              :run-timing {:header "Timing Structure of a Run"
                           :disclaimer "This structure has been simplified for clarity. For complete rules, see the Null Signal Games website."
                           :initiation "6.9.1: Initiation Phase"
                           :initiation-a "Runner declares a server"
                           :initiation-b "Runner gains Bad Publicity credits"
                           :initiation-c "Run formally begins - Run events fire"
                           :initiation-d "Proceed to the outermost ice, if applicable, and begin the approach phase (6.9.2)"
                           :initiation-e "Otherwise, proceed to the movement phase (6.9.4)"
                           :approach "6.9.2: Approach Ice Phase"
                           :approach-a "You are now approaching the ice. Approach events resolve"
                           :approach-b "Paid Ability Window. Corp may rez the approached ice, or non-ice cards, during this window"
                           :approach-c "If approached ice is rezzed, continue to encounter phase (6.9.3)"
                           :approach-d "Otherwise, proceed to the movement phase (6.9.4)"
                           :encounter "6.9.3: Encounter Ice Phase"
                           :encounter-a "You are now encountering this ice. Encounter events resolve"
                           :encounter-b "Paid ability window. Encountered ice may be interfaced during this window"
                           :encounter-c "If there are unbroken subroutines to resolve, the corporation resolves the topmost unbroken subroutine. If they do, repeat this step"
                           :encounter-d "The encounter is complete. Proceed to the movement phase (6.9.4)"
                           :movement "6.9.4: Movement Phase"
                           :movement-a "If you were encountering or approaching an ice, you pass it. Pass-Ice events resolve"
                           :movement-b "If there are no more ice inwards from the passed ice, 'when you pass all ice on the server' events resolve"
                           :movement-c "Paid ability window"
                           :movement-d "The runner may jack out. If they do, proceed to the run ends phase (6.9.6)"
                           :movement-e "The runner proceeds to the next position inwards, if applicable"
                           :movement-f "Paid ability window. The corporation may rez non-ice cards"
                           :movement-g "If you are approaching another ice, return to the approach ice phase (6.9.2)"
                           :movement-h "The runner approaches the attacked server. Approach events resolve"
                           :movement-i "Continue to the success phase (6.9.5)"
                           :success "6.9.5: Success Phase"
                           :success-a "The run is declared successful. Successful run events are met"
                           :success-b "The runner breaches the attacked server"
                           :success-c "The success phase is complete. Continue to the run ends phase (6.9.6)"
                           :run-ends "6.9.6: Run Ends Phase"
                           :run-ends-a "Any open priority windows complete or are closed"
                           :run-ends-b "The runner loses any unspent bad publicity credits"
                           :run-ends-c "If the success phase was not reached and the server still exists, the run becomes unsuccessful"
                           :run-ends-d "The run ends. Run ends events resolve"}}
   :game {:ok "OK"
          :error "Internal Server Error. Please type /bug in the chat and follow the instructions."
          :set-aside "Set aside"
          :keep "Keep"
          :mulligan "Mulligan"
          :close "Close"
          :start "Start Game"
          :remove-tag "Remove Tag"
          :run "Run"
          :purge "Purge"
          :trash-resource "Trash Resource"
          :draw "Draw"
          :gain-credit "Gain Credit"
          :game-start "Game start"
          :start-turn "Start Turn"
          :end-turn "End Turn"
          :mandatory-draw "Mandatory Draw"
          :take-clicks "Take Clicks"
          :rez-all "Rez All"
          :reveal-my-hand "Reveal My Hand"
          :hq "HQ"
          :grip "Grip"
          :rfg "Removed from the game"
          :play-area "Play Area"
          :current "Current"
          :scored-area "Scored Area"
          :last-revealed "Last Revealed"
          :archives "Archives"
          :max-hand "Max hand size"
          :brain-damage "Core Damage"
          :tag-count (fn [[base additional total]] (str base (when (pos? additional) (str " + " additional)) " Tag" (if (not= total 1) "s" "")))
          :agenda-count (fn [[agenda-point]] (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s")))
          :agenda-point-req (fn [[agenda-point-req]] (if-not (= 7 agenda-point-req) (str " (" agenda-point-req " required)") ""))
          :link-strength "Link Strength"
          :credit-count (fn [[credit run-credit]] (str credit " Credit" (if (not= credit 1) "s" "") (when (pos? run-credit) (str " (" run-credit " for run)"))))
          :click-count (fn [[click]] (str click " Click" (if (not= click 1) "s" "")))
          :bad-pub-count (fn [[base additional]] (str base (when (pos? additional) (str " + " additional)) " Bad Publicity"))
          :mu-count (fn [[unused available]] (str unused " of " available " MU unused"))
          :special-mu-count (fn [[unused available mu-type]] (str unused " of " available " " mu-type " MU unused"))
          :show-decklists "Show/Hide decklists"
          :indicate-action "Indicate paid ability"
          :spec-count (fn [[c]] (str c " Spectator" (when (> c 1) "s")))
          :spec-view "Spectator View"
          :runner-view "Runner View"
          :corp-view "Corp View"
          :leave-replay "Leave Replay"
          :leave "Leave Game"
          :unmute "Unmute spectators"
          :mute "Mute spectators"
          :attempt-reconnect "Attempt reconnect"
          :lost-connection, "Lost connection to server. Reconnecting."
          :reconnected-to-server, "Reconnected to server"
          :concede "Concede"
          :inactivity "Game closed due to inactivity"
          :server "Server"
          :unimplemented "Unimplemented"
          :abilities "Abilities"
          :subs "Subroutines"
          :let-subs-fire "Let unbroken subroutines fire"
          :fire-unbroken "Fire unbroken subroutines"
          :actions "Actions"
          :stack "Stack"
          :r&d "R&D"
          :shuffle "Shuffle"
          :show "Show"
          :close-shuffle "Close & Shuffle"
          :heap "Heap"
          :card-count (fn [[size]] (str size " card" (when (not= 1 size) "s") "."))
          :face-down-count (fn [[total face-up]] (str total " cards, " (- total face-up) " face-down."))
          :up-down-count (fn [[total face-up]] (str face-up "↑ " (- total face-up) "↓"))
          :initiation "Initiation"
          :approach-ice "Approach ice"
          :encounter-ice "Encounter ice"
          :movement "Movement"
          :breach-server "Breach server"
          :success "Success"
          :run-ends "Run ends"
          :no-current-run "No current run"
          :current-phase "Current phase"
          :unknown-phase "Unknown phase"
          :rez "Rez"
          :no-further "No further actions"
          :continue "Continue"
          :continue-to "Continue to"
          :stop-auto-pass "Stop auto-passing priority"
          :auto-pass "Auto-pass priority"
          :jack-out "Jack Out"
          :trace "Trace"
          :beat-trace "Beat Trace"
          :unbeatable "Make unbeatable"
          :credits "credits"
          :card "Card"
          :minutes "m:"
          :seconds "s"
          :seconds-remaining "s remaining"
          :time-taken (fn [[t]] (str "Time taken: " t " minutes"))
          :win-decked (fn [[turn]] (str "wins due to the Corp being decked on turn " turn))
          :win-flatlined (fn [[turn]] (str "wins by flatline on turn " turn))
          :win-conceded (fn [[turn]] (str "wins by concession on turn " turn))
          :win-claimed (fn [[turn]] (str "wins by claim on turn " turn))
          :win-points (fn [[turn]] (str "wins by scoring agenda points on turn " turn))
          :win-other (fn [[turn reason]] (str "wins by " reason " on turn " turn))}})

(defn pluralize
  "Makes a string plural based on the number n. Takes specific suffixes for singular and plural cases if necessary."
  ([string n] (pluralize string "s" n))
  ([string suffix n] (pluralize string "" suffix n))
  ([string single-suffix plural-suffix n]
   (if (or (= 1 n)
           (= -1 n))
     (str string single-suffix)
     (str string plural-suffix))))

(defn quantify
  "Ensures the string is correctly pluralized based on the number n."
  ([n string] (str n " " (pluralize string n)))
  ([n string suffix] (str n " " (pluralize string suffix n)))
  ([n string single-suffix plural-suffix]
   (str n " " (pluralize string single-suffix plural-suffix n))))

(defn enumerate-str
  "Joins a collection to a string, seperated by commas and 'and' in front of
  the last item. If collection only has one item, justs returns that item
  without seperators. Returns an empty string if coll is empty."
  [strings]
  (if (<= (count strings) 2)
    (join " and " strings)
    (str (apply str (interpose ", " (butlast strings))) ", and " (last strings))))

(defn build-spend-msg-suffix
  "Constructs the spend message  suffix for specified cost-str and verb(s)."
  ([cost-str verb] (build-spend-msg-suffix cost-str verb nil))
  ([cost-str verb verb2]
   (if (empty? cost-str)
     (str (or verb2 (str verb "s")) " ")
     (str verb " "))))

(defn render-credits
  [value]
  (if (map? value)
    (let [remainder-str (when-let [remainder (:pool value)]
                          (str remainder " [Credits]"))
          card-strs (when-let [cards (:cards value)]
                      (str (enumerate-str (map #(str (second %) " [Credits] from " (first %))
                                               cards))))
          message (str "pays "
                       card-strs
                       (when (and card-strs remainder-str)
                         " and ")
                       remainder-str
                       (when (and card-strs remainder-str)
                         " from [their] credit pool"))]
      message)
    (str "pays " value " [Credits]")))

;; okay so what should be coming in here is the server-side format, which is
;;   [<location> <server> <location>]
;; where
;; - location: :servers, :deck, :hand, :discard
;; - server: :hq :rd :archives :remoteN
;; - location: :content :ices
(defn to-zone-name
  ([zone] (to-zone-name zone :corp))
  ([[location server placement] side]
   (let [location (keyword location)
         server (keyword server)
         side (keyword side)]
     (case location
       :hand (if (= side :corp) "HQ" "the Grip")
       :deck (if (= side :corp) "R&D" "the Stack")
       :discard (if (= side :corp) "Archives" "the Heap")
       :servers
       (case server
         :hq (if (= side :corp) "HQ" "the Grip")
         :rd (if (= side :corp) "R&D" "the Stack")
         :archives (if (= side :corp) "Archives" "the Heap")
         (str "Server " (last (split (str server) #":remote"))))
       (str "unhandled server " location)))))

;; TODO fix it, jp is more comprehensive
;; TODO need 'root' wording here
(defn- render-card-internal
  [{:keys [card card-type server pos hosted]}]
  (str (if-not (empty? card)
         card
         (case (keyword card-type)
           :facedown "facedown card"
           :ice "ice"
           :card "a card"
           ""))
       (if hosted
         (str " hosted on " (render-card-internal hosted))
         (when server
           (if (not (nil? pos))
             (str " protecting "(to-zone-name server) " at position " pos)
             ;; so for better wording this is probably "from" for a non-root?
             ;; TODO need to confirm actual behavior today...
             (str " in "
                  (to-zone-name server)))))))

(defn- render-card
  [card]
  (if (string? card) card (render-card-internal card)))

;; (render-card-list value "removes" "installed program" " from the game")
;; -> removes COUNT installed program(s) from the game (VAL1, VAL2, ...)
(defn- render-card-list
  ([cards action qualifier] (render-card-list cards action qualifier "" ""))
  ([cards action qualifier trailer] (render-card-list cards action qualifier trailer ""))
  ([cards action qualifier trailer suffix]
   (str action " " (quantify (count cards) qualifier) trailer
        " (" (enumerate-str (map #(if (string? %) % (render-card %)) cards)) ")" suffix)))

(defn- to-duration
  [duration]
  (case (keyword duration)
    :end-of-run " for the remainder of the run"
    :end-of-turn " for the remainder of the turn"
    ""))

(defn- to-counter
  [counter]
  (case (keyword counter)
    :adv "advancement counter"
    :virus "virus counter"
    :power "power counter"
    ;; might do place-credits instead, or split this out
    :credit "[Credit]"
    ;; TODO stopgap
    :credits "[Credit]"))

;; TODO dumb name for now, i'm not sure if should be separate or unified
;; so this takes a list of titles and :unseen
(defn- render-card2
  [cards]
  (let [unseen (count (filter #(= "unseen" %) cards))
        self (some nil? cards)
        seen (remove nil? (filter #(not (= "unseen" %)) cards))]
    ;; TODO This is currently placing seen cards after the rest which is a bit unnatural
    (enumerate-str (remove nil? (conj seen
                                      (when (pos? unseen) (quantify unseen "unseen card"))
                                      (when self "itself"))))))

(defn- render-single-cost
  [cost value side]
  (let [hand (to-zone-name [:hand] (or side :corp))
        deck (to-zone-name [:deck] (or side :corp))]
    (case cost
      :click (str "spends " (apply str (repeat value "[Click]")))
      :lose-click (str "loses " (apply str (repeat value "[Click]")))
      :credits (render-credits value)
      :trash (str "trashes " value)
      ;; broken, in this case it's a list but printed as single
      ;; {:username "Runner", :type :install, :cost {:forfeit ("Vanity Project"), :credits 1}, :card "Chatterjee University", :origin [:deck], :raw-text nil}
      :forfeit (if (string? value)
                 (str "forfeits " value)
                 (str "forfeits " (quantify (count value) "agenda")
                      " (" (enumerate-str value) ")"))
      :gain-tag (str "takes " (quantify value "tag"))
      :tag (str "removes " (quantify value "tag"))
      :bad-pub (str "gains " value " bad publicity")
      :return-to-hand (str "returns " value " to " hand)
      :remove-from-game (str "removes " value " from the game")
      :rfg-program (render-card-list value "removes" "installed program" " from the game")
      :trash-installed (render-card-list value "trashes" "installed card")
      :hardware (render-card-list value "trashes" "installed piece" " of hardware")
      :derez (render-card-list value "derezzes" "card")
      :program (render-card-list value "trashes" "installed program")
      :resource (render-card-list value "trashes" "installed resource")
      :connection (render-card-list value "trashes" "installed connection")
      ;; TODO this renders it as 'ices'
      :ice (render-card-list value "trashes" "installed rezzed ice")
      :trash-from-deck (str "trashes " (quantify value "card") " from the top of " deck)
      :trash-from-hand (if (int? value)
                         (str "trashes " (quantify value "card") " from " hand)
                         (render-card-list value "trashes" "card" "" (str " from " hand)))
      :randomly-trash-from-hand (str "trashes " (quantify value "card") " randomly from " hand)
      :trash-entire-hand (if (int? value)
                           (str "trashes all (" value ") cards in " hand)
                           (str "trashes all (" (count value) ") cards in " hand " (" (enumerate-str value) ")"))
      :trash-hardware-from-hand (render-card-list value "trashes" "piece" " of hardware" (str " from " hand))
      :trash-program-from-hand (render-card-list value "trashes" "program" "" (str " from " hand))
      :trash-resource-from-hand (render-card-list value "trashes" "resource" "" (str " from " hand))
      :take-net (str "suffers " value " net damage")
      :take-meat (str "suffers " value " meat damage")
      :take-core (str "suffers " value " core damage")
      :shuffle-installed-to-stack (render-card-list value "shuffles" "card" "" (str " into " deck))
      :add-installed-to-bottom-of-deck (render-card-list value "adds" "installed card" "" (str " to the bottom of " deck))
      ;; TODO not sure if this makes sense. should be number and never revealed?
      :add-random-from-hand-to-bottom-of-deck (str "adds " (quantify (count value) "random card") (str " from " hand " to the bottom of " deck))
      :agenda-counter (str "spends " (quantify (second value) "hosted agenda counter") " from on " (first value))
      ;; TODO is this a list?
      ;; yes, there's a path where this is a list of title-counts...
      ;; might need a generic mechanism
      :virus (let [[host count] value]
               (str "spends " (quantify count "hosted virus counter") " from on " host))
      :advancement (str "spends " (quantify (second value) "hosted advancement counter") " from on " (first value))
      :power (str "spends " (quantify (second value) "hosted power counter") " from on " (first value)))))

(defn render-cost
  [cost side]
  (when cost
    (enumerate-str (for [[c v] cost] (render-single-cost c v side)))))

(defn render-cost-str
  [{:keys [cost side]}]
  (when-not (empty? cost)
    (str (render-cost cost side)
         ;; TODO remove in order to do cost test
         ;;" to "
         " to "
         )))

;; ? {:username Runner, :type :use, :cost {:click 1}, :effect {:make-run HQ}, :card Red Team, :forced false, :raw-text nil}
;; ? Runner spends [Click] to use Red Team to  to make a run on unknown server HQ.
(defn- render-single-effect
  [effect value]
  (when-not (and (number? value) (zero? value))
    (case effect
      :advance (str "advance " (render-card value))
      :draw-cards (str "draw " (quantify value "card"))
      :gain-credits (str "gain " value " [Credits]")
      :gain-click (str "gain " (apply str (repeat value "[Click]")))
      :lose-click (str "lose " (apply str (repeat value "[Click]")))
      :lose-credits (str "force the Runner to lose " value " [Credits]")
      :give-tag (str "give the Runner " (quantify value "tag"))
      :take-tag (str "take " (quantify value "tag"))
      :remove-tag (str "remove " (quantify value "tag"))
      :take-bp (str "take " value " bad publicity")
      ;; TODO this is a clusterfuck, figure out how to unify it later
      :add-from-stack (str "add " value " from the stack to the grip and shuffle the stack")
      :add-from-rnd (str  "reveal " value " from R&D and add it to HQ")
      :add-to-hq (str "add " (render-card value) " to HQ")
      ;; TODO not sure if this should be string or rendered card
      :add-to-grip (str "add " (render-card value) " to the Grip")
      :add-to-hq-unseen (str "add " (quantify value "card") " to HQ")
      ;; TODO making this add would be more consistent? Working Prototype uses "add", ??? uses "move"
      :move-to-top-stack (str "move " value " to the top of the stack")
      :shuffle-rnd (str "shuffle R&D")
      :reveal-and-add (let [[card from to] value]
                        (str "add " card " from " (to-zone-name from) " to " (to-zone-name to)))
      :reveal-from-hq (str "reveal " (enumerate-str value) " from HQ")
      :make-run (str "make a run on " (to-zone-name value))
      :end-run "end the run"
      ;; TODO probably need a duration here, others are encounter-only IIRC
      :gain-type (let [[card type] value] (str "make " card " gain " type " until the end of the run"))
      :place-counter (let [[type count target] value]
                       (str "place "
                            (if (or (= (keyword type) :credit) (= (keyword type) :credits))
                              (str count " [Credits]")
                              (quantify count (to-counter type)))
                            " on "
                            (if target (render-card target) "itself")))
      :remove-counter (let [[type count target] value]
                       (str "remove "
                            (quantify count (to-counter type))
                            " from "
                            (if target (render-card target) "itself")))
      :move-counter (let [[type count source target] value]
                      (str "move "
                           (quantify count (to-counter type))
                           " from "
                           (if source (render-card source) "itself")
                           " to "
                           (if target (render-card target) "itself")))
      ;; TODO need to fix hq/blah
      :trash-from-hand (if (int? value)
                         (str "trashes " (quantify value "card") " from " "HQ")
                         (render-card-list value "trashes" "card" "" (str " from " "HQ")))
      :add-str (let [[card count] value] (str "add " count " strength to " card))
      :reduce-str (let [[card count] value] (str "give -" count " strength to " (render-card card) " for the remainder of the encounter"))
      ;; TODO combine hq/rnd?
      :access-additional-from-hq (str "access " (quantify value "additional card") " from HQ")
      :access-additional-from-rnd (str "access " (quantify value "additional card") " from R&D")
      ;; TODO similar, combine?
      :deal-net (str "deal " value " net damage")
      :deal-meat (str "deal " value " meat damage")
      :deal-core (str "deal " value " core damage")
      :install (str "install " value)
      :rez (str "rez " value)
      :install-and-rez-free (str "install and rez " value ", ignoring all costs")
      :host (str "host " (render-card value))
      :bypass (str "bypass " (render-card value))
      :trash-free (str "trash " value " at no cost")
      :str-pump (let [[base-str target-str duration] value]
                  (str "increase its strength from " base-str " to " target-str (to-duration duration)))
      :lower-ice-str (let [[strength card] value]
                       (str "lower the strength of "
                            (or card "each installed icebreaker")
                            " by " strength))
      :shuffle-into-rnd (str "shuffle " (render-card2 value) " into R&D")
      :rearrange-rnd (str "rearrange the top " (quantify value "card") " of R&D")
      :reveal-from-rnd (str "reveal " (quantify value "card") " from the top of R&D")
      :look-top-rnd (str "look at the top " (quantify value "card") " of R&D")
      :move-hq-rnd (str "add " (quantify value "card") " from HQ to to the top of R&D")
      :play (str "play " value)
      :move-server (let [[server card] value]
                     (str "move " (or card "itself") " to " (to-zone-name server)))
      :prevent-access (let [[type card] value]
                        (str "prevent the runner from accessing "
                             (case (keyword type)
                               :target card
                               :exclusive (str "cards other than " card))))
      :trash-stack (str "trash " (enumerate-str value) " from the top of the stack")
      :prevent-net (str "prevent " value " net damage")
      :prevent-encounter-ability (let [[card ability] value]
                                   (str "prevent the encounter ability on " card (when ability (str " (" ability ")"))))
      :prevent-etr (str "prevent " (render-card value) " from ending the run this encounter")
      ;; TODO different duration when supported
      :gain-str (str "gain " value " strength for the remainder of the turn")
      :breach-server (str "breach " (to-zone-name value))
      :derez (str "derez "
                  (if (coll? value)
                    (enumerate-str (map render-card value))
                    (render-card value)))
      :rez-free (str "rez " (enumerate-str value) ", ignoring all costs")
      :encounter-ice (str "make the Runner encounter " (render-card value))
      :reveal-self (str "reveal itself from " (to-zone-name value))
      :add-from-hq-to-score (str "add " value " from HQ to [their] score area")
      :turn-faceup (str "turn " value " in Archives faceup")
      :add-self-to-hq (str "add itself to HQ")
      :trash (str "trash " (render-card value))
      ;; TODO this needs a duration?
      :add-str-new (let [[card count] value] (str "give " (render-card card) " +" count " strength"))
      ;; TODO could spruce this up but it follows current thunderbolt format
      :add-sub (str "add " value " after its other subroutines")
      :trash-rnd (str "trash the top " (quantify value "card") " of R&D")
      :remove-click-next-turn (str "give the Runner -" value " allotted [Click] for [their] next turn")
      :move-grip-to-stack (str "add " (enumerate-str value) " from the Grip to the top of the Stack")
      :shuffle-into-stack (str "shuffle " value " into the stack")
      :remove-all-virus-counters (str "remove all virus counters from " (render-card value))
      :trash-from-hq (str "trash " value " from HQ")
      :reveal-from-grip (str "reveal " (enumerate-str value) " from the Grip")
      :add-to-top-rnd (str "add " value " to the top of R&D")
      :add-to-bottom-rnd (str "add " value " to the bottom of R&D")
      :force-reveal (str "reveal " (quantify value "random card") " from HQ")
      :shuffle-zone-into (str "shuffle " (enumerate-str (map to-zone-name value)) " into " (to-zone-name [:deck]))
      :rfg (str "remove " (enumerate-str value) " from the game")
      :reveal-from-stack (str "reveal " (enumerate-str value) " from the top of the stack")
      :host-on-self (str "host " value " on itself")
      :host-instead-of-access (str "host " value " on itself instead of accessing it")
      :shuffle-stack (str "shuffle the stack")
      :trash-self (str "trash itself")
      :credits (str "pay " value " [Credits]")
      :draw-additional (str "draw " (quantify value "additional card"))
      :purge "purge virus counters"
      ;; TODO
      :swap-ice (throw "foo"))))

;; TODO this keyword logic is just silly
(defn render-single-effect-force-check
  [effect value side]
  (let [effect (name effect)]
    (if (ends-with? effect "-force")
      (str "force the "
           ;; This is inverted -- corp forcing effect means it's forcing runner to take the effect.
           (if (= (keyword side) :corp) "Runner" "Corp")
           " to "
           ;; oh god
           (render-single-effect (keyword (subs effect 0 (- (count effect) (count "-force")))) value))
      (render-single-effect (keyword effect) value))))

(defn render-effect
  [effects side]
  (when effects
    (enumerate-str (remove nil? (for [[c v] effects]
                                  (render-single-effect-force-check c v side)
                                  #_(render-single-effect c v))))))

(defn render-effect-str
  [{:keys [effect side]}]
  (when-not (empty? effect)
    (str " to " (render-effect effect side))))

(defmulti render-text (fn [input] (or (keyword (:type input)) :raw-text)))

(defmethod render-text :create-game [_] "has created the game")
(defmethod render-text :keep-hand [_] "keeps [their] hand")
(defmethod render-text :mulligan-hand [_] "takes a mulligan")
(defmethod render-text :mandatory-draw [_] "makes [their] mandatory start of turn draw")

(defmethod render-text :no-action [_] "has no further action")

(defmethod render-text :turn-state
  [input]
  (let [state (:state input)
        pre (if (= (:phase state) "start-turn") "started" "is ending")
        turn (:turn state)
        credits (:credits state)
        cards (:cards state)
        hand (if (= (:side state) "runner") "[their] Grip" "HQ")]
    (str pre " [their] turn " turn " with " credits " [Credit] and " (quantify cards "card") " in " hand)))

(defmethod render-text :play
  [{:keys [card cost]}]
  (let [cost-spend-msg (build-spend-msg-suffix cost "play")]
    (str cost-spend-msg card)))

;; TODO discount-str: ignore-all-costs, ignore-install-costs, cost-bonus
;; cost-bonus should in theory be used for DZMZ but i don't see it in the map
(defmethod render-text :install
  [{:keys [card card-type server new-remote origin install-source cost host side]}]
  (let [card-type (keyword card-type)]
    (str (if install-source
           (str (build-spend-msg-suffix cost "use") install-source " to install ")
           ;; TODO fix
           (build-spend-msg-suffix cost "install"))
         (if (= card-type :ice)
           (str (or card "ice"))
           (str (or card (if (= card-type :facedown)
                           "an unseen card"
                           "a card"))))
         (when origin
           (str " from " (to-zone-name origin side)))
         (when server
           (str (if (= card-type :ice)
                  " protecting "
                  " in the root of ")
                (to-zone-name server)
                (when new-remote " (new remote)")))
         (when host
           (str " on " (render-card host))))))

(defmethod render-text :rez
  [{:keys [card alternative-cost ignore-cost cost]}]
  (str (if cost "rez " "rezzes ")
       (if (string? card) card (render-card card))
       (if alternative-cost " by paying its alternative cost"
           ;; shouldn't this be ", ignoring all costs" ?
           (when ignore-cost " at no cost"))))

(defmethod render-text :use
  [{:keys [card cost effect]}]
  (let [cost-spend-msg (build-spend-msg-suffix cost "use")]
    ;; TODO this is a stopgap until everything is covnerted to effects
    ;; if there's no effect, it's assumed this is followed by raw text
    (str cost-spend-msg card (when-not effect " to "))))

(defmethod render-text :advance
  [input]
  (str "advance " (render-card input)))

(defmethod render-text :score
  [{:keys [card points]}]
  (str "scores " card " and gains " points " agenda points"))

(defmethod render-text :steal
  [{:keys [card points]}]
  (str "steals " card " and gains " points " agenda points"))

(defmethod render-text :start-run
  [{:keys [server ignore-costs cost]}]
  (str (if cost "make " "makes ")
       "a run on " (to-zone-name server)
       (when ignore-costs ", ignoring all costs")))

(defmethod render-text :continue-run
  [_]
  (str "will continue the run"))

(defmethod render-text :jack-out
  [input]
  ;; TODO also jack/jacks here
  (str (if (:cost input) "jack" "jacks") " out"))

(defmethod render-text :approach-ice
  [{:keys [ice]}]
  (str "approaches " (render-card ice)))

(defmethod render-text :bypass-ice
  [{:keys [ice]}]
  (str "bypasses " ice))

(defmethod render-text :encounter-ice
  [{:keys [ice]}]
  (str "encounters " (render-card ice)))

;; TODO this is relying on costs but costs always adds "to"
;; might need some refactoring...
(defmethod render-text :encounter-effect
  [{:keys [card]}]
  (str "on encountering " card))

(defmethod render-text :pass-ice
  [{:keys [ice]}]
  (str "passes " (render-card ice)))

;; TODO cost can be {} so need a better check for that one
(defmethod render-text :break-subs
  [{:keys [card ice subtype subs break-type sub-count str-boost cost]}]
  (let [sub-count (or sub-count (count subs))]
    (str (if str-boost
           (str (if cost "increase " "increases ")
                "the strength of " card
                " to " str-boost " and break ")
           (str (if cost "use " "uses ")
                card
                " to break "))
         (case (keyword break-type)
           :all (str "all " sub-count " subroutines")
           :remaining (str "the remaining " sub-count " subroutines")
           ;; N.B. a space is included in the passed down subtype currently...
           (quantify sub-count (str subtype "subroutine")))
         " on " ice
         (when-not break-type
           (str " (\"[subroutine] "
                (join "\" and \"[subroutine] " subs)
                "\")")))))

;; TODO red-headed stepchild here, burying stuff into :resolved unlike everything else
(defmethod render-text :resolve-subs
  [input]
  (let [info (:resolved input)
        ice (:ice info)
        resolved-subs (:subs info)]
    (str "resolves " (quantify (count resolved-subs) "unbroken subroutine")
         " on " ice
         " (\"[subroutine] "
         (join "\" and \"[subroutine] " resolved-subs)
         "\")")))

(defmethod render-text :approach-server
  [{:keys [server]}]
  (str "approaches " (to-zone-name server)))

(defmethod render-text :breach-server
  [{:keys [server]}]
  (str "breaches " (to-zone-name server)))

;; TODO need to support "everything else in archives"
(defmethod render-text :access
  [{:keys [card server]}]
  (str "accesses " (or card
                       (if (or (= server [:deck]) (= server ["deck"]))
                         "an unseen card"
                         "a card"))
       " from " (to-zone-name server)))

(defmethod render-text :access-all
  [_]
  "accesses everything else in Archives")

(defmethod render-text :trash
  [{:keys [card server]}]
  (str "trashes " (render-card card)
       (when (string? card) (when server (str " from " (to-zone-name server))))))

(defmethod render-text :take-damage
  [{:keys [cards cause]}]
  (str "trashes " (enumerate-str cards) " due to "
       (case (keyword cause)
         :net "net damage"
         :meat "meat damage"
         :brain "core damage")))

(defmethod render-text :rfg
  [{:keys [card]}]
  (str "removes " card " from the game"))

(defmethod render-text :discard
  [{:keys [card side reason]}]
  (let [not-map (or (string? card) (number? card) (coll? card))]
    (str "discards "
         (cond
           (string? card) card
           (number? card) (quantify card "card")
           (coll? card) (enumerate-str card)
           true (render-card card))
         (when not-map (str " from " (to-zone-name [:hand] side)))
         (when reason
           ;; TODO only end of turn is supported here, so...
           " at end of turn"))))

(defmethod render-text :win-game
  [_]
  "wins the game")

(defmethod render-text :direct-effect
  [{:keys [effect]}]
  ;; doesn't quite work yet, prevents doubling at least but there's a stray ' to'
  ;(render-effect effect)
  )

(defmethod render-text :fire-unbroken
  [{:keys [card]}]
  (str "indicates to fire all unbroken subroutines on " card))

(defmethod render-text :use-command
  [{:keys [command]}]
  (str "uses a command: " command))

(defmethod render-text :raw-text
  [input]
  (:raw-text input))

(defmethod render-text :default
  [input]
  (str "unknown type " input))

(defmethod render-map "en"
  [_ {:keys [username raw-text cost effect urgent] :as input}]
  (println input)
  (try-catchall
    (let [cost-str (render-cost-str input)
          effect-str (render-effect-str input)]
      (let [output (str (when urgent "[!]")
                        (if username
                          (str username " " cost-str (render-text input) effect-str ".")
                          raw-text))]
        (println output)
        output))
    (catch e# ::exception
      (throw e#)
      #_(str "BUG" (pprint-to-string input)))))

#_(defmethod render-map "en"
  [_ input]
  (str input))
