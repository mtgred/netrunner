(ns nr.translations
  (:require
    [clojure.string :refer [lower-case replace]]
    [nr.appstate :refer [app-state]]
    [taoensso.tempura :as tempura]))

(def translation-dictionary
  {
   :en 
   {:missing ":en missing text"
    :side
    {:corp "Corp"
     :runner "Runner"
     :any-side "Any Side"
     :all "All"}
    :faction
    {:all "All"
     :any-faction "Any Faction"
     :anarch "Anarch"
     :criminal "Criminal"
     :shaper "Shaper"
     :adam "Adam"
     :apex "Apex"
     :sunny-lebeau "Sunny Lebeau"
     :jinteki "Jinteki"
     :hass-bioroid "Haas-Bioroid"
     :nbn "NBN"
     :weyland-consortium "Weyland Consortium"
     :neutral "Neutral"}
    :format
    {:all "All"
     :any-format "Any Format"
     :standard "Standard"
     :eternal "Eternal"
     :core-experience "Core Experience"
     :snapshot "Snapshot"
     :snapshot-plus "Snapshot Plus"
     :socr "SOCR"
     :classic "Classic"
     :casual "Casual"}
    :card-type
    {:all "All"
     :identity "Identity"
     :agenda "Agenda"
     :asset "Asset"
     :upgrade "Upgrade"
     :operation "Operation"
     :ice "ICE"
     :event "Event"
     :hardware "Hardware"
     :resource "Resource"
     :program "Program"}
    :chat
    {:title "Play Android: Netrunner in your browser"
     :channels "Channels"
     :send "Send"
     :placeholder "Say something..."
     :delete "Delete Message"
     :delete-all "Delete All Messages From User"
     :block "Block User"
     :cancel "Cancel"}
    :nav
    {:chat "Chat"
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
    :menu
    {:settings :en.nav/settings
     :logout "Jack out"
     :admin :en.nav/admin
     :moderator "Moderator"}
    :card-browser
    {:search-hint "Search cards"
     :sort "Sort by"
     :format "Format"
     :set "Set"
     :side "Side"
     :faction "Faction"
     :type "Type"
     :clear "Clear"
     :select-art "Select Art"
     :update-success "Updated Art"
     :update-failure "Failed to Update Art"
     :sort-by {:faction "Faction"
              :name "Name"
              :type "Type"
              :influence "Influence"
              :cost "Cost"
              :set-number "Set number"}}
    :deck-builder
    {:loading-msg "Loading deck collection..."
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
     :clear-stats "Clear Stats"
     :create-game "Create Game"
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
     :hash "Tournament hash"
     :why "Why?"
     :legal "legal"
     :illegal "illegal"
     :games "Games"
     :completed "Completed"
     :won "Won"
     :lost "Lost"}
   :lobby
    {:no-games "No games"
     :tournament "Tournament"
     :competitive "Competitive"
     :casual "Casual"
     :new-game "New game"
     :reload "Reload list"
     :create "Create"
     :cancel "Cancel"
     :title "Title"
     :side "Side"
     :format "Format"
     :options "Options"
     :spectators "Allow spectators"
     :hidden "Make players' hidden information visible to spectators"
     :password-protected "Password protected"
     :password "Password"
     :start "Start"
     :leave "Leave"
     :swap "Swap sides"
     :waiting "Waiting players deck selection"
     :players "Players"
     :deck-selected "Deck selected"
     :select-deck "Select Deck"
     :chat "Chat"
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
     :private "PRIVATE"
     :reset "Reset Game Name"
     :delete "Delete Game"
     :password-for "Password for"
     :invalid-password "Invalid password"
     :not-allowed "Not allowed"
     :aborted "Connection aborted"}
   }
   
   :la-pig
   {:missing ":la-pig missing text"
    :side
    {:corp "Orpcay"
     :runner "Unnerray"
     :any-side "Anyay Idesay"
     :all "Allyay"}
    :faction
    {:all "Allyay"
     :any-faction "Anyay Actionfay"
     :anarch "Anarchyay"
     :criminal "Iminalcrimay"
     :shaper "Apershay"
     :adam "Adamyay"
     :apex "Apexyay"
     :sunny-lebeau "Unnsay Ebeaulay"
     :jinteki "Intekijay"
     :hass-bioroid "Aashay-Ioroidbay"
     :nbn "NBNYAY"
     :weyland-consortium "Eylandway Onsortiumcay"
     :neutral "Eutralnay"}
    :format
    {:all "Allyay"
     :any-format "Anyay Ormatfay"
     :standard "Andardstay"
     :eternal "Eternalyay"
     :core-experience "Orecay Experienceyay"
     :snapshot "Apshotsnay"
     :snapshot-plus "Apshotsnay Usplay"
     :socr "SOCRYAY"
     :classic "Assicclay"
     :casual "Asualcay"}
    :card-type
    {:all "Allyay"
     :identity "Identityay"
     :agenda "Agendayay"
     :asset "Assetyay"
     :upgrade "Upgradeyay"
     :operation "Operationyay"
     :ice "ICEYAY"
     :event "Eventyay"
     :hardware "Ardwarehay"
     :resource "Esourceray"
     :program "Ogrampray"}
    :chat
    {:title "Ayplay Android: Etrunnernay inyay ouryay owserbray"
     :channels "Annelschay"
     :send "Endsay"
     :placeholder "Aysay omethingsay..."
     :delete "Eleteday Essagemay"
     :delete-all "Eleteday Allyay Essagesmay Omfray Useryay"
     :block "Ockblay Useryay"
     :cancel "Ancelcay"}
    :nav
    {:chat "Atchay"
     :cards "Ardscay"
     :deck-builder "Eckday Uilderbay"
     :play "Ayplay"
     :help "Elphay"
     :settings "Ettingssay"
     :stats "Atsstay"
     :about "Aboutyay"
     :tournaments "Ournamentstay"
     :admin "Adminyay"
     :users "Usersyay"
     :features "Eaturesfay"
     :game-count (fn [[cnt]] (str cnt (if (= 1 cnt) " Amegay" " Amesgay"))) }
    :menu
    {:settings :la-pig.nav/settings
     :logout "Ackjay outyay"
     :admin :la-pig.nav/admin
     :moderator "Oderatormay"}
    :card-browser
    {:search-hint "Earchsay ardscay"
     :sort "Ortsay ybay"
     :format "Ormatfay"
     :set "Etsay"
     :side "Idesay"
     :faction "Actionfay"
     :type "Etypay"
     :clear "Earclay"
     :select-art "Electsay Artyay"
     :update-success "Updatedyay Artyay"
     :update-failure "Ailedfay otay Updateyay Artyay"
     :sort-by {:faction "Actionfay"
              :name "Amenay"
              :type "Etypay"
              :influence "Influenceyay"
              :cost "Ostcay"
              :set-number "Etsay umbernay"}}
    :deck-builder
    {:loading-msg "Oadinglay eckday ollectioncay..."
     :new-corp "Ewnay Orpcay eckday"
     :new-runner "Ewnay Unnerray eckday"
     :import-button "Importyay Eckday"
     :reset "Esetray"
     :import-title "Enteryay ayay ublicpay bnrday eckday idyay oryay urlyay"
     :import "Importyay"
     :cancel "Ancelcay"
     :import-placeholder "Bnrday idyay"
     :deck-count (fn [[cnt]] (str cnt (if (= 1 cnt) " Eckday" " Ecksday")))
     :filtered "(ilteredfay)"
     :save "Avesay"
     :confirm-delete "Onfirmcay Eleteday"
     :edit "Edityay"
     :delete "Eleteday"
     :clear-stats "Earclay Atsstay"
     :create-game "Eatecray Amegay"
     :deck-name "Eckday Amenay"
     :format "Ormatfay"
     :identity "Identityay"
     :deck-notes "Eckday otesnay"
     :decklist "Ecklistday"
     :decklist-inst "(Ypetay oryay astepay ayay ecklistday, ityay illway ebay arsedpay)"
     :notes "Otesnay"
     :add-to-deck "Ddaay otay eckday"
     :add-cards "Ddaay ardscay"
     :card-name "Ardcay amenay"
     :no-decks "Onay ecksday"
     :cards "ardscay"
     :min "inimummay"
     :max "aximummay"
     :influence "Influenceyay"
     :agenda-points "Agendayay ointspay"
     :hash "Ournamenttay ashhay"
     :why "Whyay?"
     :legal "egallay"
     :illegal "illegalyay"
     :games "Amesgay"
     :completed "Ompletedcay"
     :won "Onway"
     :lost "Ostlay"}
   :lobby
    {:no-games "Onay amesgay"
     :tournament "Ournamenttay"
     :competitive "Ompetitivecay"
     :casual "Asualcay"
     :new-game "Ewnay amegay"
     :reload "Eloadray istlay"
     :create "Eatecray"
     :cancel "Ancelcay"
     :title "Itletay"
     :side "Idesay"
     :format "Ormatfay"
     :options "Optionsyay"
     :spectators "Allowyay ectatorsspay"
     :hidden "Akemay ayersplay iddenhay informationyay isiblevay otay ectatorsspay"
     :password-protected "Asswordpay otectedpray"
     :password "Asswordpay"
     :start "Artstay"
     :leave "Eavelay"
     :swap "Apsway idessay"
     :waiting "Aitingway ayersplay eckday electionsay"
     :players "Ayersplay"
     :deck-selected "Eckday electedsay"
     :select-deck "Electsay eckday"
     :chat "Atchay"
     :select-title "Electsay ouryay eckday"
     :spectator-count (fn [[cnt]] (str cnt " Ectatorspay" (when (not= cnt 1) "s")))
     :closed-msg "Amegay obbylay osedclay ueday otay inactivityay"
     :title-error "Easeplay illfay ayay amegay itletay."
     :password-error "Easepl illfay ayay asswordpay."
     :too-little-data "Ootay ittlelay ataday"
     :completion-rate "Amegay Ompletioncay Ateray"
     :watch "Atchway"
     :join "Oinjay"
     :rejoin "Ejoinray"
     :private "IVATEPRAY"
     :reset "Esetray Amegay Amenay"
     :delete "Eleteday Amegay"
     :password-for "Asswordpay orfay"
     :invalid-password "Nvalidiay asswordpay"
     :not-allowed "Otnay allowedyay"
     :aborted "Onnectioncay abortedyay"}
   }})

(def opts {:dict translation-dictionary})

(defn tr [resource & params]
  (let [lang (keyword (get-in @app-state [:options :language] "en"))]
    (tempura/tr opts [lang :en] resource (vec params))))

(defn tr-string [prefix s]
  (let [side (lower-case (replace s " " "-"))
        kw (keyword (str prefix "." side))]
    (tr [kw "Unknown"])))

(def tr-type (partial tr-string "card-type"))
(def tr-side (partial tr-string "side"))
(def tr-faction (partial tr-string "faction"))
(def tr-format (partial tr-string "format"))
(def tr-sort (partial tr-string "card-browser.sort-by"))
(def tr-watch-join (partial tr-string "lobby"))
