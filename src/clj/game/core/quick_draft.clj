(ns game.core.quick-draft
  (:require
   [game.core.card :refer [corp? ice? identity? agenda? runner? has-subtype? has-any-subtype? program?]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.prompts :refer [show-wait-prompt show-prompt clear-wait-prompt]]
   [game.core.say :refer [system-msg]]
   [game.core.toasts :refer [toast]]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :refer [other-side]]
   [jinteki.chimera :refer [corp-bans]]
   [game.core.eid :refer [effect-completed]]
   [game.macros :refer [continue-ability req msg wait-for]]
   [game.utils :refer [quantify server-card same-side?]]))

(def runner-bans
  #{"Blackmail"
    "Calling in Favors"
    "Charm Offensive"
    "Data Breach"
    "Diana's Hunt"
    "Direct Access"
    "Employee Strike"
    "Endurance"
    "Exploratory Romp"
    "Feint"
    "Government Investigations"
    "Immolation Script"
    "Itinerant Protesters"
    "Leverage"
    "Mass Install"
    "Networking"
    "Office Supplies"
    "Paper Tripping"
    "Populist Rally"
    "Power Nap"
    "Rebirth"
    "Reboot"
    "Spree"
    "Surge"
    "Traffic Jam"
    "Uninstall"
    ;; "Watch the World Burn"
    "Acacia"
    "Archives Interface"
    "BMI Buffer"
    "Bookmark"
    "Capstone"
    "Capybara"
    "Deep Red"
    ;; "Dorm Computer"
    "Ekomind"
    "Forger"
    ;; "Jeitinho"
    "LLDS Processor"
    "MemStrips"
    "Monolith"
    "Mu Safecracker"
    "Muresh Bodysuit"
    "Plascrete Carapace"
    "Public Terminal"
    "Qianju PT"
    "Rabbit Hole"
    "Ramujan-reliant 550 BMI"
    "Recon Drone"
    "Replicator"
    "Security Chip"
    "Titanium Ribs"
    "Unregistered S&W '35"
    "Window"
    "Activist Support"
    "Adjusted Chronotype"
    "Akshara Sareen"
    "Angel Arena"
    "Assimilator"
    "Bazaar"
    "Bio-Modeled Network"
    "Chrome Parlor"
    "Citadel Sanctuary"
    "Cookbook"
    "Crash Space"
    ;; "Daeg, First Net-Cat"
    "Debbie \"Downtown\" Moreira"
    "District 99"
    "Donut Taganes"
    "Dr. Lovegood"
    "Fester"
    "Bloo Moose"
    "Rezeki"
    "First Responders"
    "Gene Conditioning Shoppe"
    "Globalsec Security Clearance"
    "Investigative Journalism"
    "Investigator Inez Delgado"
    "Investigator Inez Delgado 2"
    "Investigator Inez Delgado 3"
    "Investigator Inez Delgado 4"
    "Jarogniew Mercs"
    "Keros Mcintyre"
    "Liberated Chela"
    "Motivation"
    ;; "Net Mercur"
    "New Angeles City Hall"
    "Off-Campus Apartment"
    "Order of Sol"
    "Paige Piper"
    "Paparazzi"
    "Power Tap"
    "Public Sympathy"
    "Sacrificial Clone"
    "Shadow Team"
    "Starlight Crusade Funding"
    "Synthetic Blood"
    "Tallie Perrault"
    "The Back"
    "Thunder Art Gallery"
    "Underworld Contact"
    "Urban Art Vernissage"
    "Valentina Ferreira Carvalho"
    "Virus Breeding Ground"
    "Wasteland"
    "Whistleblower"
    "Wireless Net Pavilion"
    ;; "Blackstone"
    "Crowbar"
    ;; "Dagger"
    "Dai V"
    ;; "Fawkes"
    ;; "Houdini"
    ;; "Sage"
    "Shiv"
    "Spike"
    ;; "Umbrella"
    "Au Revoir"
    "Copycat"
    "Disrupter"
    "Flux Capacitor"
    "Heliamphora"
    "Hivemind"
    "Incubator"
    "Ixodidae"
    "LLDS Energy Regulator"
    "Panchatantra"
    "Pawn"
    "Plague"
    "Progenitor"
    "Surveillance Network Key"
    "Surveillance Network Key 2"
    })

;; How are we doing this?
;; CORP:
;;  We must have agendas.
;;  We need to draft agendas.
;;  Do we play a 16 point deck?
;;  We do that with:
;;  2x 3points (GFI)= 6 | 06
;;  4x 2points      = 8 | 14
;; That's 6 slots gone instantly.
;;  Then there are 28 slots to fill
;;  Let's cheat and do 8 picks, 2 copies of jhow, and 1 set of hedge funds
;;  The first pick will be 8 ice
;;  Everything after that will be 8 choose 1
;;  Then you draft an Identity. Use all the draft ids, and then a small subset of other ids.

;; RUNNER:
;;  We must have a way to break each fundamental ice type
;;  We should also start with 3x Sure Gamble
;;  We want to be making the same number of choices as the corp (11 of them)
;;  30 + 3 + 6 padding cards + identity
;;    3 copies of blueberry diesel
;;    3 copies of sure gamble
;;    2 copies of crypsis
;;
;; Full up to date explanation of the drafting process can be found here: https://github.com/mtgred/netrunner/wiki/Drafting-on-Jinteki.net-%E2%80%90-Quick-Draft-Format

;; each draft instance looks like one of the following:
;;
;; {:type :identity
;;  :msg "Choose an identity"
;;  :qty x
;;  :choices ...
;; }

(def valid-3pointers
  ["Bellona"
   "City Works Project"
   "Degree Mill"
   "Elective Upgrade"
   "Fuji Asset Retrieval"
   "Global Food Initiative"
   "Ikawah Project"
   "Obokata Protocol"
   "Project Vacheron"
   "SDS Drone Deployment"
   "Send a Message"
   "SSL Endorsement"
   "The Basalt Spire"
   "The Future Perfect"
   "Vulnerability Audit"])

(def valid-2pointers
  ["Above the Law"
   "Accelerated Beta Test"
   "AstroScript Pilot Program"
   "Azef Protocol"
   "Broad Daylight"
   "Blood in the Water"
   "Cyberdex Sandbox"
   "Longevity Serum"
   "Medical Breakthrough"
   "NAPD Contract"
   "Freedom of Information"
   "Oaktown Renovation"
   "Offworld Office"
   "Philotic Entanglement"
   "Project Beale"
   "Tomorrow's Headline"])

(def valid-corp-ids
  ["Strategic Innovations: Future Forward"
   "Synthetic Systems: The World Re-imagined"
   "Information Dynamics: All You Need To Know"
   "Fringe Applications: Tomorrow, Today"
   "Cybernetics Division: Humanity Upgraded"
   "Hyoubu Institute: Absolute Clarity"
   "AgInfusion: New Miracles for a New World"
   "PT Untaian: Life's Building Blocks"
   "NBN: The World is Yours*"
   "Pravdivost Consulting: Political Solutions"
   "Argus Security: Protection Guaranteed"
   "Thule Subsea: Safety Below"
   "Sportsmetal: Go Big or Go Home"
   "Jinteki: Replicating Perfection"
   "NBN: Reality Plus"
   "Epiphany Analytica: Nations Undivided"
   "Weyland Consortium: Built to Last"
   "Haas-Bioroid: Precision Design"
   "The Outfit: Family Owned and Operated"
   "Earth Station: SEA Headquarters"])

(defn- generate-pick
  "Generates a pick from one category of cards"
  [input qty choices title phase]
  {:type :deck
   :prompt (str "Choose a " title ". You will receive " qty " copies")
   :qty qty
   :phase phase
   :choices (take choices (shuffle input))})

(defn- generate-multi-pick
  "Generates a pick from two categories of card"
  [input-a input-b qty choices-a choices-b title phase]
  {:type :deck
   :prompt (str "Choose a " title ". You will receive " qty " copies")
   :qty qty
   :phase phase
   :choices (concat
              (take choices-a (shuffle input-a))
              (take choices-b (shuffle input-b)))})

(defn- generate-corp-quick-draft
  []
  (let [corp-cards (filter #(and (corp? %)
                                 (not= "tdc" (:set_code %))) (vals @all-cards))
        corp-format-cards (filter #(and (not (corp-bans (:title %)))
                                        (not (identity? %))
                                        (not (agenda? %)))
                             corp-cards)
        ice (take 12 (shuffle (filter ice? corp-format-cards)))
        corp-ice {:type :deck
                  :prompt "Choose an ice. You will receive 3 copies."
                  :phase :ice
                  :qty 3
                  :choices (map :title ice)}
        corp-info {:type :info
                   :phase :info
                   :cards ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Jackson Howard" "Jackson Howard"]
                   :prompt "Your deck starts with 3 copies of Hedge Fund and 2 copies of Jackson Howard"}]
    {:info corp-info
     :stage-one [{:type :deck
                  :phase :agenda-3
                  :prompt "Choose an agenda (This game will be played to 6 points. You will receive 2 copies, and have 14 points in your deck)"
                  :qty 2
                  :choices (take 5 (shuffle valid-3pointers))}
                 {:type :deck
                  :phase :agenda-2
                  :prompt "Choose an agenda (This game will be played to 6 points. You will receive 4 copies, and have 14 points in your deck)"
                  :qty 4
                  :choices (take 5 (shuffle valid-2pointers))}
                 corp-ice
                 (generate-pick corp-format-cards 3 9 "card" :card)
                 (generate-pick corp-format-cards 3 9 "card" :card)
                 (generate-pick corp-format-cards 3 9 "card" :card)
                 (generate-pick corp-format-cards 3 9 "card" :card)]
     :identity {:type :identity
                :phase :identity
                :prompt "Choose your identity"
                :choices (take 4 (shuffle valid-corp-ids))}
     :stage-two [(generate-pick corp-format-cards 2 12 "card" :card)
                 (generate-pick corp-format-cards 2 12 "card" :card)
                 (generate-pick corp-format-cards 2 12 "card" :card)
                 (generate-pick corp-format-cards 2 12 "card" :card)]}))

(def valid-runner-ids
  ["Hayley Kaplan: Universal Scholar"
   "Lat: Ethical Freelancer"
   "Jamie \"Bzzz\" Micken: Techno Savant"
   "Ele \"Smoke\" Scovak: Cynosure of the Net"
   "Nasir Meidan: Cyber Explorer"
   "Rielle \"Kit\" Peddler: Transhuman"
   "Captain Padma Isbister: Intrepid Explorer"

   "Nero Severn: Information Broker"
   "Boris \"Syfr\" Kovac: Crafty Veteran"
   "Barry \"Baz\" Wong: Tri-Maf Veteran"
   "Silhouette: Stealth Operative"
   "Zahya Sadeghi: Versatile Smuggler"
   "Az McCaffrey: Mechanical Prodigy"
   "Gabriel Santiago: Consummate Professional"

   "Wyvern: Chemically Enhanced"
   "Edward Kim: Humanity's Hammer"
   "Nathaniel \"Gnat\" Hall: One-of-a-Kind"
   "Topan: Ormas Leader"
   "Sebastião Souza Pessoa: Activist Organizer"
   "Quetzal: Free Spirit"
   "Reina Roja: Freedom Fighter"])

;; Corp is 34 cards, ?? picks
;; Runner should be 6 + 6 = 12 (3 picks)
;; then we want to get to 35(?) cards, 9 picks

(defn- generate-runner-quick-draft
  []
  (let [runner-cards (filter #(and (runner? %)
                                   (not= "tdc" (:set_code %)))
                             (vals @all-cards))
        runner-format-cards (filter #(and (not (runner-bans (:title %)))
                                          (not (identity? %)))
                                    runner-cards)
        not-main-breaker? (fn [c] (not (has-any-subtype? c ["Fracter" "Decoder" "Killer"])))
        non-programs (filter not-main-breaker? runner-format-cards)
        fracters (filter #(has-subtype? % "Fracter") runner-format-cards)
        decoders (filter #(has-subtype? % "Decoder") runner-format-cards)
        killers  (filter #(has-subtype? % "Killer")  runner-format-cards)
        runner-info {:type :info
                     :cards ["Blueberry!™ Diesel" "Blueberry!™ Diesel" "Blueberry!™ Diesel"
                             "Sure Gamble" "Sure Gamble" "Crypsis"]
                     :phase :info
                     :prompt "Your deck starts with 3 copies of Blueberry!™ Diesel, 2 copies of Sure Gamble, and 1 copy of Crypsis"}]
    {:info runner-info
     :stage-one [(generate-pick non-programs 3 9 "card" :card)
                 (generate-pick runner-format-cards 3 10 "card" :card)
                 (generate-pick non-programs 3 9 "card" :card)
                 (generate-pick runner-format-cards 3 10 "card" :card)
                 (generate-multi-pick fracters non-programs 2 4 3 "fracter" :fracter)
                 (generate-multi-pick decoders non-programs 2 4 3 "decoder" :decoder)
                 (generate-multi-pick killers  non-programs 2 4 3 "killer" :killer)]
     :identity {:type :identity
                :phase :identity
                :prompt "Choose your identity"
                :choices (take 4 (shuffle valid-runner-ids))}
     :stage-two [(generate-pick non-programs 2 12 "card" :card)
                 (generate-pick runner-format-cards 2 12 "card" :card)
                 (generate-pick non-programs 2 12 "card" :card)
                 (generate-pick runner-format-cards 2 12 "card" :card)]}))

(defn- combine [corp runner]
  (vec (map (fn [c r]
              {:type :deck
               :corp c
               :runner r})
            corp runner)))

(defn- generate-quick-draft
  []
  (let [corp   (generate-corp-quick-draft)
        runner (generate-runner-quick-draft)
        corp-info (:info corp)
        runner-info (:info runner)
        corp-stage-one (:stage-one corp)
        runner-stage-one (:stage-one runner)
        corp-id (:identity corp)
        runner-id (:identity runner)
        corp-stage-two (:stage-two corp)
        runner-stage-two (:stage-two runner)]
    (concat
      [{:type :info
        :corp corp-info
        :runner runner-info}]
      (combine corp-stage-one runner-stage-one)
      [{:type :identity
        :corp corp-id
        :runner runner-id}]
      (combine corp-stage-two runner-stage-two))))

(defn- build-card
  "Duplicated code, circular restrictions"
  [card]
  (let [s-card (or (when (string? card) (server-card card))
                   (server-card (:title card))
                   card)]
    (assoc (make-card s-card) :art (:art card))))

(defn- set-id
  [state side card-name]
  (try
    (let [s-card (server-card card-name)
          card (when (and s-card (same-side? (:side s-card) side))
                 (build-card s-card))]
      (if card
        (let [new-id (-> card :title server-card make-card (assoc :zone [:identity] :type "Identity"))]
          (swap! state assoc-in [side :identity] new-id)
          (card-init state side new-id {:resolve-effect true :init-data true}))
        (toast state side (str card-name " isn't a valid card"))))
    (catch Exception ex
      (toast state side (str card-name " isn't a real card")))))

(defn- add-cards
  ([state side cards]
   (let [cards (mapv #(assoc (build-card %) :zone [:deck]) cards)]
     (swap! state update-in [side :deck] into cards)))
  ([state side qty card]
   (add-cards state side (repeat qty card))))

(defn- maybe-complete
  [state side eid f]
  #(let [target (:value %)
         os (other-side side)
         other-side-complete? (-> @state :draft :pick os)]
     (wait-for
       (f state side nil [target])
       (if-not other-side-complete?
         ^:ignore-async-check
         (do (swap! state assoc-in [:draft :pick side] true)
             (show-wait-prompt state side "your opponent to pick an option")
             ;; deliberately leave the eid open until both players have picked
             )
         (do (clear-wait-prompt state (other-side side))
             (effect-completed state side eid))))))

(defn- show-draft-prompt
  [state side eid corp runner]
  (swap! state assoc-in [:draft :pick] {})
  (show-prompt state :corp nil   (:prompt corp)   (:choices corp)   (maybe-complete state :corp eid   (:effect corp))   {:prompt-type :draft})
  (show-prompt state :runner nil (:prompt runner) (:choices runner) (maybe-complete state :runner eid (:effect runner)) {:prompt-type :draft}))

(defn- picks-remaining
  [s remaining]
  (str s " - you have " (quantify remaining "pick") " remaining"))

(defn- info-prompt
  [{:keys [cards prompt]} remaining]
  {:prompt (picks-remaining prompt remaining)
   :choices ["OK"]
   :async true
   :effect (req (add-cards state side cards)
                (effect-completed state side eid))})

(defn- deck-prompt
  [{:keys [choices prompt qty]} remaining]
  {:prompt (picks-remaining prompt (inc remaining))
   :choices choices
   :async true
   :waiting-prompt true
   :effect (req
             (add-cards state side qty target)
             (effect-completed state side eid))})

(defn- id-prompt
  [state side corp runner remaining]
  (let [runner-prompt {:prompt (picks-remaining (:prompt runner) (inc remaining))
                       :choices (:choices runner)
                       :waiting-prompt true
                       :effect (req (system-msg state side (str "selects " target " as their identity"))
                                    (set-id state side target))}]
    {:prompt (str (:prompt corp) " - you have " remaining " picks remaining")
     :choices (:choices corp)
     :waiting-prompt true
     :async true
     :effect (req (system-msg state side (str "selects " target " as their identity"))
                  (set-id state side target)
                  (continue-ability state :runner runner-prompt nil nil))}))

(defn- resolve-quick-draft
  [state side eid draft-queue]
  (if (seq draft-queue)
    (let [{:keys [corp runner] :as item} (first draft-queue)
          next-queue (rest draft-queue)
          remaining (count next-queue)]
      (swap! state update-in [:draft :phase] inc)
      (case (:type item)
        :info (wait-for
                (show-draft-prompt
                  state side
                  (info-prompt corp   remaining)
                  (info-prompt runner remaining))
                (resolve-quick-draft state side eid next-queue))
        :deck (wait-for
                (show-draft-prompt
                  state side
                  (deck-prompt corp remaining)
                  (deck-prompt runner remaining))
                (resolve-quick-draft state side eid next-queue))
        :identity (wait-for
                    (resolve-ability state side (id-prompt state :corp corp runner remaining) nil nil)
                    (resolve-quick-draft state side eid next-queue))))
    (do (swap! state update-in [:corp   :deck] shuffle)
        (swap! state update-in [:runner :deck] shuffle)
        (swap! state dissoc :draft)
        (effect-completed state side eid))))

(defn- phases
  [draft-queue side]
  (mapv #(-> % side :phase) draft-queue))

(defn check-quick-draft
  [state format eid]
  (if (not= format "quick-draft")
    (effect-completed state nil eid)
    (let [draft-state (generate-quick-draft)]
      (swap! state assoc-in [:runner :agenda-point-req] 6)
      (swap! state assoc-in [:corp :agenda-point-req] 5)
      (swap! state assoc :draft {:phase -1 :phases {:corp   (phases draft-state :corp)
                                                    :runner (phases draft-state :runner)}})
      (resolve-quick-draft state :corp eid draft-state))))
