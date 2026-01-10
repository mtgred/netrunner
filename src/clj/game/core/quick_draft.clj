(ns game.core.quick-draft
  (:require
   [game.core.card :refer [corp? ice? identity? agenda? runner? has-subtype?]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.toasts :refer [toast]]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :refer [other-side]]
   [jinteki.chimera :refer [runner-bans corp-bans]]
   [game.core.eid :refer [effect-completed]]
   [game.macros :refer [continue-ability req msg]]
   [game.utils :refer [server-card same-side?]]
   ))



;; How are we doing this shit?
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


;; each draft instance looks like one of the following:
;;
;; {:type :identity
;;  :msg "Choose an identity"
;;  :qty x
;;  :choices ...
;; }

(def valid-3pointers
  ["Obokata Protocol"
   "Degree Mill"
   "Send a Message"
   "Bellona"
   "Global Food Initiative"
   "SSL Endorsement"
   "SDS Drone Deployment"
   "Vulnerability Audit"
   "The Future Perfect"
   "The Basalt Spire"
   "Ikawah Project"
   "Elective Upgrade"
   "Project Vacheron"])

(def valid-2pointers
  ["Blood in the Water"
   "Accelerated Beta Test"
   "AstroScript Pilot Program"
   "Philotic Entanglement"
   "Longevity Serum"
   "Medical Breakthrough"
   "Oaktown Renovation"
   "Offworld Office"
   "Cyberdex Sandbox"
   "Broad Daylight"
   "Above the Law"
   "Tomorrowʼs Headline"
   "Project Beale"])

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
  [input qty choices title]
  {:type :deck
   :prompt (str "Choose a " title ". You will receive " qty " copies")
   :qty qty
   :choices (take choices (shuffle input))})

(defn- generate-corp-quick-draft
  []
  (let [corp-cards (filter corp? (vals @all-cards))
        corp-format-cards (filter #(and (not (corp-bans (:title %)))
                                        (not (identity? %))
                                        (not (agenda? %)))
                             corp-cards)
        ice (take 12 (shuffle (filter ice? corp-format-cards)))
        corp-ice {:type :deck
                  :prompt "Choose an ice. You will receive 3 copies."
                  :qty 3
                  :choices (map :title ice)}
        corp-info {:type :info
                    :prompt "Your deck starts with 3 copies of Hedge Fund and 2 copies of Jackson Howard"}]
    [corp-info
     {:type :deck
      :prompt "Choose an agenda (This game will be played to 6 points. You will receive 2 copies, and have 14 points in your deck)"
      :qty 2
      :choices (take 5 (shuffle valid-3pointers))}
     {:type :deck
      :prompt "Choose an agenda (This game will be played to 6 points. You will receive 4 copies, and have 14 points in your deck)"
      :qty 4
      :choices (take 5 (shuffle valid-2pointers))}
     corp-ice
     (generate-pick corp-format-cards 3 9 "card")
     (generate-pick corp-format-cards 3 9 "card")
     (generate-pick corp-format-cards 3 9 "card")
     (generate-pick corp-format-cards 3 9 "card")
     {:type :identity
      :prompt "Choose your identity"
      :choices (take 4 (shuffle valid-corp-ids))}
     (generate-pick corp-format-cards 2 12 "card")
     (generate-pick corp-format-cards 2 12 "card")
     (generate-pick corp-format-cards 2 12 "card")
     (generate-pick corp-format-cards 2 12 "card")]))

(def valid-runner-ids
  ["Hayley Kaplan: Universal Scholar"
   "Nero Severn: Information Broker"
   "Lat: Ethical Freelancer"
   "Jamie \"Bzzz\" Micken: Techno Savant"
   "Boris \"Syfr\" Kovac: Crafty Veteran"
   "Wyvern: Chemically Enhanced"
   "Barry \"Baz\" Wong: Tri-Maf Veteran"
   "Silhouette: Stealth Operative"
   "Ele \"Smoke\" Scovak: Cynosure of the Net"
   "Edward Kim: Humanity's Hammer"
   "Nathaniel \"Gnat\" Hall: One-of-a-Kind"
   "Quetzal: Free Spirit"
   "Reina Roja: Freedom Fighter"
   "Nasir Meidan: Cyber Explorer"
   "Rielle “Kit” Peddler: Transhuman"
   "Zahya Sadeghi: Versatile Smuggler"
   "Captain Padma Isbister: Intrepid Explorer"
   "Az McCaffrey: Mechanical Prodigy"])

;; Corp is 34 cards, ?? picks
;; Runner should be 6 + 6 = 12 (3 picks)
;; then we want to get to 35(?) cards, 9 picks

(defn- generate-runner-quick-draft
  []
  (let [runner-cards (filter runner? (vals @all-cards))
        runner-format-cards (filter #(and (not (runner-bans (:title %)))
                                          (not (identity? %)))
                                    runner-cards)
        fracters (filter #(has-subtype? % "Fracter") runner-format-cards)
        decoders (filter #(has-subtype? % "Decoder") runner-format-cards)
        killers  (filter #(has-subtype? % "Killer")  runner-format-cards)
        runner-info {:type :info
                     :prompt "Your deck starts with 3 copies of Blueberry!™ Diesel, 2 copies of Sure Gamble, and 1 copy of Crypsis"}]
    [runner-info
     (generate-pick fracters 2 6 "fracter")
     (generate-pick decoders 2 6 "decoder")
     (generate-pick killers  2 6 "killer")
     ;; 12
     (generate-pick runner-format-cards 3 8 "card")
     (generate-pick runner-format-cards 3 8 "card")
     (generate-pick runner-format-cards 3 8 "card")
     (generate-pick runner-format-cards 3 8 "card")
     ;; 24
     {:type :identity
      :prompt "Choose your identity"
      :choices (take 4 (shuffle valid-runner-ids))}
     (generate-pick runner-format-cards 2 12 "card")
     (generate-pick runner-format-cards 2 12 "card")
     (generate-pick runner-format-cards 2 12 "card")
     (generate-pick runner-format-cards 2 12 "card")]))

(defn- generate-quick-draft
  []
  {:corp   (generate-corp-quick-draft)
   :runner (generate-runner-quick-draft)})

;; so final format is:
;;  Start with some quality
;;  Select your agendas
;;  1 picks x 3 copies
;;  4 picks x 3 copies
;; pick your id
;;  4 picks x 2 copies
;;
(defn- build-card
  "Duplicated code, circular restrictions"
  [card]
  (let [s-card (or (when (string? card)
                     (server-card card))
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

(defn- resolve-quick-draft
  [state side eid draft-queue draft-state]
  (if (seq (side draft-queue))
    (let [[item & rem] (side draft-queue)
          next-queue (assoc draft-queue side rem)]
      (continue-ability
        state side
        (case (:type item)
          :info {:prompt (str (:prompt item) " - you have " (dec (count (side draft-queue))) " picks to make")
                 :choices ["OK"]
                 :waiting-prompt true
                 :async true
                 :effect (req (resolve-quick-draft
                                state (other-side side) eid
                                next-queue draft-state))}
          :deck {:prompt (str (:prompt item) " - you have " (count (side draft-queue)) " picks remaining")
                 :choices (:choices item)
                 :async true
                 :waiting-prompt true
                 :effect (req (resolve-quick-draft
                                state (other-side side) eid
                                next-queue
                                (update-in draft-state [side :deck] into (repeat (:qty item) target))))}
          :identity {:prompt (str (:prompt item) " - you have " (count (side draft-queue)) " picks remaining")
                     :choices (:choices item)
                     :async true
                     :waiting-prompt true
                     :msg (msg "selects " target " as their identity")
                     :effect (req
                               (set-id state side target)
                               (resolve-quick-draft
                                    state (other-side side) eid
                                    next-queue
                                    (assoc-in draft-state [side :identity] target)))})
        nil nil))
    (let [c-deck
          (mapv #(assoc % :zone [:deck])
                (shuffle (mapv build-card (:deck (:corp draft-state)))))
          r-deck
          (mapv #(assoc % :zone [:deck])
                (shuffle (mapv build-card (:deck (:runner draft-state)))))]
      (swap! state assoc-in [:corp :deck] c-deck)
      (swap! state assoc-in [:runner :deck] r-deck)
      (swap! state assoc-in [:runner :agenda-point-req] 6)
      (swap! state assoc-in [:corp :agenda-point-req] 5)
      (effect-completed state side eid))))

(defn check-quick-draft
  [state format eid]
  (if (not= format "quick-draft")
    (effect-completed state nil eid)
    (let [draft-state (generate-quick-draft)]
      (resolve-quick-draft
        state :corp eid draft-state
        {:corp {:deck ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Jackson Howard" "Jackson Howard"]}
         :runner {:deck ["Blueberry!™ Diesel" "Blueberry!™ Diesel" "Blueberry!™ Diesel"
                         "Sure Gamble" "Sure Gamble" "Crypsis"]}}))))
