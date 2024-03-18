(ns game.cards.identities
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [game.core.access :refer [access-bonus access-cost-bonus access-non-agenda]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed card->server
                            get-remote-names get-remotes server->zone]]
   [game.core.card :refer [agenda? asset? can-be-advanced?
                           corp-installable-type? corp? faceup? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype?
                           ice? in-discard? in-hand? in-play-area? installed? is-type? operation? program?
                           resource? rezzed? runner? upgrade?]]
   [game.core.charge :refer [charge-ability]]
   [game.core.cost-fns :refer [install-cost play-cost
                               rez-additional-cost-bonus rez-cost]]
   [game.core.damage :refer [chosen-damage corp-can-choose-damage? damage
                             enable-corp-damage-choice]]
   [game.core.def-helpers :refer [corp-recur defcard offer-jack-out]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed is-basic-advance-action? make-eid]]
   [game.core.engine :refer [not-used-once? pay register-events register-once resolve-ability trigger-event]]
   [game.core.events :refer [event-count first-event?
                             first-successful-run-on-server? no-event? not-last-turn? run-events turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-latest]]
   [game.core.flags :refer [card-flag? clear-persistent-flag!
                            register-persistent-flag! register-turn-flag! zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-extra-sub! break-sub pump-ice remove-sub! update-all-ice update-all-icebreakers]]
   [game.core.initializing :refer [make-card]]
   [game.core.installing :refer [corp-install install-locked? runner-can-pay-and-install? runner-install]]
   [game.core.link :refer [link+ update-link]]
   [game.core.mark :refer [identify-mark-ability]]
   [game.core.memory :refer [mu+]]
   [game.core.moving :refer [mill move swap-ice trash trash-cards]]
   [game.core.optional :refer [get-autoresolve never? set-autoresolve]]
   [game.core.payment :refer [build-cost-label can-pay? cost-name cost->string merge-costs]]
   [game.core.pick-counters :refer [pick-virus-counters-to-spend]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.revealing :refer [conceal-hand reveal reveal-hand]]
   [game.core.rezzing :refer [rez]]
   [game.core.runs :refer [end-run get-current-encounter make-run redirect-run
                           set-next-phase start-next-phase total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote? name-zone
                              target-server zone->name]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck]]
   [game.core.tags :refer [gain-tags lose-tags tag-prevent]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [number-of-runner-virus-counters]]
   [game.core.winning :refer [check-win-by-agenda]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

;;; Helper functions for Draft cards
(def draft-points-target
  "Set each side's agenda points target at 6, per draft format rules"
  (req (swap! state assoc-in [:runner :agenda-point-req] 6)
       (swap! state assoc-in [:corp :agenda-point-req] 6)))

(defn- has-most-faction?
  "Checks if the faction has a plurality of rezzed / installed cards"
  [state side fc]
  (let [card-list (all-active-installed state side)
        faction-freq (frequencies (map :faction card-list))
        reducer (fn [{:keys [max-count] :as acc} faction count]
                  (cond
                    ;; Has plurality update best-faction
                    (> count max-count)
                    {:max-count count :max-faction faction}
                    ;; Lost plurality
                    (= count max-count)
                    (dissoc acc :max-faction)
                    ;; Count is not more, do not change the accumulator map
                    :else
                    acc))
        best-faction (:max-faction (reduce-kv reducer {:max-count 0 :max-faction nil} faction-freq))]
    (= fc best-faction)))

;; Card definitions

(defcard "419: Amoral Scammer"
  {:events [{:event :corp-install
             :async true
             :req (req (and (first-event? state :corp :corp-install)
                            (pos? (:turn @state))
                            (not (rezzed? (:card context)))
                            (not (#{:rezzed-no-cost :rezzed-no-rez-cost :rezzed :face-up} (:install-state context)))))
             :waiting-prompt true
             :effect
             (effect
               (continue-ability
                 {:optional
                  {:prompt "Expose installed card unless the Corp pays 1 [Credits]?"
                   :player :runner
                   :autoresolve (get-autoresolve :auto-fire)
                   :no-ability {:effect (req (clear-wait-prompt state :corp))}
                   :yes-ability
                   {:async true
                    :effect (req (if (not (can-pay? state :corp eid card nil :credit 1))
                                   (do
                                     (toast state :corp "Cannot afford to pay 1 [Credit] to block card exposure" "info")
                                     (expose state :runner eid (:card context)))
                                   (continue-ability
                                     state side
                                     {:optional
                                      {:waiting-prompt true
                                       :prompt (req (str "Pay 1 [Credits] to prevent exposing " (card-str state (:card context)) "?"))
                                       :player :corp
                                       :no-ability
                                       {:async true
                                        :effect (effect (expose :runner eid (:card context)))}
                                       :yes-ability
                                       {:async true
                                        :effect
                                        (req (wait-for
                                               (pay state :corp (make-eid state eid) card [:credit 1])
                                               (system-msg state :corp
                                                           (str (:msg async-result)
                                                                " to prevent exposing "
                                                                (card-str state (:card context))))
                                               (effect-completed state side eid)))}}}
                                     card targets)))}}}
                 card targets))}]
   :abilities [(set-autoresolve :auto-fire "419: Amoral Scammer")]})

(defcard "A Teia: IP Recovery"
  {:events [{:event :corp-install
             :async true
             :req (req (and (is-remote? (second (get-zone (:card context))))
                            (first-event? state side :corp-install #(is-remote? (second (get-zone (:card (first %))))))))
             :effect (req (let [original-server (zone->name (second (get-zone (:card context))))]
                            (continue-ability
                              state side
                              {:prompt "Choose a card to install in or protecting another remote server"
                               :waiting-prompt true
                               :choices {:card #(and (corp? %)
                                                     (corp-installable-type? %)
                                                     (in-hand? %))}
                               :async true
                               :effect (req (let [chosen-card target]
                                              (continue-ability
                                                state side
                                                {:prompt "Choose a remote server"
                                                 :waiting-prompt true
                                                 :msg "install a card from HQ ignoring all costs"
                                                 :choices (req (conj (vec (filter #(not= original-server %)
                                                                                  (get-remote-names state))) "New remote"))
                                                 :async true
                                                 :effect (effect (corp-install eid chosen-card target {:ignore-install-cost true}))}
                                                card nil)))}
                              card nil)))}]
   ;; This effect will be resolved when the ID is reenabled after Strike / Direct Access
   :effect (effect
             (continue-ability
               {:req (req (< 2 (count (get-remotes state))))
                :prompt "Choose 2 servers to be saved from the rules apocalypse"
                :choices (req (get-remote-names state))
                :async true
                :effect (req (let [saved target]
                               (continue-ability
                                 state side
                                 {:prompt "Choose another server to save"
                                  :choices (req (filter #(not= saved %) (get-remote-names state)))
                                  :async true
                                  :effect (req (let [to-be-trashed (remove #(in-coll? ["Archives" "R&D" "HQ" target saved] (zone->name (second (get-zone %))))
                                                                           (all-installed state :corp))]
                                                 (system-msg state side (str "chooses " target " and " saved " to be saved from the rules apocalypse and trashes "
                                                                             (quantify (count to-be-trashed) "card")))
                                                 ;; these cards get trashed by the game and not by players
                                                 (trash-cards state side eid to-be-trashed {:unpreventable true :game-trash true})))}
                                 card nil)))}
               card nil))})

(defcard "Acme Consulting: The Truth You Need"
  (letfn [(outermost? [state ice]
            (let [server-ice (:ices (card->server state ice))]
              (same-card? ice (last server-ice))))]
    {:static-abilities [{:type :tags
                         :req (req (and (get-current-encounter state)
                                        (rezzed? current-ice)
                                        (outermost? state current-ice)))
                         :value 1}]}))

(defcard "Adam: Compulsive Hacker"
  {:events [{:event :pre-start-game
             :req (req (= side :runner))
             :async true
             :waiting-prompt true
             :effect (req (let [directives (->> (server-cards)
                                                (filter #(has-subtype? % "Directive"))
                                                (map make-card)
                                                (map #(assoc % :zone [:play-area]))
                                                (into []))]
                            ;; Add directives to :play-area - assumed to be empty
                            (swap! state assoc-in [:runner :play-area] directives)
                            (continue-ability
                              state side
                              {:prompt (str "Choose 3 starting directives")
                               :choices {:max 3
                                         :all true
                                         :card #(and (runner? %)
                                                     (in-play-area? %))}
                               :effect (req (wait-for
                                              (runner-install
                                                state side
                                                (make-eid state eid) (first targets)
                                                {:ignore-all-cost true
                                                 :custom-message (fn [_] (str "starts with " (:title (first targets)) " in play"))})
                                              (wait-for
                                                (runner-install
                                                  state side
                                                  (make-eid state eid) (second targets)
                                                  {:ignore-all-cost true
                                                   :custom-message (fn [_] (str "starts with " (:title (second targets)) " in play"))})
                                                (wait-for
                                                  (runner-install
                                                  state side
                                                  (make-eid state eid) (-> targets
                                                                           next
                                                                           next
                                                                           first)
                                                  {:ignore-all-cost true
                                                   :custom-message (fn [_] (str "starts with " (:title (-> targets
                                                                           next
                                                                           next
                                                                           first)) " in play"))})
                                                  (swap! state assoc-in [:runner :play-area] [])
                                                  (effect-completed state nil eid))))
                                            )}
                              card nil)))}]})

(defcard "AgInfusion: New Miracles for a New World"
  {:abilities [{:label "Trash a piece of ice to choose another server- the runner is now running that server"
                :once :per-turn
                :async true
                :req (req (and run
                               (= :approach-ice (:phase run))
                               (not (rezzed? current-ice))))
                :prompt "Choose another server and redirect the run to its outermost position"
                :choices (req (cancellable (remove #{(-> @state :run :server central->name)} servers)))
                :msg (msg "trash the approached piece of ice. The Runner is now running on " target)
                :effect (req (let [dest (server->zone state target)
                                   ice (count (get-in corp (conj dest :ices)))
                                   phase (if (pos? ice) :encounter-ice :movement)]
                               (wait-for (trash state side (make-eid state eid) current-ice {:unpreventable true})
                                         (redirect-run state side target phase)
                                         (start-next-phase state side eid))))}]})

(defcard "Akiko Nisei: Head Case"
  {:events [{:event :breach-server
             :interactive (req true)
             :psi {:req (req (= target :rd))
                   :player :runner
                   :equal {:msg "access 1 additional card"
                           :effect (effect (access-bonus :rd 1)
                                           (effect-completed eid))}}}]})

(defcard "Alice Merchant: Clan Agitator"
  {:events [{:event :successful-run
             :interactive (req true)
             :req (req (and (= :archives (target-server context))
                            (first-successful-run-on-server? state :archives)
                            (not-empty (:hand corp))))
             :waiting-prompt true
             :prompt "Choose a card in HQ to discard"
             :player :corp
             :choices {:all true
                       :card #(and (in-hand? %)
                                   (corp? %))}
             :msg "force the Corp to trash 1 card from HQ"
             :async true
             :effect (effect (trash :corp eid target nil))}]})

(defcard "Andromeda: Dispossessed Ristie"
  {:events [{:event :pre-start-game
             :req (req (= side :runner))
             :async true
             :effect (effect (draw eid 4 {:suppress-event true}))}]
   :mulligan (effect (draw eid 4 {:suppress-event true}))})

(defcard "Apex: Invasive Predator"
  (let [ability {:prompt "Choose a card to install facedown"
                 :label "Install a card facedown (start of turn)"
                 :once :per-turn
                 :choices {:max 1
                           :card #(and (runner? %)
                                       (in-hand? %))}
                 :req (req (and (pos? (count (:hand runner)))
                                (:runner-phase-12 @state)))
                 :async true
                 :msg "install a card facedown"
                 :effect (effect
                           (runner-install
                             (assoc eid :source card :source-type :runner-install)
                             target
                             {:facedown true :no-msg true}))}]
    {:implementation "Install restriction not enforced"
     :events [(assoc ability :event :runner-turn-begins)]
     :flags {:runner-phase-12 (req true)}
     :abilities [ability]}))

(defcard "Argus Security: Protection Guaranteed"
  {:events [{:event :agenda-stolen
             :prompt "Choose one"
             :waiting-prompt true
             :async true
             :choices ["Take 1 tag" "Suffer 2 meat damage"]
             :player :runner
             :msg (msg "force the Runner to " (decapitalize target))
             :effect (req (if (= target "Take 1 tag")
                            (gain-tags state :runner eid 1)
                            (damage state :runner eid :meat 2 {:unboostable true :card card})))}]})

(defcard "Armand \"Geist\" Walker: Tech Lord"
  {:events [{:event :runner-trash
             :async true
             :interactive (req true)
             :req (req (and (= side :runner) (= :ability-cost (:cause target))))
             :msg "draw 1 card"
             :effect (effect (draw eid 1))}]})

(defcard "Arissana Rocha Nahu: Street Artist"
  {:abilities [{:req (req (and run
                               (not-used-once? state {:once :per-turn} card)
                               (not (install-locked? state side))))
                :async true
                :label "Install a program from the grip"
                :effect
                (effect
                  (continue-ability
                    {:prompt "Choose a program to install"
                     :waiting-prompt true
                     :choices (req (cancellable
                                     (filter #(and (program? %)
                                                   (runner-can-pay-and-install?
                                                     state side
                                                     (assoc eid :source-type :runner-install) % false))
                                             (:hand runner))))
                     :msg (msg "install " (:title target) " from the grip")
                     :effect (req (wait-for (runner-install state :runner
                                                            (assoc (make-eid state eid) :source card :source-type :runner-install)
                                                            (assoc-in target [:special :street-artist] true) nil)
                                            (register-once state side {:once :per-turn} card)
                                            (register-events
                                              state side card
                                              [{:event :run-ends
                                                :interactive (req true)
                                                :duration :end-of-run
                                                :req (req (some #(get-in % [:special :street-artist]) (all-installed state :runner)))
                                                :effect (req (doseq [program (filter #(get-in % [:special :street-artist]) (all-installed state :runner))]
                                                               (if (has-subtype? program "Trojan")
                                                                 (update! state :runner (dissoc-in program [:special :street-artist]))
                                                                 (do
                                                                   (system-msg state side (str "uses " (:title card) " to trash " (:title program)))
                                                                   (trash-cards state side eid [program] {:cause-card card})))))}])))}
                    card nil))}]})

(defcard "Asa Group: Security Through Vigilance"
  {:events [{:event :corp-install
             :async true
             :req (req (first-event? state :corp :corp-install))
             :effect (req (let [installed-card (:card context)
                                z (butlast (get-zone installed-card))]
                            (continue-ability
                              state side
                              {:prompt "Choose a non-agenda card in HQ to install"
                               :choices {:card #(and (in-hand? %)
                                                     (corp? %)
                                                     (corp-installable-type? %)
                                                     (or (is-remote? z) (not (asset? %)))
                                                     (not (agenda? %)))}
                               :async true
                               :effect (effect (corp-install eid target (zone->name z) nil))}
                              card nil)))}]})

(defcard "Ayla \"Bios\" Rahim: Simulant Specialist"
  {:abilities [{:label "Add 1 hosted card to the grip"
                :cost [:click 1]
                :async true
                :prompt "Choose a hosted card"
                :choices (req (cancellable (:hosted card)))
                :msg "add a hosted card to the grip"
                :effect (effect (move target :hand)
                                (effect-completed eid))}]
   :events [{:event :pre-start-game
             :req (req (= side :runner))
             :async true
             :waiting-prompt true
             :effect (req (doseq [c (take 6 (:deck runner))]
                            (move state side c :play-area))
                          (continue-ability
                            state side
                            {:prompt "Choose 4 cards to be hosted"
                             :choices {:max 4
                                       :all true
                                       :card #(and (runner? %)
                                                   (in-play-area? %))}
                             :effect (req (doseq [c targets]
                                            (host state side (get-card state card) c {:facedown true}))
                                          (doseq [c (get-in @state [:runner :play-area])]
                                            (move state side c :deck))
                                          (shuffle! state side :deck))}
                            card nil))}]})

(defcard "Az McCaffrey: Mechanical Prodigy"
  ;; Effect marks Az's ability as "used" if it has already met it's trigger condition this turn
  (letfn [(az-type? [card] (or (hardware? card)
                               (and (resource? card)
                                    (or (has-subtype? card "Job")
                                        (has-subtype? card "Connection")))))
          (not-triggered? [state] (no-event? state :runner :runner-install #(az-type? (:card (first %)))))]
    {:static-abilities [{:type :install-cost
                         :req (req (and (az-type? target)
                                        (not-triggered? state)))
                         :value -1}]
     :events [{:event :runner-install
               :req (req (and (az-type? (:card context))
                              (not-triggered? state)))
               :silent (req true)
               :msg (msg "reduce the install cost of " (:title (:card context)) " by 1 [Credits]")}]}))

(defcard "Azmari EdTech: Shaping the Future"
  {:events [{:event :corp-turn-ends
             :prompt "Choose a card type"
             :choices ["Event" "Resource" "Program" "Hardware" "None"]
             :effect (req (update! state side (assoc card :card-target (if (= "None" target) nil target)))
                          (if (= "None" target)
                            (system-msg state side (str "declines to use " (:title card)))
                            (system-msg state side (str "uses " (:title card) " to name " target))))}
            {:event :runner-install
             :req (req (and (:card-target card)
                            (is-type? (:card context) (:card-target card))
                            (not (:facedown context))))
             :async true
             :effect (effect (gain-credits :corp eid 2))
             :once :per-turn
             :msg (msg "gain 2 [Credits] from " (:card-target card))}
            {:event :play-event
             :req (req (and (:card-target card)
                            (is-type? (:card context) (:card-target card))))
             :async true
             :effect (effect (gain-credits :corp eid 2))
             :once :per-turn
             :msg (msg "gain 2 [Credits] from " (:card-target card))}]})

(defcard "Blue Sun: Powering the Future"
  {:flags {:corp-phase-12 (req (and (not (:disabled card))
                                    (some rezzed? (all-installed state :corp))))}
   :abilities [{:choices {:card rezzed?}
                :label "Add 1 rezzed card to HQ and gain credits equal to its rez cost"
                :msg (msg "add " (:title target) " to HQ and gain " (rez-cost state side target) " [Credits]")
                :async true
                :effect (effect (move target :hand)
                                (gain-credits eid (rez-cost state side target)))}]})

(defcard "Boris \"Syfr\" Kovac: Crafty Veteran"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-turn-begins
             :req (req (and (has-most-faction? state :runner "Criminal")
                            (pos? (get-in runner [:tag :base]))))
             :msg "remove 1 tag"
             :async true
             :effect (effect (lose-tags eid 1))}]})

(defcard "Captain Padma Isbister: Intrepid Explorer"
  {:events [{:event :run
             :once :per-turn
             :async true
             :req (req (and (= (:server target) [:rd])
                            (first-event? state side :run #(= [:rd] (:server (first %))))))
             :effect (effect (continue-ability (charge-ability state side eid card) card nil))}]})

(defcard "Cerebral Imaging: Infinite Frontiers"
  {:static-abilities [(corp-hand-size+ (req (:credit corp)))]
   :effect (req (swap! state assoc-in [:corp :hand-size :base] 0))
   :leave-play (req (swap! state assoc-in [:corp :hand-size :base] 5))})

(defcard "Chaos Theory: Wünderkind"
  {:static-abilities [(mu+ 1)]})

(defcard "Chronos Protocol: Selective Mind-mapping"
  {:req (req (empty? (filter #(= :net (:damage-type (first %))) (turn-events state :runner :damage))))
   :effect (effect (enable-corp-damage-choice))
   :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-corp))
   :events [{:event :corp-phase-12
             :effect (effect (enable-corp-damage-choice))}
            {:event :runner-phase-12
             :effect (effect (enable-corp-damage-choice))}
            {:event :pre-resolve-damage
             :optional
             {:player :corp
              :req (req (and (= target :net)
                             (corp-can-choose-damage? state)
                             (pos? (last targets))
                             (empty? (filter #(= :net (:damage-type (first %))) (turn-events state :runner :damage)))
                             (pos? (count (:hand runner)))))
              :waiting-prompt true
              :prompt "Choose the first card to trash?"
              :yes-ability
              {:async true
               :msg (msg "look at the grip ( "
                         (enumerate-str (map :title (sort-by :title (:hand runner))))
                         " ) and choose the card that is trashed")
               :effect
               (effect (continue-ability
                         {:prompt "Choose 1 card to trash"
                          :choices (req (:hand runner))
                          :not-distinct true
                          :msg (msg "choose " (:title target) " to trash")
                          :effect (req (chosen-damage state :corp target))}
                         card nil))}
              :no-ability
              {:effect (req (system-msg state :corp (str "declines to use " (:title card))))}}}]})

(defcard "Cybernetics Division: Humanity Upgraded"
  {:static-abilities [(hand-size+ -1)]})

(defcard "Earth Station: SEA Headquarters"
  (let [flip-effect (effect (update! (if (:flipped card)
                                       (do (system-msg state :corp "flipped their identity to Earth Station: SEA Headquarters")
                                           (assoc card
                                                  :flipped false
                                                  :face :front
                                                  :code (subs (:code card) 0 5)))
                                       (assoc card
                                              :flipped true
                                              :face :back
                                              :code (str (subs (:code card) 0 5) "flip")))))]
    {:events [{:event :pre-first-turn
               :req (req (= side :corp))
               :effect (effect (update! (assoc card :flipped false :face :front)))}
              {:event :successful-run
               :req (req (and (= :hq (target-server context))
                              (:flipped card)))
               :effect flip-effect}]
     :static-abilities [{:type :run-additional-cost
                         :req (req (or
                                     (and (not (:flipped card))
                                          (= :hq (:server (second targets))))
                                     (and (:flipped card)
                                          (in-coll? (keys (get-remotes state)) (:server (second targets))))))
                         :value (req [:credit (if (:flipped card) 6 1)])}]
     :async true
     ; This effect will be resolved when the ID is reenabled after Strike / Direct Access
     :effect (effect
               (continue-ability
                 {:req (req (< 1 (count (get-remotes state))))
                  :prompt "Choose a server to be saved from the rules apocalypse"
                  :choices (req (get-remote-names state))
                  :async true
                  :effect (req (let [to-be-trashed (remove #(in-coll? ["Archives" "R&D" "HQ" target] (zone->name (second (get-zone %))))
                                                           (all-installed state :corp))]
                                 (system-msg state side (str "chooses " target
                                                             " to be saved from the rules apocalypse and trashes "
                                                             (quantify (count to-be-trashed) "card")))
                                 ; these cards get trashed by the game and not by players
                                 (trash-cards state side eid to-be-trashed {:unpreventable true :game-trash true})))}
                 card nil))
     :abilities [{:label "Flip identity to Earth Station: Ascending to Orbit"
                  :req (req (not (:flipped card)))
                  :cost [:click 1]
                  :msg "flip their identity to Earth Station: Ascending to Orbit"
                  :effect flip-effect}
                 {:label "Manually flip identity to Earth Station: SEA Headquarters"
                  :req (req (:flipped card))
                  :effect flip-effect}]}))

(defcard "Edward Kim: Humanity's Hammer"
  {:events [{:event :access
             :once :per-turn
             :req (req (and (operation? target)
                            (first-event? state side :access #(operation? (first %)))))
             :async true
             :effect (req (if (in-discard? target)
                            (effect-completed state side eid)
                            (do (when run
                                  (swap! state assoc-in [:run :did-trash] true))
                                (swap! state assoc-in [:runner :register :trashed-card] true)
                                (system-msg state :runner
                                            (str "uses " (:title card) " to"
                                                 " trash " (:title target)
                                                 " at no cost"))
                                (trash state side eid target nil))))}]})

(defcard "Ele \"Smoke\" Scovak: Cynosure of the Net"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Epiphany Analytica: Nations Undivided"
  (let [valid-trash (fn [target] (corp? (:card target)))
        ability {:once :per-turn
                 :msg "place 1 power counter on itself"
                 :effect (req (add-counter state side card :power 1))}]
  {:events [(assoc ability :event :runner-trash :req (req (valid-trash target)))
            (assoc ability :event :agenda-stolen :req (req true))]
   :abilities [{:label "Look at the top 3 cards of R&D"
                :cost [:power 1 :click 1]
                :msg "look at the top 3 cards of R&D"
                :effect (req (let [top (take 3 (:deck corp))]
                               (wait-for (resolve-ability state side
                                                          {:async true
                                                           :waiting-prompt true
                                                           :prompt (msg "The top cards of R&D are (top->bottom): " (enumerate-str (map :title top)))
                                                           :choices ["OK"]}
                                                          card nil)
                                         (continue-ability
                                           state :corp
                                           {:prompt "Choose a card to install"
                                            :waiting-prompt true
                                            :not-distinct true
                                            :choices (cancellable (filter #(corp-installable-type? %) top))
                                            :async true
                                            :cancel-effect
                                            (effect (system-msg (str "declines to use " (get-title card) " to install a card from the top of R&D"))
                                                    (effect-completed eid))
                                            :msg (msg "install the "
                                                      (pprint/cl-format nil "~:R"
                                                        (inc (first (keep-indexed #(when (same-card? target %2) %1) top))))
                                                      " card from the top of R&D")
                                            :effect (effect (corp-install eid target nil))}
                                           card nil))))}]}))

(defcard "Esâ Afontov: Eco-Insurrectionist"
  (letfn
    [(check-brain [targets]
       (let [context (first targets)]
         (and (pos? (:amount context))
              (= :brain (:damage-type context)))))]
    {:events [{:event :damage
               :req (req (and (check-brain targets)
                              (first-event? state :runner :damage check-brain)))
               :async true
               :effect (req
                         (wait-for (resolve-ability
                                     state side
                                     {:optional
                                      {:prompt "Draw 1 card?"
                                       :autoresolve (get-autoresolve :auto-fire)
                                       :yes-ability {:async true
                                                     :msg "draw 1 card"
                                                     :effect (effect (draw eid 1))}}}
                                     card nil)
                                   (continue-ability
                                     state side
                                     (sabotage-ability 2)
                                     card nil)))}]
     :abilities [(set-autoresolve :auto-fire "Esâ Afontov: Eco-Insurrectionist drawing cards")]}))

(defcard "Exile: Streethawk"
  {:flags {:runner-install-draw true}
   :events [{:event :runner-install
             :async true
             :req (req (and (program? (:card context))
                            (some #{:discard} (:previous-zone (:card context)))))
             :msg "draw 1 card"
             :effect (effect (draw eid 1))}]})

(defcard "Freedom Khumalo: Crypto-Anarchist"
  {:interactions
   {:access-ability
    {:async true
     :once :per-turn
     :label "Trash card"
     :req (req (and (not (:disabled card))
                    (not (agenda? target))
                    (not (in-discard? target))
                    (<= (play-cost state side target)
                        (number-of-runner-virus-counters state))))
     :waiting-prompt true
     :effect (req (let [accessed-card target
                        play-or-rez (:cost target)]
                    (if (zero? play-or-rez)
                      (continue-ability
                        state side
                        {:async true
                         :msg (msg "trash " (:title accessed-card) " at no cost")
                         :effect (effect (trash eid (assoc accessed-card :seen true) {:accessed true}))}
                        card nil)
                      (wait-for (resolve-ability state side (pick-virus-counters-to-spend play-or-rez) card nil)
                                (if-let [msg (:msg async-result)]
                                  (do (system-msg state :runner
                                                  (str "uses " (:title card) " to"
                                                       " trash " (:title accessed-card)
                                                       " at no cost, spending " msg))
                                      (trash state side eid (assoc accessed-card :seen true) {:accessed true}))
                                  ;; Player cancelled ability
                                  (do (swap! state dissoc-in [:per-turn (:cid card)])
                                      (access-non-agenda state side eid accessed-card :skip-trigger-event true)))))))}}})

(defcard "Fringe Applications: Tomorrow, Today"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-turn-begins
             :player :corp
             :req (req (and (not (:disabled card))
                            (has-most-faction? state :corp "Weyland Consortium")
                            (some ice? (all-installed state side))))
             :prompt "Choose a piece of ice to place 1 advancement token on"
             :choices {:card #(and (installed? %)
                                   (ice? %))}
             :msg (msg "place 1 advancement token on " (card-str state target))
             :effect (req (add-prop state :corp target :advance-counter 1 {:placed true}))}]})

(defcard "Gabriel Santiago: Consummate Professional"
  {:events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            (first-successful-run-on-server? state :hq)))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits eid 2))}]})

(defcard "Gagarin Deep Space: Expanding the Horizon"
  {:events [{:event :pre-access-card
             :req (req (is-remote? (second (get-zone target))))
             :effect (effect (access-cost-bonus [:credit 1]))
             :msg "make the Runner spend 1 [Credits] to access"}]})

(defcard "GameNET: Where Dreams are Real"
  (let [gamenet-ability {:req (req (and (:run @state)
                                        (= (:side (:source eid)) "Corp")
                                        (or (and (not= (:source-type eid) :runner-trash-corp-cards)
                                                 (not= (:source-type eid) :runner-steal))
                                            (some (fn [[cost source]] (and (some #(or (= (cost-name %) :credit) (= (cost-name %) :x-credits)) (merge-costs cost))
                                                                           (= (:side (:source source)) "Corp")))
                                                  (:additional-costs eid)))))
                         :async true
                         :msg "gain 1 [Credits]"
                         :effect (effect (gain-credits :corp eid 1))}]
    {:events [(assoc gamenet-ability :event :runner-credit-loss)
              (assoc gamenet-ability :event :runner-spent-credits)]}))

(defcard "GRNDL: Power Unleashed"
  {:events [{:event :pre-start-game
             :req (req (= :corp side))
             :async true
             :effect (req (wait-for (gain-credits state :corp 5)
                                    (if (zero? (count-bad-pub state))
                                      (gain-bad-publicity state :corp eid 1)
                                      (effect-completed state side eid))))}]})

(defcard "Haarpsichord Studios: Entertainment Unleashed"
  {:static-abilities [{:type :cannot-steal
                       :value (req (pos? (event-count state side :agenda-stolen)))}]
   :events [{:event :access
             :req (req (and (agenda? target)
                            (pos? (event-count state side :agenda-stolen))))
             :effect (effect (toast :runner "Cannot steal due to Haarpsichord Studios." "warning"))}]})

(defcard "Haas-Bioroid: Architects of Tomorrow"
  {:events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (has-subtype? (:ice context) "Bioroid")
                            (first-event? state :runner :pass-ice
                                          (fn [targets]
                                            (let [context (first targets)
                                                  ice  (:ice context)]
                                              (and (rezzed? ice)
                                                   (installed? ice)
                                                   (has-subtype? ice "Bioroid")))))))
             :waiting-prompt true
             :prompt "Choose a Bioroid to rez"
             :player :corp
             :choices
             {:req (req (and (has-subtype? target "Bioroid")
                             (not (rezzed? target))
                             (can-pay? state side (assoc eid :source card :source-type :rez) target nil
                                       [:credit (rez-cost state side target {:cost-bonus -4})])))}
             :msg (msg "rez " (:title target))
             :async true
             :effect (effect (rez eid target {:cost-bonus -4}))}]})

(defcard "Haas-Bioroid: Engineering the Future"
  {:events [{:event :corp-install
             :req (req (first-event? state corp :corp-install))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Haas-Bioroid: Precision Design"
  {:static-abilities [(corp-hand-size+ 1)]
   :events [{:event :agenda-scored
             :interactive (req true)
             :optional {:prompt "Add 1 card from Archives to HQ?"
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability (corp-recur)}}]
   :abilities [(set-autoresolve :auto-fire "Haas-Bioroid: Precision Design")]})

(defcard "Haas-Bioroid: Stronger Together"
  {:static-abilities [{:type :ice-strength
                       :req (req (has-subtype? target "Bioroid"))
                       :value 1}]
   :leave-play (effect (update-all-ice))
   :effect (effect (update-all-ice))})

(defcard "Harishchandra Ent.: Where You're the Star"
  (letfn [(format-grip [runner]
            (if (pos? (count (:hand runner)))
              (enumerate-str (map :title (sort-by :title (:hand runner))))
              "no cards"))]
    {:events [{:event :post-runner-draw
               :req (req (is-tagged? state))
               :msg (msg "see that the Runner drew: "
                         (enumerate-str (map :title runner-currently-drawing)))}
              {:event :tags-changed
               :effect (req (if (is-tagged? state)
                              (when-not (get-in @state [:runner :openhand])
                                (system-msg state :corp (str "uses " (get-title card) " make the Runner play with their grip revealed"))
                                (system-msg state :corp (str "uses " (get-title card) " to see that the Runner currently has "
                                                             (format-grip runner) " in their grip"))
                              (reveal-hand state :runner))
                            (when (get-in @state [:runner :openhand])
                              (system-msg state :corp (str "uses " (get-title card) " stop making the Runner play with their grip revealed"))
                              (system-msg state :corp (str "uses " (get-title card) " to see that the Runner had "
                                                           (format-grip runner) " in their grip before it was concealed"))
                              (conceal-hand state :runner))))}]
   :effect (req (when (is-tagged? state)
                  (reveal-hand state :runner)))
   :leave-play (req (when (is-tagged? state)
                      (conceal-hand state :runner)))}))

(defcard "Harmony Medtech: Biomedical Pioneer"
  {:effect (effect (lose :agenda-point-req 1)
                   (lose :runner :agenda-point-req 1))
   :leave-play (effect (gain :agenda-point-req 1)
                       (gain :runner :agenda-point-req 1))})

(defcard "Hayley Kaplan: Universal Scholar"
  {:events [{:event :runner-install
             :silent (req (not (and (first-event? state side :runner-install)
                                    (some #(is-type? % (:type (:card context))) (:hand runner)))))
             :req (req (and (first-event? state side :runner-install)
                            (not (:facedown context))))
             :once :per-turn
             :async true
             :waiting-prompt true
             :effect
             (effect (continue-ability
                       (let [itarget (:card context)
                             card-type (:type itarget)]
                         (if (some #(is-type? % (:type itarget)) (:hand runner))
                           {:optional
                            {:prompt (str "Install another " card-type " from the grip?")
                             :yes-ability
                             {:prompt (str "Choose a " card-type " to install")
                              :choices {:card #(and (is-type? % card-type)
                                                    (in-hand? %))}
                              :msg (msg "install " (:title target))
                              :async true
                              :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}}}
                           {:prompt (str "You have no " card-type " in hand")
                            :choices ["Carry on!"]
                            :prompt-type :bogus}))
                       card nil))}]})

(defcard "Hoshiko Shiro: Untold Protagonist"
  (let [flip-effect (req (update! state side (if (:flipped card)
                                               (assoc card
                                                      :flipped false
                                                      :face :front
                                                      :code (subs (:code card) 0 5)
                                                      :subtype "Natural")
                                               (assoc card
                                                      :flipped true
                                                      :face :back
                                                      :code (str (subs (:code card) 0 5) "flip")
                                                      :subtype "Digital")))
                         (update-link state))]
    {:static-abilities [(link+ (req (:flipped card)) 1)
                        {:type :gain-subtype
                         :req (req (and (same-card? card target) (:flipped card)))
                         :value "Digital"}
                        {:type :lose-subtype
                         :req (req (and (same-card? card target) (:flipped card)))
                         :value "Natural"}]
     :events [{:event :pre-first-turn
               :req (req (= side :runner))
               :effect (effect (update! (assoc card :flipped false :face :front)))}
              {:event :runner-turn-ends
               :interactive (req true)
               :async true
               :effect (req (cond
                              (and (:flipped card)
                                   (not (:accessed-cards runner-reg)))
                              (do (system-msg state :runner "flips their identity to Hoshiko Shiro: Untold Protagonist")
                                  (continue-ability state :runner {:effect flip-effect} card nil))

                              (and (not (:flipped card))
                                   (:accessed-cards runner-reg))
                              (wait-for (gain-credits state :runner 2)
                                        (system-msg state :runner "gains 2 [Credits] and flips their identity to Hoshiko Shiro: Mahou Shoujo")
                                        (continue-ability state :runner {:effect flip-effect} card nil))

                              :else
                              (effect-completed state side eid)))}
              {:event :runner-turn-begins
               :req (req (:flipped card))
               :async true
               :effect (req (wait-for (draw state :runner 1)
                                      (wait-for (lose-credits state :runner (make-eid state eid) 1)
                                                (system-msg state :runner (str "uses " (:title card) " to draw 1 card and lose 1 [Credits]"))
                                                (effect-completed state side eid))))}]
     :abilities [{:label "flip identity"
                  :msg "flip their identity manually"
                  :effect flip-effect}]}))

(defcard "Hyoubu Institute: Absolute Clarity"
  {:events [{:event :corp-reveal
             :once :per-turn
             :req (req (first-event? state side :corp-reveal #(pos? (count %))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]
   :abilities [{:cost [:click 1]
                :label "Reveal the top card of the Stack"
                :async true
                :effect (req (if-let [revealed-card (-> runner :deck first)]
                               (do (system-msg state side (str "uses " (:title card) " to reveal "
                                                               (:title revealed-card) " from the top of the Stack"))
                                   (reveal state side eid revealed-card))
                               (effect-completed state side eid)))}
               {:cost [:click 1]
                :label "Reveal a random card from the Grip"
                :async true
                :effect (req (if-let [revealed-card (-> runner :hand shuffle first)]
                               (do (system-msg state side (str "uses " (:title card) " to reveal "
                                                               (:title revealed-card) " from the Grip"))
                                   (reveal state side eid revealed-card))
                               (effect-completed state side eid)))}]})

(defcard "Iain Stirling: Retired Spook"
  (let [ability {:req (req (> (:agenda-point corp) (:agenda-point runner)))
                 :once :per-turn
                 :msg "gain 2 [Credits]"
                 :async true
                 :effect (effect (gain-credits eid 2))}]
    {:flags {:drip-economy true}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Industrial Genomics: Growing Solutions"
  {:static-abilities [{:type :trash-cost
                       :value (req (count (remove :seen (:discard corp))))}]})

(defcard "Information Dynamics: All You Need To Know"
  {:events (let [inf {:req (req (and (not (:disabled card))
                                     (has-most-faction? state :corp "NBN")))
                      :msg "give the Runner 1 tag"
                      :async true
                      :effect (effect (gain-tags :corp eid 1))}]
             [{:event :pre-start-game
               :effect draft-points-target}
              (assoc inf :event :agenda-scored)
              (assoc inf :event :agenda-stolen)])})

(defcard "Issuaq Adaptics: Sustaining Diversity"
  {:effect (effect (lose :agenda-point-req (get-counters card :power)))
   :leave-play (effect (gain :agenda-point-req (get-counters card :power)))
   :events [{:event :agenda-scored
             :interactive (req true)
             :req (req (and (->> (turn-events state side :corp-install)
                                 (map #(:card (first %)))
                                 (filter #(same-card? (:card context) %))
                                 empty?)
                            (->> (turn-events state side :advance)
                                 (map #(first %))
                                 (filter #(same-card? (:card context) %))
                                 empty?)))
             :msg "put 1 charge counter on itself"
             :effect (req (add-counter state side card :power 1)
                          (swap! state assoc-in [:corp :agenda-point-req]
                                 (dec (get-in @state [:corp :agenda-point-req])))
                          (check-win-by-agenda state))}]})

(defcard "Jamie \"Bzzz\" Micken: Techno Savant"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-install
             :req (req (and (has-most-faction? state :runner "Shaper")
                            (first-event? state side :runner-install)))
             :msg "draw 1 card"
             :once :per-turn
             :async true
             :effect (effect (draw eid 1))}]})

(defcard "Jemison Astronautics: Sacrifice. Audacity. Success."
  {:events [{:event :corp-forfeit-agenda
             :async true
             :waiting-prompt true
             :effect
             (effect
               (continue-ability
                 (let [p (inc (get-agenda-points (:card context)))]
                   {:prompt "Choose a card to place advancement tokens on"
                    :choices {:card #(and (installed? %)
                                          (corp? %))}
                    :msg (msg "place " (quantify p "advancement token")
                              " on " (card-str state target))
                    :effect (effect (add-prop :corp target :advance-counter p {:placed true}))})
                 card nil))}]})

(defcard "Jesminder Sareen: Girl Behind the Curtain"
  {:flags {:forced-to-avoid-tag true}
   :events [{:event :pre-tag
             :async true
             :once :per-run
             :req (req (:run @state))
             :msg "avoid the first tag during this run"
             :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "Jinteki Biotech: Life Imagined"
  {:events [{:event :pre-first-turn
             :req (req (= side :corp))
             :prompt (msg "Choose a copy of " (:title card) " to use this game")
             :choices ["The Brewery" "The Tank" "The Greenhouse"]
             :effect (effect (update! (assoc card :biotech-target target :face :front))
                             (system-msg (str "has chosen a copy of " (:title card) " for this game")))}]
   :abilities [{:label "Check chosen flip identity"
                :effect (req (case (:biotech-target card)
                               "The Brewery"
                               (toast state :corp "Flip to: The Brewery (Do 2 net damage)" "info")
                               "The Tank"
                               (toast state :corp "Flip to: The Tank (Shuffle Archives into R&D)" "info")
                               "The Greenhouse"
                               (toast state :corp "Flip to: The Greenhouse (Place 4 advancement tokens on a card)" "info")
                               ;; default case
                               (toast state :corp "No flip identity specified" "info")))}
               {:cost [:click 3]
                :req (req (not (:biotech-used card)))
                :label "Flip this identity"
                :async true
                :effect (req (let [flip (:biotech-target card)]
                               (update! state side (assoc (get-card state card) :biotech-used true))
                               (case flip
                                 "The Brewery"
                                 (do (system-msg state side (str "uses " flip " to do 2 net damage"))
                                     (update! state side (assoc card :code "brewery" :face :brewery))
                                     (damage state side eid :net 2 {:card card}))
                                 "The Tank"
                                 (do (system-msg state side (str "uses " flip " to shuffle Archives into R&D"))
                                     (shuffle-into-deck state side :discard)
                                     (update! state side (assoc card :code "tank" :face :tank))
                                     (effect-completed state side eid))
                                 "The Greenhouse"
                                 (do (system-msg state side (str "uses " flip " to place 4 advancement counters "
                                                                 "on a card that can be advanced"))
                                     (update! state side (assoc card :code "greenhouse" :face :greenhouse))
                                     (continue-ability
                                       state side
                                       {:prompt "Choose a card that can be advanced"
                                        :choices {:card can-be-advanced?}
                                        :effect (effect (add-prop target :advance-counter 4 {:placed true}))}
                                       card nil))
                                 (toast state :corp (str "Unknown Jinteki Biotech: Life Imagined card: " flip) "error"))))}]})

(defcard "Jinteki: Personal Evolution"
  (let [ability {:async true
                 :req (req (not (:winner @state)))
                 :msg "do 1 net damage"
                 :effect (effect (damage eid :net 1 {:card card}))}]
    {:events [(assoc ability
                     :event :agenda-scored
                     :interactive (req true))
              (assoc ability :event :agenda-stolen)]}))

(defcard "Jinteki: Potential Unleashed"
  {:events [{:async true
             :event :pre-resolve-damage
             :req (req (and (-> @state :corp :disable-id not)
                            (= target :net)
                            (pos? (last targets))))
             :effect (req (let [c (first (get-in @state [:runner :deck]))]
                            (system-msg state :corp (str "uses " (:title card) " to trash " (:title c)
                                                         " from the top of the stack"))
                            (mill state :corp eid :runner 1)))}]})

(defcard "Jinteki: Replicating Perfection"
  {:static-abilities [{:type :cannot-run-on-server
                       :req (req (no-event? state side :run #(is-central? (:server (first %)))))
                       :value (req (map first (get-remotes state)))}]})

(defcard "Jinteki: Restoring Humanity"
  {:events [{:event :corp-turn-ends
             :req (req (pos? (count (remove :seen (:discard corp)))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Kabonesa Wu: Netspace Thrillseeker"
  {:abilities [{:label "Install a non-virus program from the stack, lowering the cost by 1 [Credit]"
                :cost [:click 1]
                :prompt "Choose a program"
                :choices (req (cancellable
                                (filter #(and (program? %)
                                              (not (has-subtype? % "Virus"))
                                              (can-pay? state :runner (assoc eid :source card :source-type :runner-install) % nil
                                                        [:credit (install-cost state side % {:cost-bonus -1})]))
                                        (:deck runner))))
                :msg (msg "install " (:title target) " from the stack, lowering the cost by 1 [Credit]")
                :async true
                :effect (effect (trigger-event :searched-stack nil)
                                (shuffle! :deck)
                                (register-events
                                  card
                                  [{:event :runner-turn-ends
                                    :interactive (req true)
                                    :duration :end-of-turn
                                    :req (req (some #(get-in % [:special :kabonesa]) (all-installed state :runner)))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (req (doseq [program (filter #(get-in % [:special :kabonesa]) (all-installed state :runner))]
                                                   (move state side program :rfg)))}])
                                (runner-install (assoc eid :source card :source-type :runner-install)
                                                (assoc-in target [:special :kabonesa] true)
                                                {:cost-bonus -1}))}]})

(defcard "Kate \"Mac\" McCaffrey: Digital Tinker"
  ;; Effect marks Kate's ability as "used" if it has already met it's trigger condition this turn
  (letfn [(kate-type? [card] (or (hardware? card)
                                 (program? card)))
          (not-triggered? [state] (no-event? state :runner :runner-install #(kate-type? (:card (first %)))))]
    {:static-abilities [{:type :install-cost
                         :req (req (and (kate-type? target)
                                        (not-triggered? state)))
                         :value -1}]
     :events [{:event :runner-install
               :req (req (and (kate-type? (:card context))
                              (not-triggered? state)))
               :silent (req true)
               :msg (msg "reduce the install cost of " (:title (:card context)) " by 1 [Credits]")}]}))

(defcard "Ken \"Express\" Tenma: Disappeared Clone"
  {:events [{:event :play-event
             :req (req (and (has-subtype? (:card context) "Run")
                            (first-event? state :runner :play-event #(has-subtype? (:card (first %)) "Run"))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Khan: Savvy Skiptracer"
  {:events [{:event :pass-ice
             :req (req (first-event? state :runner :pass-ice))
             :async true
             :interactive (req true)
             :effect (effect
                       (continue-ability
                         (when (some #(and (has-subtype? % "Icebreaker")
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [:credit (install-cost state side % {:cost-bonus -1})]))
                                     (:hand runner))
                           {:prompt "Choose an icebreaker to install"
                            :choices
                            {:req (req (and (in-hand? target)
                                            (has-subtype? target "Icebreaker")
                                            (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                      [:credit (install-cost state side target {:cost-bonus -1})])))}
                            :async true
                            :msg (msg "install " (:title target) " from the grip, lowering the cost by 1 [Credits]")
                            :effect (effect (runner-install eid target {:cost-bonus -1}))})
                         card nil))}]})

(defcard "Laramy Fisk: Savvy Investor"
  {:events [{:event :successful-run
             :async true
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional
             {:req (req (and (is-central? (:server context))
                             (first-event? state side :successful-run
                                           (fn [targets]
                                             (let [context (first targets)]
                                               (is-central? (:server context)))))))
              :autoresolve (get-autoresolve :auto-fire)
              :prompt "Force the Corp to draw 1 card?"
              :yes-ability {:msg "force the Corp to draw 1 card"
                            :async true
                            :effect (effect (draw :corp eid 1))}
              :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
   :abilities [(set-autoresolve :auto-fire "Laramy Fisk: Savvy Investor")]})

(defcard "Lat: Ethical Freelancer"
  {:events [{:event :runner-turn-ends
             :interactive (req true)
             :effect
             (effect (continue-ability 
                       {:optional {:req (req (= (count (:hand runner)) (count (:hand corp))))
                        :autoresolve (get-autoresolve :auto-fire)
                        :prompt "Draw 1 card?"
                        :yes-ability {:async true
                                      :msg "draw 1 card"
                                      :effect (effect (draw :runner eid 1))}
                        :no-ability
                        {:effect (effect (system-msg (str "declines to use " (:title card))))}}}
                       card nil))}]
   :abilities [(set-autoresolve :auto-fire "Lat: Ethical Freelancer")]})

(defcard "Leela Patel: Trained Pragmatist"
  (let [leela {:interactive (req true)
               :prompt "Choose an unrezzed card to return to HQ"
               :choices {:card #(and (not (faceup? %))
                                     (installed? %)
                                     (corp? %))}
               :msg (msg "add " (card-str state target) " to HQ")
               :effect (effect (move :corp target :hand))}]
    {:events [(assoc leela :event :agenda-scored)
              (assoc leela :event :agenda-stolen)]}))

(defcard "Liza Talking Thunder: Prominent Legislator"
  {:events [{:event :successful-run
             :async true
             :interactive (req true)
             :msg "draw 2 cards and take 1 tag"
             :req (req (and (is-central? (:server context))
                            (first-event? state side :successful-run
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (is-central? (:server context)))))))
             :effect (req (wait-for (gain-tags state :runner 1)
                                    (draw state :runner eid 2)))}]})

(defcard "Los: Data Hijacker"
  {:events [{:event :rez
             :once :per-turn
             :req (req (ice? (:card context)))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 2))}]})

(defcard "MaxX: Maximum Punk Rock"
  (let [ability {:msg (msg (let [deck (:deck runner)]
                             (if (pos? (count deck))
                               (str "trash " (enumerate-str (map :title (take 2 deck))) " from the stack and draw 1 card")
                               "trash the top 2 cards from the stack and draw 1 card - but the stack is empty")))
                 :label "trash and draw cards"
                 :once :per-turn
                 :async true
                 :effect (req (wait-for (mill state :runner :runner 2)
                                        (draw state :runner eid 1)))}]
    {:flags {:runner-turn-draw true
             :runner-phase-12 (req (and (not (:disabled card))
                                        (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner))))}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Mercury: Chrome Libertador"
  {:events [{:event :breach-server
             :req (req (and run
                            (empty? (run-events state side :subroutines-broken))
                            (#{:hq :rd} target)))
             :async true
             :effect (req (let [breached-server target]
                            (continue-ability
                              state side
                              {:optional
                               {:prompt "Access 1 additional card?"
                                :waiting-prompt true
                                :once :per-turn
                                :yes-ability
                                {:msg (msg "access 1 additional card")
                                 :async true
                                 :effect (req (access-bonus state side breached-server 1 :end-of-access)
                                              (effect-completed state side eid))}
                                :no-ability {:effect (effect (system-msg (str "declines to use " (:title card) " to access 1 additional card")))}}}
                              card nil)))}]})

(defcard "MirrorMorph: Endless Iteration"
  (let [mm-clear {:prompt "Manually fix Mirrormorph"
                  :msg "manually clear Mirrormorph flags"
                  :label "Manually fix Mirrormorph"
                  :effect (effect
                           (update! (assoc-in card [:special :mm-actions] []))
                           (update! (assoc-in (get-card state card) [:special :mm-click] false)))}
        mm-ability {:prompt "Choose one"
                    :choices ["Gain [Click]" "Gain 1 [Credits]"]
                    :msg (msg (decapitalize target))
                    :once :per-turn
                    :label "Manually trigger ability"
                    :async true
                    :effect (req (if (= "Gain [Click]" target)
                                   (do (gain-clicks state side 1)
                                       (update! state side (assoc-in (get-card state card) [:special :mm-click] true))
                                       (effect-completed state side eid))
                                   (gain-credits state side eid 1)))}]
    {:implementation "Does not work with terminal Operations"
     :abilities [mm-ability mm-clear]
     :events [{:event :corp-spent-click
               :async true
               :effect (req (let [cid (first target)
                                  ability-idx (:ability-idx (:source-info eid))
                                  bac-cid (get-in @state [:corp :basic-action-card :cid])
                                  cause (if (keyword? (first target))
                                          (case (first target)
                                            :play-instant [bac-cid 3]
                                            :corp-click-install [bac-cid 2]
                                            (first target)) ; in clojure there's: (= [1 2 3] '(1 2 3))
                                          [cid ability-idx])
                                  prev-actions (get-in card [:special :mm-actions] [])
                                  actions (conj prev-actions cause)]
                              (update! state side (assoc-in card [:special :mm-actions] actions))
                              (update! state side (assoc-in (get-card state card) [:special :mm-click] false))
                              (if (and (= 3 (count actions))
                                       (= 3 (count (distinct actions))))
                                (continue-ability state side mm-ability (get-card state card) nil)
                                (effect-completed state side eid))))}
              {:event :runner-turn-begins
               :effect (effect
                        (update! (assoc-in card [:special :mm-actions] []))
                        (update! (assoc-in (get-card state card) [:special :mm-click] false)))}
              {:event :corp-turn-ends
               :effect (effect
                        (update! (assoc-in card [:special :mm-actions] []))
                        (update! (assoc-in (get-card state card) [:special :mm-click] false)))}]
     :static-abilities [{:type :prevent-paid-ability
                         :req (req (and (get-in card [:special :mm-click])
                                        (let [cid (:cid target)
                                              ability-idx (nth targets 2 nil)
                                              cause [cid ability-idx]
                                              prev-actions (get-in card [:special :mm-actions] [])
                                              actions (conj prev-actions cause)]
                                          (not (and (= 4 (count actions))
                                                    (= 4 (count (distinct actions))))))))
                         :value true}]}))

(defcard "Mti Mwekundu: Life Improved"
  {:events [{:event :approach-server
             :async true
             :interactive (req true)
             :waiting "Corp to make a decision"
             :req (req (and (pos? (count (:hand corp)))
                            (not (used-this-turn? (:cid card) state))))
             :effect (req (if (some ice? (:hand corp))
                            (continue-ability
                             state side
                             {:optional
                              {:prompt "Install a piece of ice?"
                               :once :per-turn
                               :yes-ability
                               {:prompt "Choose a piece of ice to install from HQ"
                                :choices {:card #(and (ice? %)
                                                      (in-hand? %))}
                                :async true
                                :msg "install a piece of ice at the innermost position of this server. Runner is now approaching that piece of ice"
                                :effect (req (wait-for (corp-install state side target (zone->name (target-server run))
                                                                     {:ignore-all-cost true
                                                                      :front true})
                                                       (swap! state assoc-in [:run :position] 1)
                                                       (set-next-phase state :approach-ice)
                                                       (update-all-ice state side)
                                                       (update-all-icebreakers state side)
                                                       (continue-ability state side
                                                                         (offer-jack-out {:req (req (:approached-ice? (:run @state)))})
                                                                         card nil)))}}}
                             card nil)
                            ;; bogus prompt so Runner cannot infer the Corp has no ice in hand
                            (continue-ability
                             state :corp
                             {:async true
                              :prompt "No ice to install"
                              :choices ["Carry on!"]
                              :prompt-type :bogus
                              :effect (effect (effect-completed eid))}
                             card nil)))}]})

(defcard "Nasir Meidan: Cyber Explorer"
  {:events [{:event :approach-ice
             :req (req (not (rezzed? (:ice context))))
             :effect (effect
                       (register-events
                         card
                         (let [ice (:ice context)
                               cost (rez-cost state side ice)]
                           [{:event :encounter-ice
                             :duration :end-of-encounter
                             :unregister-once-resolved true
                             :req (req (same-card? (:ice context) ice))
                             :msg (msg "lose all credits and gain " cost
                                       " [Credits] from the rez of " (:title ice))
                             :async true
                             :effect (req (wait-for (lose-credits state :runner (make-eid state eid) :all)
                                                    (gain-credits state :runner eid cost)))}])))}]})

(defcard "Nathaniel \"Gnat\" Hall: One-of-a-Kind"
  (let [ability {:label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :interactive (req true)
                 :async true
                 :effect (req (if (and (> 3 (count (:hand runner)))
                                       (:runner-phase-12 @state))
                                (do (system-msg state :runner (str "uses " (:title card) " to gain 1 [Credits]"))
                                    (gain-credits state :runner eid 1))
                                (effect-completed state side eid)))}]
    {:flags {:drip-economy true
             :runner-phase-12 (req (and (not (:disabled card))
                                        (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner))))}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)]}))

(defcard "NBN: Controlling the Message"
  {:events [{:event :runner-trash
             :interactive (req true)
             :once-per-instance true
             :optional
             {:player :corp
              :req (req (and (some #(and (corp? (:card %))
                                         (installed? (:card %)))
                                   targets)
                             (first-event? state side :runner-trash
                                           (fn [targets]
                                             (some #(and (installed? (:card %))
                                                         (corp? (:card %)))
                                                   targets)))))
              :waiting-prompt true
              :prompt "Initiate a trace with strength 4?"
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability
              {:trace {:base 4
                       :successful
                       {:msg "give the Runner 1 tag"
                        :async true
                        :effect (effect (gain-tags :corp eid 1 {:unpreventable true}))}}}}}]
   :abilities [(set-autoresolve :auto-fire "NBN: Controlling the Message")]})

(defcard "NBN: Making News"
  {:recurring 2
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "NBN: Reality Plus"
  {:events [{:event :runner-gain-tag
             :req (req (first-event? state :runner :runner-gain-tag))
             :player :corp
             :async true
             :waiting-prompt true
             :prompt "Choose one"
             :choices ["Gain 2 [Credits]" "Draw 2 cards"]
             :msg (msg (decapitalize target))
             :effect (req
                       (if (= target "Gain 2 [Credits]")
                            (gain-credits state :corp eid 2)
                            (draw state :corp eid 2)))}]})

(defcard "NBN: The World is Yours*"
  {:static-abilities [(corp-hand-size+ 1)]})

(defcard "Near-Earth Hub: Broadcast Center"
  {:events [{:event :server-created
             :req (req (first-event? state :corp :server-created))
             :async true
             :msg "draw 1 card"
             :effect (req
                      (if-not (some #(= % :deck) (:zone target))
                        (draw state :corp eid 1)
                        (do
                          ;; Register the draw to go off when the card is finished installing -
                          ;;  this is after the checkpoint when it should go off, but is needed to
                          ;;  fix the interaction between architect (and any future install from R&D
                          ;;  cards) and NEH, where the card would get drawn before the install,
                          ;;  fizzling it in a confusing manner. Because we only do it in this
                          ;;  special case, there should be no gameplay implications. -nbkelly, 2022
                          (register-events
                           state side
                           card
                           [{:event :corp-install
                             :interactive (req true)
                             :duration (req true)
                             :unregister-once-resolved true
                             :async true
                             :effect (effect (draw :corp eid 1))}])
                          (effect-completed state side eid))))}]})

(defcard "Nero Severn: Information Broker"
  {:events [{:event :encounter-ice
             :optional (:optional (offer-jack-out {:req (req (has-subtype? (:ice context) "Sentry"))
                                                   :once :per-turn}))}]})

(defcard "New Angeles Sol: Your News"
  (let [nasol {:optional
               {:prompt "Play a Current?"
                :player :corp
                :req (req (some #(has-subtype? % "Current") (concat (:hand corp) (:discard corp) (:current corp))))
                :yes-ability {:prompt "Choose a Current to play from HQ or Archives"
                              :show-discard true
                              :async true
                              :choices {:card #(and (has-subtype? % "Current")
                                                    (corp? %)
                                                    (or (in-hand? %)
                                                        (in-discard? %)))}
                              :msg (msg "play a current from " (name-zone "Corp" (get-zone target)))
                              :effect (effect (play-instant eid target))}}}]
    {:events [(assoc nasol :event :agenda-scored)
              (assoc nasol :event :agenda-stolen)]}))

(defcard "NEXT Design: Guarding the Net"
  (let [ndhelper (fn nd [n] {:prompt (msg "When finished, click " (:title card) " to draw back up to 5 cards in HQ. "
                                          "Choose a piece of ice in HQ to install")
                             :choices {:card #(and (corp? %)
                                                   (ice? %)
                                                   (in-hand? %))}
                             :effect (req (wait-for (corp-install state side target nil nil)
                                                    (continue-ability state side (when (< n 3) (nd (inc n))) card nil)))})]
    {:events [{:event :pre-first-turn
               :req (req (= side :corp))
               :msg "install up to 3 pieces of ice and draw back up to 5 cards"
               :async true
               :effect (req (wait-for (resolve-ability state side (ndhelper 1) card nil)
                                      (update! state side (assoc card :fill-hq true))
                                      (effect-completed state side eid)))}]
     :abilities [{:req (req (:fill-hq card))
                  :label "draw remaining cards"
                  :msg (msg "draw " (quantify (- 5 (count (:hand corp))) "card"))
                  :async true
                  :effect (req (wait-for (draw state side (- 5 (count (:hand corp))) {:suppress-event true})
                                         (update! state side (dissoc card :fill-hq))
                                         (swap! state assoc :turn-events nil)
                                         (effect-completed state side eid)))}]}))

(defcard "Nisei Division: The Next Generation"
  {:events [{:event :reveal-spent-credits
             :req (req (and (some? (first targets))
                            (some? (second targets))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Noise: Hacker Extraordinaire"
  {:events [{:async true
             :event :runner-install
             :req (req (has-subtype? (:card context) "Virus"))
             :msg "force the Corp to trash the top card of R&D"
             :effect (effect (mill :corp eid :corp 1))}]})

(defcard "Null: Whistleblower"
  {:events [{:event :encounter-ice
             :optional
             {:req (req (pos? (count (:hand runner))))
              :prompt "Trash a card in the grip to lower the strength of encountered ice by 2?"
              :once :per-turn
              :yes-ability
              {:prompt "Choose a card to trash"
               :choices {:card in-hand?}
               :msg (msg "trash " (:title target)
                         " from the grip to lower the strength of " (:title current-ice)
                         " by 2 for the remainder of the run")
               :async true
               :effect (effect (register-lingering-effect
                                 card
                                 (let [ice current-ice]
                                   {:type :ice-strength
                                    :duration :end-of-run
                                    :req (req (same-card? target ice))
                                    :value -2}))
                               (update-all-ice)
                               (trash eid target {:unpreventable true}))}}}]})

(defcard "Nuvem SA: Law of the Land"
  (let [abi2 {:once :per-turn
              :event :corp-trash
              :req (req (and (= :corp (:active-player @state))
                             (= [:deck] (:zone (:card target)))))
              :msg "gain 2 [Credits]"
              :async true
              :effect (effect (gain-credits :corp eid 2))}
        abi1 {:prompt (msg "The top card of R&D is: " (:title (first (:deck corp))))
              :async true
              :msg "look at the top card of R&D"
              :choices ["OK"]
              :req (req (seq (:deck corp)))
              :effect
              (effect (continue-ability
                        {:optional
                         {:prompt (str "Trash " (:title (first (:deck corp))) "?")
                          :yes-ability
                          {:msg "trash the top card of R&D"
                           :async true
                           :effect (req (mill state :corp eid :corp 1))}}}
                        card nil))}]
    {:events [(assoc abi1 :event :expend-resolved)
              (assoc abi1 :event :play-operation-resolved)
              abi2]}))

(defcard "Nyusha \"Sable\" Sintashta: Symphonic Prodigy"
  {:events [(assoc identify-mark-ability :event :runner-turn-begins)
            {:event :successful-run
             :interactive (req true)
             :req (req (and (:marked-server target)
                            (first-event? state side :successful-run #(:marked-server (first %)))))
             :msg "gain [Click]"
             :effect (effect (gain-clicks 1))}]})

(defcard "Ob Superheavy Logistics: Extract. Export. Excel."
  ;; note - we ensure the card can be installed (asset/upgrade/ice) - condition counters (like patch)
  ;;   are very questionable, and somebody on rules would need to say something to convince me they
  ;;   would be valid targets --nbkelly
  ;; If you install a card with an additional cost, you can (or should be able to) refuse to pay it
  ;;  and have the card not be rezzed (additionally, you wouldn't need to reveal it)
  ;; A lot of abilities/cards will need to be adjusted (most of them don't have a cause when they
  ;;  call the (trash-cards) function - this makes it tough to tell who/what trashed the cards.)
  ;;  to update these cards, just add {:cause card} into the keys pass with (trash-card)
  ;;  At the moment, the source (or cause) of the trash must be a corp-card, a subroutine,
  ;;  or it must be an ability cost.
  (letfn [(resolve-install []
            (req
              (shuffle! state side :deck)
              (if (= "Done" target)
                (effect-completed state side eid)
                ;; if it has an additional cost, the rez needs to be optional
                (let [add-costs (rez-additional-cost-bonus state side target #(not (= :credit (first %))))
                      inst-target target]
                  (cond
                    (and (pos? (count add-costs))
                         (can-pay? state side (:title inst-target) add-costs))
                    (continue-ability
                      state side
                      {:optional
                       {:prompt (str "Rez " (:title inst-target) ", paying additional costs?")
                        :yes-ability {:msg (msg "rez " (:title inst-target)
                                                ", paying additional costs")
                                      :async true
                                      :effect (req (corp-install state side eid inst-target nil
                                                                 {:ignore-all-cost true
                                                                  :no-warning true
                                                                  :install-state :rezzed-no-rez-cost}))}
                        :no-ability {:msg "install a card from R&D ignoring all credit costs"
                                     :async true
                                     :effect (req (corp-install state side eid inst-target nil
                                                                {:ignore-all-cost true}))}}}
                      card nil)
                    ;; It might be worth having a fake prompt here - at the very least, this prevents
                    ;; the corp from accidentally revealing that they can't pay for the card they select
                    (pos? (count add-costs))
                    (continue-ability
                      state side
                      {:msg "install a card from R&D without paying additional costs to rez"
                       :async true
                       :effect (req (corp-install state side eid inst-target nil
                                                  {:ignore-all-cost true
                                                   :no-warning true}))}
                      card nil)
                    :else
                    (wait-for (reveal state side inst-target)
                              (corp-install state side eid (get-card state inst-target) nil
                                            {:ignore-all-cost true
                                             :no-warning true
                                             :install-state :rezzed-no-rez-cost})))))))
          ;; Identify that the card wasn't just dragged to the discard, and that it was trashed
          ;; by the corporation.
          ;; This requires that any (trash-cards ..) or (trash ..) fns use {:source card}
          ;; to be compatable. Updating those functions is quite quick, just make sure it actually
          ;; is the corporation doing it.
          (trash-cause [eid target]
            (let [cause (:cause target)
                  cause-card (:cause-card target)]
              (or
                (corp? (:source eid))
                (= :ability-cost cause)
                (= :subroutine cause)
                (and (corp? cause-card) (not= cause :opponent-trashes))
                (and (runner? cause-card) (= cause :forced-to-trash)))))
          ;; prompts to install an x-cost card (handles validation)
          (ob-ability [target-cost]
            {:optional
             {:prompt (if (>= target-cost 0)
                        (str "Install a " target-cost "-cost card from your deck?")
                        (str "Shuffle your deck (search for a " target-cost "-cost card from your deck?)"))
              :once :per-turn
              :waiting-prompt true
              :yes-ability
              {:msg (msg "search R&D for a " target-cost "-cost card")
               :async true
               :effect (req (if (>= target-cost 0)
                              (continue-ability
                                state side
                                {:prompt "Choose a card to install and rez"
                                 :choices (req (concat
                                                 (->> (:deck corp)
                                                      (filter
                                                        #(and (or (asset? %)
                                                                  (upgrade? %)
                                                                  (ice? %))
                                                              (= target-cost (:cost %))))
                                                      (sort-by :title)
                                                      (seq))
                                                 ["Done"]))
                                 :msg "shuffle R&D"
                                 :async true
                                 :effect (resolve-install)}
                                card nil)
                              (continue-ability
                                state side
                                {:msg "shuffle R&D"
                                 :effect (effect (shuffle! :corp :deck))}
                                card nil)))}
              :no-ability
              {:effect (effect (system-msg (str "declines to use " (:title card))))}}})]
    {:events [{:event :corp-trash
               :req (req (and
                           (installed? (:card context))
                           (rezzed? (:card context))
                           (trash-cause eid target)
                           (not (used-this-turn? (:cid card) state))))
               :async true
               :interactive (req true)
               :effect (req (let [target-cost (dec (:cost (:card target)))]
                              (continue-ability
                                state side
                                (ob-ability target-cost)
                                card nil)))}]}))

(defcard "Omar Keung: Conspiracy Theorist"
  {:abilities [{:cost [:click 1]
                :msg "make a run on Archives"
                :once :per-turn
                :makes-run true
                :async true
                :effect (effect (update! (assoc-in card [:special :omar-run] true))
                                (make-run eid :archives (get-card state card)))}]
   :events [{:event :pre-successful-run
             :interactive (req true)
             :req (req (and (get-in card [:special :omar-run])
                            (= :archives (-> run :server first))))
             :prompt "Choose one"
             :choices ["HQ" "R&D"]
             :msg (msg "change the attacked server to " target)
             :effect (req (let [target-server (if (= target "HQ") :hq :rd)]
                            (swap! state assoc-in [:run :server] [target-server])
                            (trigger-event state :corp :no-action)))}
            {:event :run-ends
             :effect (effect (update! (dissoc-in card [:special :omar-run])))}]})

(defcard "Ampère: Cybernetics For Anyone"
    ;; No special implementation
  {})

(defcard "Nova Initiumia: Catalyst & Impetus"
      ;; No special implementation
  {})

(defcard "Pālanā Foods: Sustainable Growth"
  {:events [{:event :runner-draw
             :req (req (and (first-event? state :corp :runner-draw)
                            (pos? (:count target))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Pravdivost Consulting: Political Solutions"
  {:events [{:event :successful-run
             :req (req (first-event? state side :successful-run))
             :interactive (req true)
             :async true
             :waiting-prompt true
             :prompt "Choose a card that can be advanced to place 1 advancement token on"
             :choices {:card #(and (installed? %) (can-be-advanced? %))}
             :msg (msg "place 1 advancement token on " (card-str state target))
             :effect (effect (add-prop :corp eid target :advance-counter 1 {:placed true}))
             :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                    (effect-completed eid))}]})

(defcard "Quetzal: Free Spirit"
  {:abilities [(assoc (break-sub nil 1 "Barrier" {:repeatable false}) :once :per-turn)]})

(defcard "Reina Roja: Freedom Fighter"
  (letfn [(not-triggered? [state]
            (no-event? state :runner :rez #(ice? (:card (first %)))))]
    {:static-abilities [{:type :rez-cost
                         :req (req (and (ice? target)
                                        (not (rezzed? target))
                                        (not-triggered? state)))
                         :value 1}]
     :events [{:event :rez
               :req (req (and (ice? (:card context))
                              (not-triggered? state)))
               :msg (msg "increased the rez cost of " (:title (:card context)) " by 1 [Credits]")}]}))

(defcard "René \"Loup\" Arcemont: Party Animal"
  {:events [{:event :runner-trash
             :req (req (and (:accessed context)
                            (first-event? state side :runner-trash
                                          (fn [targets]
                                            (some #(:accessed %) targets)))))
             :async true
             :msg "gain 1 [Credits] and draw 1 card"
             :effect (req (wait-for (draw state :runner 1)
                                    (gain-credits state :runner eid 1)))}]})

(defcard "Rielle \"Kit\" Peddler: Transhuman"
  {:events [{:event :encounter-ice
             :once :per-turn
             :msg (msg "make " (:title (:ice context))
                       " gain Code Gate until the end of the run")
             :effect (effect (register-lingering-effect
                               card
                               (let [ice (:ice context)]
                                 {:type :gain-subtype
                                  :duration :end-of-run
                                  :req (req (same-card? ice target))
                                  :value "Code Gate"})))}]})

(defcard "Saraswati Mnemonics: Endless Exploration"
  (letfn [(install-card [chosen]
            {:prompt "Choose a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (req (let [tgtcid (:cid chosen)]
                            (register-persistent-flag!
                              state side
                              card :can-rez
                              (fn [state _ card]
                                (if (= (:cid card) tgtcid)
                                  ((constantly false)
                                   (toast state :corp "Cannot rez due to Saraswati Mnemonics: Endless Exploration." "warning"))
                                  true)))
                            (register-turn-flag!
                              state side
                              card :can-score
                              (fn [state _side card]
                                (if (and (= (:cid card) tgtcid)
                                         (<= (get-advancement-requirement card) (get-counters card :advancement)))
                                  ((constantly false)
                                   (toast state :corp "Cannot score due to Saraswati Mnemonics: Endless Exploration." "warning"))
                                  true))))
                          (wait-for (corp-install state side chosen target nil)
                                    (add-prop state :corp (find-latest state chosen) :advance-counter 1 {:placed true})
                                    (effect-completed state side eid)))})]
    {:abilities [{:async true
                  :label "Install a card from HQ"
                  :cost [:click 1 :credit 1]
                  :prompt "Choose a card to install from HQ"
                  :choices {:card #(and (or (asset? %) (agenda? %) (upgrade? %))
                                     (corp? %)
                                     (in-hand? %))}
                  :msg (msg "install a card in a remote server and place 1 advancement token on it")
                  :effect (effect (continue-ability (install-card target) card nil))}]
     :events [{:event :corp-turn-begins
               :effect (req (clear-persistent-flag! state side card :can-rez))}]}))

(defcard "Sebastião Souza Pessoa: Activist Organizer"
  {:static-abilities [{:type :basic-ability-additional-trash-cost
                       :req (req (and (resource? target) (has-subtype? target "Connection") (= :corp side)))
                       :value [:trash-from-hand 1]}]
   :events [{:event :runner-gain-tag
             :async true
             :req (req (and (not (install-locked? state side))
                            (= (second targets) (count-tags state)))) ;; every tag is one that was just gained
             :prompt "Choose a connection to install, paying 2 [Credits] less"
             :player :runner
             :choices
             {:req (req (and (has-subtype? target "Connection")
                             (resource? target)
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [:credit (install-cost state side target {:cost-bonus -2})])))}
             :msg (msg "install " (:title target) " from the grip, paying 2 [Credit] less")
             :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -2}))}]})

(defcard "Seidr Laboratories: Destiny Defined"
  {:implementation "Manually triggered"
   :abilities [{:req (req (and run (seq (:discard corp))))
                :label "add card from Archives to R&D during a run"
                :once :per-turn
                :prompt "Choose a card to add to the top of R&D"
                :show-discard true
                :choices {:card #(and (corp? %)
                                      (in-discard? %))}
                :effect (effect (move target :deck {:front true}))
                :msg (msg "add " (if (:seen target) (:title target) "a card") " to the top of R&D")}]})

(defcard "Silhouette: Stealth Operative"
  {:events [{:event :successful-run
             :interactive (req (some #(not (rezzed? %)) (all-installed state :corp)))
             :async true
             :req (req (and (= :hq (target-server context))
                            (first-successful-run-on-server? state :hq)))
             :choices {:card #(and (installed? %)
                                   (not (rezzed? %)))}
             :msg "expose 1 card"
             :effect (effect (expose eid target))}]})

(defcard "Skorpios Defense Systems: Persuasive Power"
  {:implementation "Manually triggered, no restriction on which cards in Heap can be targeted. Cannot use on in progress run event"
   :abilities [{:label "Remove a card in the Heap that was just trashed from the game"
                :waiting-prompt true
                :prompt "Choose a card in the Heap that was just trashed"
                :once :per-turn
                :choices (req (cancellable (:discard runner)))
                :msg (msg "remove " (:title target) " from the game")
                :effect (req (move state :runner target :rfg))}]})

(defcard "Spark Agency: Worldswide Reach"
  {:events [{:event :rez
             :req (req (and (has-subtype? (:card context) "Advertisement")
                            (first-event? state :corp :rez #(has-subtype? (:card (first %)) "Advertisement"))))
             :async true
             :effect (effect (lose-credits :runner eid 1))
             :msg "make the Runner lose 1 [Credits] by rezzing an Advertisement"}]})

(defcard "Sportsmetal: Go Big or Go Home"
  (let [ab {:prompt "Choose one"
            :waiting-prompt true
            :player :corp
            :choices ["Gain 2 [Credits]" "Draw 2 cards"]
            :msg (msg (decapitalize target))
            :async true
            :interactive (req true)
            :effect (req (if (= target "Gain 2 [Credits]")
                           (gain-credits state :corp eid 2)
                           (draw state :corp eid 2)))}]
    {:events [(assoc ab :event :agenda-scored)
              (assoc ab :event :agenda-stolen)]}))

(defcard "SSO Industries: Fueling Innovation"
  (letfn [(installed-faceup-agendas [state]
            (->> (all-installed state :corp)
                 (filter agenda?)
                 (filter faceup?)))
          (selectable-ice? [card]
            (and
              (ice? card)
              (installed? card)
              (zero? (get-counters card :advancement))))
          (ice-with-no-advancement-tokens [state]
            (->> (all-installed state :corp)
                 (filter selectable-ice?)))]
    {:events [{:event :corp-turn-ends
               :optional
               {:req (req (and (not-empty (installed-faceup-agendas state))
                               (not-empty (ice-with-no-advancement-tokens state))))
                :waiting-prompt true
                :prompt "Place advancement tokens on an installed piece of ice?"
                :autoresolve (get-autoresolve :auto-fire)
                :yes-ability
                {:async true
                 :effect (req (let [agendas (installed-faceup-agendas state)
                                    agenda-points (->> agendas
                                                       (map :agendapoints)
                                                       (reduce +))]
                                (continue-ability
                                  state side
                                  {:prompt (str "Choose a piece of ice with no advancement tokens to place "
                                                (quantify agenda-points "advancement token") " on")
                                   :choices {:card #(selectable-ice? %)}
                                   :msg (msg "place " (quantify agenda-points "advancement token")
                                             " on " (card-str state target))
                                   :effect (effect (add-prop target :advance-counter agenda-points {:placed true}))}
                                  card nil)))}}}]
     :abilities [(set-autoresolve :auto-fire "SSO Industries: Fueling Innovation")]}))

(defcard "Steve Cambridge: Master Grifter"
  {:events [{:event :successful-run
             :optional
             {:req (req (and (= :hq (target-server context))
                             (first-successful-run-on-server? state :hq)
                             (<= 2 (count (:discard runner)))
                             (not (zone-locked? state :runner :discard))))
              :prompt "Choose 2 cards in the heap?"
              :autoresolve (get-autoresolve :auto-fire)
              :interactive (req true)
              :yes-ability
              {:async true
               :prompt "Choose 2 cards in the heap"
               :show-discard true
               :choices {:max 2
                         :all true
                         :card #(and (in-discard? %)
                                     (runner? %))}
               :effect
               (effect (continue-ability
                         (let [c1 (first targets)
                               c2 (second targets)]
                           {:waiting-prompt true
                            :prompt "Choose which card to remove from the game"
                            :player :corp
                            :choices [c1 c2]
                            :msg (msg (let [[chosen other](if (= target c1)
                                                            [c1 c2]
                                                            [c2 c1])]
                                        (str "add " (:title other) " from the heap to the grip."
                                             " Corp removes " (:title chosen) " from the game")))
                            :effect (req (let [[chosen other] (if (= target c1)
                                                                [c1 c2]
                                                                [c2 c1])]
                                           (move state :runner chosen :rfg)
                                           (move state :runner other :hand)))})
                         card nil))}}}]
   :abilities [(set-autoresolve :auto-fire "Steve Cambridge: Master Grifter")]})

(defcard "Strategic Innovations: Future Forward"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-turn-ends
             :req (req (and (not (:disabled card))
                            (has-most-faction? state :corp "Haas-Bioroid")
                            (pos? (count (:discard corp)))))
             :prompt "Choose a card in Archives to shuffle into R&D"
             :choices {:card #(and (corp? %)
                                   (in-discard? %))}
             :player :corp
             :show-discard true
             :msg (msg "shuffle " (if (:seen target) (:title target) "a card")
                       " into R&D")
             :effect (effect (move :corp target :deck)
                             (shuffle! :corp :deck))}]})

(defcard "Sunny Lebeau: Security Specialist"
  ;; No special implementation
  {})

(defcard "SYNC: Everything, Everywhere"
  {:static-abilities [{:type :card-ability-additional-cost
                       :req (req (let [targetcard (first targets)
                                       target (second targets)]
                                   (and (not (:sync-flipped card))
                                        (same-card? targetcard (:basic-action-card runner))
                                        (= "Remove 1 tag" (:label target)))))
                       :value [:credit 1]}
                      {:type :card-ability-additional-cost
                       :req (req (let [targetcard (first targets)
                                       target (second targets)]
                                   (and (:sync-flipped card)
                                        (same-card? targetcard (:basic-action-card corp))
                                        (= "Trash 1 resource if the Runner is tagged" (:label target)))))
                       :value [:credit -2]}]
   :abilities [{:cost [:click 1]
                :effect (req (if (:sync-flipped card)
                               (update! state side (-> card (assoc :sync-flipped false :face :front :code "09001")))
                               (update! state side (-> card (assoc :sync-flipped true :face :back :code "sync")))))
                :label "Flip this identity"
                :msg "flip their identity"}]})

(defcard "Synthetic Systems: The World Re-imagined"
  {:events [{:event :pre-start-game
             :effect draft-points-target}]
   :flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                    (has-most-faction? state :corp "Jinteki")
                                    (<= 2 (count (filter ice? (all-installed state :corp))))))}
   :abilities [{:prompt "Choose 2 installed pieces of ice to swap"
                :label "swap 2 installed pieces of ice"
                :choices {:card #(and (installed? %)
                                      (ice? %))
                          :max 2
                          :all true}
                :once :per-turn
                :effect (req (apply swap-ice state side targets))
                :msg (msg "swap the positions of " (card-str state (first targets))
                          " and " (card-str state (second targets)))}]})

(defcard "Tāo Salonga: Telepresence Magician"
  (let [swap-ability
        {:interactive (req true)
         :optional
         {:req (req (<= 2 (count (filter ice? (all-installed state :corp)))))
          :prompt "Swap 2 pieces of ice?"
          :waiting-prompt true
          :yes-ability
          {:prompt "Choose 2 pieces of ice"
           :choices {:req (req (and (installed? target)
                                    (ice? target)))
                     :max 2
                     :all true}
           :msg (msg "swap the positions of " (card-str state (first targets))
                     " and " (card-str state (second targets)))
           :effect (req (swap-ice state side (first targets) (second targets)))}
          :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
    {:events [(assoc swap-ability :event :agenda-scored)
              (assoc swap-ability :event :agenda-stolen)]}))

(defcard "Tennin Institute: The Secrets Within"
  {:flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                    (not-last-turn? state :runner :successful-run)))}
   :abilities [{:msg (msg "place 1 advancement token on " (card-str state target))
                :label "Place 1 advancement token on a card if the Runner did not make a successful run last turn"
                :choices {:card installed?}
                :req (req (and (:corp-phase-12 @state)
                               (not-last-turn? state :runner :successful-run)))
                :once :per-turn
                :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]})

(defcard "The Catalyst: Convention Breaker"
  ;; No special implementation
  {})

(defcard "The Foundry: Refining the Process"
  {:events [{:event :rez
             :optional
             {:prompt (msg "Add another copy of " (:title (:card context)) " to HQ?")
              :req (req (and (ice? (:card context))
                             (first-event? state :runner :rez #(ice? (:card (first %))))))
              :yes-ability
              {:effect (req (if-let [found-card (some #(when (= (:title %) (:title (:card context))) %) (concat (:deck corp) (:play-area corp)))]
                              (do (move state side found-card :hand)
                                  (system-msg state side (str "uses " (:title card) " to add a copy of "
                                                              (:title found-card) " to HQ, and shuffle R&D"))
                                  (shuffle! state side :deck))
                              (do (system-msg state side (str "shuffles R&D"))
                                  (shuffle! state side :deck))))}}}]})

(defcard "The Masque: Cyber General"
  {:events [{:event :pre-start-game
             :effect draft-points-target}]})

(defcard "The Outfit: Family Owned and Operated"
  {:events [{:event :corp-gain-bad-publicity
             :msg "gain 3 [Credit]"
             :async true
             :effect (effect (gain-credits eid 3))}]})

(defcard "The Professor: Keeper of Knowledge"
  ;; No special implementation
  {})

(defcard "The Shadow: Pulling the Strings"
  {:events [{:event :pre-start-game
             :effect draft-points-target}]})

(defcard "The Syndicate: Profit over Principle"
  ;; No special implementation
  {})

(defcard "Thule Subsea: Safety Below"
  {:events [{:event :agenda-stolen
             :async true
             :effect (effect (continue-ability
                               {:prompt "Choose one"
                                :player :runner
                                :choices (req [(when (can-pay? state :runner eid card nil [:credit 2 :click 1])
                                                 "Pay [Click] and 2 [Credits]")
                                               "Suffer 1 core damage"])
                                :async true
                                :waiting-prompt true
                                :msg (msg (if (= target "Pay [Click] and 2 [Credits]")
                                            (str "force the runner to " (decapitalize target))
                                            "do 1 core damage"))
                                :effect (req (if (= target "Pay [Click] and 2 [Credits]")
                                               (wait-for (pay state side (make-eid state eid) card [:click 1 :credit 2])
                                                         (system-msg state side (:msg async-result))
                                                         (effect-completed state :runner eid))
                                               (damage state side eid :brain 1 {:card card})))}
                               card nil))}]})

(defcard "Thunderbolt Armaments: Peace Through Power"
  (let [thunderbolt-sub
        {:player :runner
         :async true
         :label (str "End the run unless the Runner pays " (build-cost-label [:trash-installed 1]))
         :prompt "Choose one"
         :waiting-prompt true
         :choices (req ["End the run"
                        (when (can-pay? state :runner eid card nil [:trash-installed 1])
                          (capitalize (cost->string [:trash-installed 1])))])
         :msg (msg (if (= "End the run" target)
                     (decapitalize target)
                     (str "force the runner to " (decapitalize target))))
         :effect (req (if (= "End the run" target)
                        (end-run state :corp eid card)
                        (wait-for (pay state :runner (make-eid state eid) card [:trash-installed 1])
                                  (when-let [payment-str (:msg async-result)]
                                    (system-msg state :runner
                                                (str payment-str
                                                     " due to " (:title card)
                                                     " subroutine")))
                                  (effect-completed state side eid))))}]
    {:events [{:event :run-ends
               :effect (req (let [cid (:cid card)
                                  ices (get-in card [:special :thunderbolt-armaments])]
                              (doseq [i ices]
                                (when-let [ice (get-card state i)]
                                  (remove-sub! state side ice #(= cid (:from-cid %))))))
                            (update! state side (dissoc-in card [:special :thunderbolt-armaments])))}
              {:event :rez
               :req (req (and run
                              (ice? (:card context))
                              (or (has-subtype? (:card context) "AP")
                                  (has-subtype? (:card context) "Destroyer"))))
               :msg (msg "give " (card-str state (:card context))
                         " +1 strength and \""
                         (:label thunderbolt-sub)
                         "\" after its other subroutines")
               :async true
               :effect (effect (add-extra-sub! (get-card state (:card context))
                                               thunderbolt-sub
                                               (:cid card) {:front false})
                               (update! (update-in card [:special :thunderbolt-armaments]
                                                   #(conj % (:card context))))
                               (pump-ice (:card context) 1 :end-of-run)
                               (effect-completed eid))}]}))

(defcard "Titan Transnational: Investing In Your Future"
  {:events [{:event :agenda-scored
             :msg (msg "place 1 agenda counter on " (:title (:card context)))
             :effect (effect (add-counter (get-card state (:card context)) :agenda 1))}]})

(defcard "Valencia Estevez: The Angel of Cayambe"
  {:events [{:event :pre-start-game
             :req (req (and (= side :runner)
                            (zero? (count-bad-pub state))))
             ;; This doesn't use `gain-bad-publicity` to avoid the event
             :effect (effect (gain :corp :bad-publicity 1))}]})

(defcard "Weyland Consortium: Because We Built It"
  {:recurring 1
   :interactions {:pay-credits {:req (req (or (= :advance (:source-type eid))
                                              (is-basic-advance-action? eid)))
                                :type :recurring}}})

(defcard "Weyland Consortium: Builder of Nations"
  {:implementation "[Erratum] The first time an encounter with a piece of ice with at least 1 advancement token ends each turn, do 1 meat damage."
   :events [{:event :end-of-encounter
             :async true
             :req (req (and (rezzed? (:ice context))
                            (pos? (get-counters (:ice context) :advancement))
                            (first-event? state :runner :end-of-encounter
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (and (rezzed? (:ice context))
                                                   (pos? (get-counters (:ice context) :advancement))))))))
             :msg "do 1 meat damage"
             :effect (effect (damage eid :meat 1 {:card card}))}]})

(defcard "Weyland Consortium: Building a Better World"
  {:events [{:event :play-operation
             :req (req (has-subtype? (:card context) "Transaction"))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Weyland Consortium: Built to Last"
  {:events [{:event :advance
             :async true
             :req (req ((complement pos?)
                        (- (get-counters target :advancement) (:amount (second targets) 0))))
             :msg "gain 2 [Credits]"
             :effect (req (gain-credits state :corp eid 2))}]})

(defcard "Whizzard: Master Gamer"
  {:recurring 3
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (corp? target)))
                                :type :recurring}}})

(defcard "Wyvern: Chemically Enhanced"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-trash
             :interactive (req true)
             :req (req (and (has-most-faction? state :runner "Anarch")
                            (corp? (:card target))
                            (pos? (count (:discard runner)))
                            (not (zone-locked? state :runner :discard))))
             :msg (msg "shuffle " (:title (last (:discard runner))) " into the stack")
             :effect (effect (move :runner (last (:discard runner)) :deck)
                             (shuffle! :runner :deck)
                             (trigger-event :searched-stack nil))}]})

(defcard "Zahya Sadeghi: Versatile Smuggler"
  {:events [{:event :run-ends
             :optional
             {:req (req (and (#{:hq :rd} (target-server context))
                          (pos? (total-cards-accessed context))))
              :prompt "Gain 1 [Credits] for each card you accessed?"
              :once :per-turn
              :yes-ability
              {:msg (msg "gain " (total-cards-accessed context) " [Credits]")
               :once :per-turn
               :async true
               :effect (req (gain-credits state :runner eid (total-cards-accessed context)))}}}]})
