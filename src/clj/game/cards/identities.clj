(ns game.cards.identities
  (:require
   [clojure.pprint :as pprint]
   [game.core.access :refer [access-bonus access-cost-bonus access-non-agenda]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed card->server
                            get-all-cards get-remote-names get-remotes server->zone]]
   [game.core.card :refer [agenda? asset? can-be-advanced?
                           condition-counter? corp-installable-type? corp? event? faceup? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype?
                           has-any-subtype? ice? in-discard? in-deck? in-hand? in-play-area? in-rfg? installed? is-type?
                           operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.charge :refer [charge-ability]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [install-cost play-cost
                               rez-additional-cost-bonus rez-cost]]
   [game.core.damage :refer [chosen-damage corp-can-choose-damage? damage
                             enable-corp-damage-choice]]
   [game.core.def-helpers :refer [all-cards-in-hand* in-hand*? corp-recur defcard offer-jack-out run-server-ability with-revealed-hand]]
   [game.core.drawing :refer [draw maybe-draw]]
   [game.core.effects :refer [register-lingering-effect is-disabled?]]
   [game.core.eid :refer [effect-completed get-ability-targets is-basic-advance-action? make-eid]]
   [game.core.engine :refer [not-used-once? pay register-events register-once resolve-ability trigger-event unregister-event-by-uuid]]
   [game.core.events :refer [event-count first-event? first-trash?
                             first-successful-run-on-server? no-event? not-last-turn? run-events run-event-count turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-latest]]
   [game.core.flags :refer [card-flag? clear-persistent-flag!
                            register-persistent-flag! register-turn-flag! zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [break-sub pump-ice update-all-ice update-all-icebreakers]]
   [game.core.initializing :refer [make-card]]
   [game.core.installing :refer [corp-install install-locked? runner-can-pay-and-install? runner-install]]
   [game.core.link :refer [link+ update-link]]
   [game.core.mark :refer [identify-mark-ability mark-changed-event]]
   [game.core.memory :refer [available-mu mu+]]
   [game.core.moving :refer [mill move swap-ice trash trash-cards]]
   [game.core.optional :refer [get-autoresolve never? set-autoresolve]]
   [game.core.payment :refer [build-cost-label can-pay? cost->string merge-costs ->c]]
   [game.core.prevention :refer [prevent-tag]]
   [game.core.pick-counters :refer [pick-virus-counters-to-spend]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.revealing :refer [conceal-hand reveal reveal-hand reveal-loud]]
   [game.core.rezzing :refer [rez]]
   [game.core.runs :refer [end-run get-current-encounter make-run redirect-run
                           set-next-phase start-next-phase total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote? name-zone
                              target-server zone->name]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck shuffle-cards-into-deck!]]
   [game.core.tags :refer [gain-tags lose-tags]]
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
                            (not (#{:face-up} (:install-state context)))))
             :waiting-prompt true
             :effect
             (effect
               (continue-ability
                 {:optional
                  {:prompt "Expose installed card unless the Corp pays 1 [Credits]?"
                   :autoresolve (get-autoresolve :auto-fire)
                   :no-ability {:effect (req (clear-wait-prompt state :corp))}
                   :yes-ability
                   {:async true
                    :effect (req (if (not (can-pay? state :corp eid card nil (->c :credit 1)))
                                   (do
                                     (toast state :corp "Cannot afford to pay 1 [Credit] to block card exposure" "info")
                                     (expose state :runner eid [(:card context)]))
                                   (continue-ability
                                     state side
                                     {:optional
                                      {:waiting-prompt true
                                       :prompt (req (str "Pay 1 [Credits] to prevent exposing " (card-str state (:card context)) "?"))
                                       :player :corp
                                       :no-ability
                                       {:async true
                                        :effect (effect (expose :runner eid [(:card context)]))}
                                       :yes-ability
                                       {:async true
                                        :effect
                                        (req (wait-for
                                               (pay state :corp (make-eid state eid) card [(->c :credit 1)])
                                               (system-msg state :corp
                                                           (str (:msg async-result)
                                                                " to prevent exposing "
                                                                (card-str state (:card context))))
                                               (effect-completed state side eid)))}}}
                                     card targets)))}}}
                 card targets))}]
   :abilities [(set-autoresolve :auto-fire "419: Amoral Scammer")]})

(defcard "A Teia: IP Recovery"
  {:flags {:server-limit 2}
   :events [{:event :corp-install
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
                                                 :choices (req (conj (vec (filter #(not= original-server %)
                                                                                  (get-remote-names state))) "New remote"))
                                                 :async true
                                                 :effect (effect (corp-install eid chosen-card target {:ignore-install-cost true
                                                                                                       :msg-keys {:install-source card
                                                                                                                  :display-origin true}}))}
                                                card nil)))}
                              card nil)))}]
   ;; This effect will be resolved when the ID is reenabled after Strike / Direct Access
   :enforce-conditions {:req (req (< 2 (count (get-remotes state))))
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
                                         card nil)))}})

(defcard "AU Co.: The Gold Standard in Clones"
  (let [abi {:msg (msg "place 1 power counter on itself")
             :label "Manually place 1 power counter"
             :once-per-instance true
             :async true
             :effect (req (add-counter state side eid card :power 1))}
        start-of-turn-ability
        {:interactive (req true)
         :skippable true
         :event :corp-turn-begins
         :change-in-game-state {:silent true :req (req (<= 2 (get-counters card :power)))}
         :label "Look at the top 3 cards of R&D"
         :optional {:req (req (and (seq (:deck corp))
                                   (:corp-phase-12 @state)))
                    :prompt "Look at the top 3 cards of R&D?"
                    :waiting-prompt true
                    :yes-ability {:cost [(->c :power 2)]
                                  :async true
                                  :msg "look at the top 3 cards of R&D"
                                  :effect (req (let [top-3 (take 3 (:deck corp))
                                                     to-draw (dec (count top-3))]
                                                 (continue-ability
                                                   state side
                                                   {:async true
                                                    :prompt (str "The top of R&D is (top->bottom): " (enumerate-str (map :title top-3)) ". Choose a card to trash")
                                                    :not-distinct true
                                                    :choices (req top-3)
                                                    :msg (msg (let [target-position (first (positions #{target} (take 3 (:deck corp))))
                                                                    position (case target-position
                                                                               0 "top "
                                                                               1 "second "
                                                                               2 "third "
                                                                               "this-should-not-happen ")]
                                                                (str "trash the " position "card from R&D"))
                                                              (when (pos? to-draw)
                                                                (str " and draw " to-draw " cards")))
                                                    :effect (req (wait-for (trash state :corp target {:cause-card card :suppress-checkpoint (pos? to-draw)})
                                                                           (if (pos? to-draw)
                                                                             (draw state side eid to-draw)
                                                                             (effect-completed state side eid))))}
                                                   card nil)))}}}]
    {:events [(assoc abi :event :damage :req (req (= :corp (:from-side target))))
              (assoc abi :event :corp-trash
                     :req (req
                            (letfn [(valid-ctx? [ctx]
                                      (and ((every-pred corp? in-hand?) (:card ctx))
                                           (or (:cause ctx) (:cause-card ctx))))]
                              (some valid-ctx? targets))))
              start-of-turn-ability]
     :abilities [abi
                 start-of-turn-ability]}))

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
                               :async true
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
             :automatic :pre-breach
             :interactive (req true)
             :psi {:req (req (= target :rd))
                   :equal {:msg "access 1 additional card"
                           :async true
                           :effect (effect (access-bonus :rd 1)
                                           (effect-completed eid))}}}]})

(defcard "Alice Merchant: Clan Agitator"
  {:events [{:event :successful-run
             :interactive (req true)
             :automatic :force-discard
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


(defcard "Ampère: Cybernetics For Anyone"
  ;; No special implementation
  {})

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
                           :req (req (and (runner? target)
                                          (in-hand*? state target)))}
                 :req (req (and (pos? (count (:hand runner)))
                                (:runner-phase-12 @state)))
                 :async true
                 :effect (effect
                           (runner-install
                             (assoc eid :source card :source-type :runner-install)
                             target
                             {:facedown true
                              :msg-keys {:install-source card}}))}]
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
                                                     (assoc eid :source-type :runner-install) % {:no-toast true}))
                                             (all-cards-in-hand* state :runner))))
                     :async true
                     :effect (req (wait-for (runner-install state :runner
                                                            (assoc (make-eid state eid) :source card :source-type :runner-install)
                                                            (assoc-in target [:special :street-artist] true) {:msg-keys {:install-source card
                                                                                                                         :display-origin true}})
                                            (register-once state side {:once :per-turn} card)
                                            (register-events
                                              state side card
                                              [{:event :run-ends
                                                :interactive (req true)
                                                :duration :end-of-run
                                                :req (req (some #(get-in % [:special :street-artist]) (all-installed state :runner)))
                                                :async true
                                                :effect (req (doseq [program (filter #(get-in % [:special :street-artist]) (all-installed state :runner))]
                                                               (if (has-subtype? program "Trojan")
                                                                 (do (update! state :runner (dissoc-in program [:special :street-artist]))
                                                                     (effect-completed state side eid))
                                                                 (do
                                                                   (system-msg state side (str "uses " (:title card) " to trash " (:title program)))
                                                                   (trash-cards state side eid [program] {:cause-card card})))))}])
                                            (effect-completed state side eid)))}
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
                               :effect (effect (corp-install eid target (zone->name z) {:msg-keys {:install-source card
                                                                                                   :display-origin true}}))}
                              card nil)))}]})

(defcard "Ayla \"Bios\" Rahim: Simulant Specialist"
  {:abilities [{:action true
                :label "Add 1 hosted card to the grip"
                :cost [(->c :click 1)]
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
                                    (has-any-subtype? card ["Job" "Connection"]))))
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
                            (first-event? state :runner :runner-install
                                         #(is-type? (:card (first %)) (:card-target card)))
                            (not (:facedown context))))
             :async true
             :effect (effect (gain-credits :corp eid 2))
             :msg (msg "gain 2 [Credits] from " (:card-target card))}
            {:event :play-event
             :req (req (and (:card-target card)
                            (first-event? state :runner :play-event)
                            (is-type? (:card context) (:card-target card))))
             :async true
             :effect (effect (gain-credits :corp eid 2))
             :msg (msg "gain 2 [Credits] from " (:card-target card))}]})

(defcard "Barry \"Baz\" Wong: Tri-Maf Veteran"
  {:events [{:async true
             :prompt "Install a resource or piece of hardware"
             :event :rez
             :waiting-prompt true
             :player :runner
             :interactive (req true)
             :skippable true
             :req (req (ice? (:card context)))
             :change-in-game-state {:silent true :req (req (seq (all-cards-in-hand* state :runner)))}
             :choices {:req (req (and (in-hand*? state target)
                                      (or (resource? target) (hardware? target))
                                      (runner-can-pay-and-install? state side eid target)))}
             :effect (effect (runner-install (assoc eid :source card) target {:msg-keys {:install-source card}}))}]})

(defcard "BANGUN: When Disaster Strikes"
  {:abilities [{:label "Manually turn an agenda faceup"
	        :choices {:req (req (and (agenda? target)
                                         (installed? target)))}
		:msg (msg "turn " (card-str state target {:visible true}) " faceup")
                :effect (req (update! state side (assoc target
                                                        :seen true
                                                        :rezzed true)))}]
   :events [{:event :access
             :req (req ((every-pred faceup? installed? agenda? :was-seen) target))
             :interactive (req true)
             :msg (msg "do 2 meat damage and give the Runner a tag")
             :async true
             :effect (req (wait-for (damage state :corp :meat 2 {:card card :suppress-checkpoint true})
                                    (gain-tags state :corp eid 1)))}
            {:event :corp-install
             :interactive (req true)
             :skippable true
             ;; note, since this is a lot of stuff:
             ;; if it's ice or a condition counter, or already faceup, we don't care
             ;; and if it's on a central, we don't care
             ;; if there's already a faceup asset or agenda in the slot, we also don't care
             :req (req (and (not (ice? (:card target)))
                            (not (condition-counter? (:card target)))
                            (not (rezzed? (:card context)))
                            (not (contains? #{:hq :archives :rd} (second (get-zone (:card target)))))
                            (not (#{:face-up} (:install-state context)))
                            (let [cards-in-slot (filter #(= (:zone %) (:zone (:card context))) (all-installed state :corp))]
                              (not (some #(and (or (asset? %) (agenda? %))
                                               (or (rezzed? %) (:face-up %)))
                                         cards-in-slot)))))
             :async true
             :effect (req (let [tcard (:card target)]
                            (continue-ability
                              state side
                              (if (agenda? tcard)
                                {:optional
                                 {:prompt (str "Turn " (:title tcard) " faceup?")
                                  :waiting-prompt true
                                  :yes-ability {:msg (str "turn " (card-str state tcard {:visible true}) " faceup")
                                                :effect (req (update! state side (assoc tcard :seen true :rezzed true)))}}}
                                {:prompt "Nothing to see here"
                                 :waiting-prompt true
                                 :choices ["OK"]})
                              card nil)))}]})

(defcard "Blue Sun: Powering the Future"
  {:flags {:corp-phase-12 (req (and (not (:disabled card))
                                    (not (is-disabled? state side card))
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
             :async true
             :req (req (and (= (:server target) [:rd])
                            (first-event? state side :run #(= [:rd] (:server (first %))))))
             :effect (effect (continue-ability (charge-ability state side) card nil))}]})

(defcard "Cerebral Imaging: Infinite Frontiers"
  {:static-abilities [(corp-hand-size+ (req (- (:credit corp) 5)))]})

(defcard "Chaos Theory: Wünderkind"
  {:static-abilities [(mu+ 1)]})

(defcard "Chronos Protocol: Haas-Bioroid"
  {:events [{:event :damage
             :req (req (= (:damage-type context) :brain))
             :msg (msg "remove all copies of " (enumerate-str (map :title (:cards-trashed context))) ", everywhere, from the game")
             :effect (req (doseq [c (:cards-trashed context)
                                  :let [all-candidates (filter #(and (not (in-rfg? %))
                                                                     (runner? %)
                                                                     (= (:title %) (:title c)))
                                                               (get-all-cards state))]
                                  candidate all-candidates]
                            (move state :runner candidate :rfg)))}]})

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
             {:req (req (and (= target :net)
                             (corp-can-choose-damage? state)
                             (pos? (last targets))
                             (empty? (filter #(= :net (:damage-type (first %))) (turn-events state :runner :damage)))
                             (pos? (count (:hand runner)))))
              :waiting-prompt true
              :prompt "Choose the first card to trash?"
              :yes-ability (with-revealed-hand :runner {:no-event true}
                             {:prompt "Choose 1 card to trash"
                              :choices {:card (every-pred runner? in-hand?)}
                              :msg (msg "choose " (:title target) " to trash")
                              :effect (req (chosen-damage state :corp target))})
              :no-ability
              {:effect (req (system-msg state :corp (str "declines to use " (:title card))))}}}]})

(defcard "Cybernetics Division: Humanity Upgraded"
  {:static-abilities [(hand-size+ -1)]})

(defcard "Dewi Subrotoputri: Pedagogical Dhalang"
  (let [flip-effect {:effect (effect (update! (if (:flipped card)
                                                (assoc card
                                                       :flipped false
                                                       :face :front
                                                       :code (subs (:code card) 0 5))
                                                (assoc card
                                                       :flipped true
                                                       :face :back
                                                       :code (str (subs (:code card) 0 5) "flip")))))}
        maybe-flip {:event :successful-run
                    :skippable true
                    :interactive (req true)
                    :change-in-game-state
                               {:silent true
                                :req (req (or
                                            (and (:flipped card) (pos? (available-mu state)))
                                            (and (not (:flipped card)) (zero? (available-mu state)))))}
                    :optional {:prompt (msg "Flip your ID (" (if (:flipped card) "draw 1 card)?" "gain 1 [Credits])?"))
                               :yes-ability {:async true
                                             :effect (req
                                                       (if (:flipped card)
                                                         (if (pos? (available-mu state))
                                                           (wait-for
                                                             (draw state :runner 1)
                                                             (system-msg state side
                                                                         "draws 1 card and flips [their] identity to Dewi Subrotoputri: Pedagogical Dhalang")
                                                             (continue-ability state side flip-effect card targets))
                                                           (effect-completed state side eid))
                                                         (if (zero? (available-mu state))
                                                           (wait-for
                                                             (gain-credits state :runner 1)
                                                             (system-msg
                                                               state side
                                                               "gain 1 [Credits] and flips [their] identity to Dewi Subrotoputri: Shadow Guide")
                                                             (continue-ability state :runner flip-effect card nil))
                                                           (effect-completed state side eid))))}}}]
    {:events [{:event :pre-first-turn
               :req (req (= side :runner))
               :effect (effect (update! (assoc card :flipped false :face :front)))}
              maybe-flip]
     :abilities [(assoc flip-effect :label "Manually flip identity"
                        :force-menu true
                        :msg "manually flip [their] identity")]}))

(defcard "Earth Station: SEA Headquarters"
  (let [flip-effect (effect (update! (if (:flipped card)
                                       (do (system-msg state :corp "flipped [pronoun] identity to Earth Station: SEA Headquarters")
                                           (assoc card
                                                  :flipped false
                                                  :face :front
                                                  :code (subs (:code card) 0 5)))
                                       (assoc card
                                              :flipped true
                                              :face :back
                                              :code (str (subs (:code card) 0 5) "flip")))))]
    {:flags {:server-limit 1}
     :events [{:event :pre-first-turn
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
                         :value (req [(->c :credit (if (:flipped card) 6 1))])}]
     :async true
     :enforce-conditions {:req (req (< 1 (count (get-remotes state))))
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
     :abilities [{:action true
                  :label "Flip identity to Earth Station: Ascending to Orbit"
                  :req (req (not (:flipped card)))
                  :cost [(->c :click 1)]
                  :msg "flip [their] identity to Earth Station: Ascending to Orbit"
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
                                (swap! state assoc-in [:runner :register :trashed-accessed-card] true)
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
                 :async true
                 :effect (req (add-counter state side eid card :power 1 nil))}]
  {:events [(assoc ability :event :runner-trash :req (req (valid-trash target)))
            (assoc ability :event :agenda-stolen :req (req true))]
   :abilities [{:action true
                :label "Look at the top 3 cards of R&D"
                :change-in-game-state {:req (req (seq (:deck corp)))}
                :cost [(->c :click 1) (->c :power 1)]
                :msg "look at the top 3 cards of R&D"
                :async true
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
                                            :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                                                     :origin-index (first (keep-indexed #(when (same-card? target %2) %1) top))
                                                                                                     :display-origin true}}))}
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
     :trash? true
     :once :per-turn
     :label "Trash card"
     :req (req (and (not (:disabled card))
                    (not (is-disabled? state side card))
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
             :req (req (and (not (:disabled card))
                            (not (is-disabled? state side card))
                            (has-most-faction? state :corp "Weyland Consortium")))
             ;; TODO - ncigs change
             ;; :change-in-game-state {:silent true
             ;;                        :req (req (seq (filter ice? (all-installed state :corp))))}
             :prompt "Choose a piece of ice to place 1 advancement counter on"
             :choices {:card #(and (installed? %)
                                   (ice? %))}
             :msg (msg "place 1 advancement token on " (card-str state target))
             :async true
             :effect (req (add-prop state :corp eid target :advance-counter 1 {:placed true}))}]})

(defcard "Gabriel Santiago: Consummate Professional"
  {:events [{:event :successful-run
             :automatic :gain-credits
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            (first-successful-run-on-server? state :hq)))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits eid 2))}]})

(defcard "Gagarin Deep Space: Expanding the Horizon"
  {:events [{:event :pre-access-card
             :req (req (is-remote? (second (get-zone target))))
             :effect (effect (access-cost-bonus [(->c :credit 1)]))
             :msg "make the Runner spend 1 [Credits] to access"}]})

(defcard "GameNET: Where Dreams are Real"
  (let [gamenet-ability
        {:req (req (and (:run @state)
                        (= "Corp" (:side (:source eid)))
                        (or (not (#{:runner-trash-corp-cards :runner-steal} (:source-type eid)))
                            (some (fn [cost]
                                    (and (#{:credit :x-credit} (:cost/type cost))
                                         (= "Corp" (:side (:source (:cost/args cost))))))
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
                                       [(->c :credit (rez-cost state side target {:cost-bonus -4}))])))}
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
                                (system-msg state :corp (str "uses " (get-title card) " make the Runner play with [runner-pronoun] grip revealed"))
                                (system-msg state :corp (str "uses " (get-title card) " to see that the Runner currently has "
                                                             (format-grip runner) " in [runner-pronoun] grip"))
                              (reveal-hand state :runner))
                            (when (get-in @state [:runner :openhand])
                              (system-msg state :corp (str "uses " (get-title card) " stop making the Runner play with [runner-pronoun] grip revealed"))
                              (system-msg state :corp (str "uses " (get-title card) " to note that the Runner had "
                                                           (format-grip runner) " in [runner-pronoun] grip before it was concealed"))
                              (conceal-hand state :runner))))}]
   :effect (req (when (is-tagged? state)
                  (reveal-hand state :runner)))
   :leave-play (req (when (is-tagged? state)
                      (conceal-hand state :runner)))}))

(defcard "Harmony Medtech: Biomedical Pioneer"
  {:static-abilities [{:type :agenda-point-req
                       :value -1}]})

(defcard "Hayley Kaplan: Universal Scholar"
  {:events [{:event :runner-install
             :req (req (and (first-event? state side :runner-install)
                            (not (:facedown context))))
             :interactive (req (some #(card-flag? % :runner-install-draw)
                                     (all-installed state :runner)))
             :async true
             :waiting-prompt true
             :effect
             (effect (continue-ability
                       (let [itarget (:card context)
                             card-type (:type itarget)]
                         (if (some #(is-type? % (:type itarget)) (all-cards-in-hand* state :runner))
                           {:optional
                            {:prompt (str "Install another " card-type " from the grip?")
                             :yes-ability
                             {:prompt (str "Choose a " card-type " to install")
                              :choices {:req (req (and (is-type? target card-type)
                                                       (in-hand*? state target)))}
                              :async true
                              :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:msg-keys {:install-source card
                                                                                                                                       :display-origin true}}))}}}
                           {:prompt (str "You have no " card-type " to install")
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
               :automatic :gain-credits
               :interactive (req true)
               :async true
               :effect (req (cond
                              (and (:flipped card)
                                   (not (:accessed-cards runner-reg)))
                              (do (system-msg state :runner "flips [their] identity to Hoshiko Shiro: Untold Protagonist")
                                  (continue-ability state :runner {:effect flip-effect} card nil))
                              (and (not (:flipped card))
                                   (:accessed-cards runner-reg))
                              (wait-for (gain-credits state :runner 2)
                                        (system-msg state :runner "gains 2 [Credits] and flips [their] identity to Hoshiko Shiro: Mahou Shoujo")
                                        (continue-ability state :runner {:effect flip-effect} card nil))
                              :else
                              (effect-completed state side eid)))}
              {:event :runner-turn-begins
               :automatic :lose-credits
               :req (req (:flipped card))
               :async true
               :effect (req (wait-for (draw state :runner 1)
                                      (wait-for (lose-credits state :runner (make-eid state eid) 1)
                                                (system-msg state :runner (str "uses " (:title card) " to draw 1 card and lose 1 [Credits]"))
                                                (effect-completed state side eid))))}]
     :abilities [{:label "flip identity"
                  :msg "flip [their] identity manually"
                  :effect flip-effect}]}))

(defcard "Hyoubu Institute: Absolute Clarity"
  {:events [{:event :corp-reveal
             :req (req (letfn [(valid-ctx? [[{:keys [cards] :as ctx}]] (pos? (count cards)))]
                         (and (valid-ctx? [context])
                              (first-event? state side :corp-reveal valid-ctx?))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]
   :abilities [{:action true
                :cost [(->c :click 1)]
                :label "Reveal the top card of the Stack"
                :async true
                :effect (req (if-let [revealed-card (-> runner :deck first)]
                               (do (system-msg state side (str "uses " (:title card) " to reveal "
                                                               (:title revealed-card) " from the top of the Stack"))
                                   (reveal state side eid revealed-card))
                               (effect-completed state side eid)))}
               {:action true
                :cost [(->c :click 1)]
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
                 :automatic :gain-credits
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
                                     (not (is-disabled? state side card))
                                     (has-most-faction? state :corp "NBN")))
                      :interactive (req true)
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
   :static-abilities [{:type :agenda-point-req
                       :req (req (= :corp side))
                       :value (req (- (get-counters card :power)))}]
   :events [{:event :agenda-scored
             :interactive (req true)
             :req (req (and (->> (turn-events state side :corp-install)
                                 (map #(:card (first %)))
                                 (filter #(same-card? (:card context) %))
                                 empty?)
                            (->> (turn-events state side :advance)
                                 (map #(first %))
                                 (filter #(same-card? (:card context) (:card %)))
                                 empty?)))
             :msg "put 1 charge counter on itself"
             :async true
             :effect (req (add-counter state side eid card :power 1))}]})

(defcard "Jamie \"Bzzz\" Micken: Techno Savant"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-install
             :req (req (and (has-most-faction? state :runner "Shaper")
                            (first-event? state side :runner-install)))
             :msg "draw 1 card"
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
                    :async true
                    :effect (effect (add-prop :corp eid target :advance-counter p {:placed true}))})
                 card nil))}]})

(defcard "Jesminder Sareen: Girl Behind the Curtain"
  {:static-abilities [{:type :forced-to-avoid-tag
                       :req (req (and run (zero? (run-event-count state side :tag-interrupt))))
                       :value true}]
   :events [{:event :tag-interrupt
             :async true
             :req (req (and run (<= (run-event-count state side :tag-interrupt) 1)))
             :msg "avoid 1 tag"
             :effect (effect (prevent-tag :runner eid 1))}]})

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
               {:action true
                :cost [(->c :click 3)]
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
                                        :choices {:req (req (can-be-advanced? state target))}
                                        :async true
                                        :effect (effect (add-prop eid target :advance-counter 4 {:placed true}))}
                                       card nil))
                                 (do (toast state :corp (str "Unknown Jinteki Biotech: Life Imagined card: " flip) "error")
                                     (effect-completed state side eid)))))}]})

(defcard "Jinteki: Personal Evolution"
  (let [ability {:async true
                 :interactive (req true)
                 :msg "do 1 net damage"
                 :effect (effect (damage eid :net 1 {:card card}))}]
    {:events [(assoc ability :event :agenda-scored)
              (assoc ability :event :agenda-stolen)]}))

(defcard "Jinteki: Potential Unleashed"
  {:events [{:async true
             :event :damage
             :req (req (and
                         (= (:damage-type context) :net)
                         (pos? (:amount context))))
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
             :automatic :gain-credits
             :req (req (pos? (count (remove :seen (:discard corp)))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Kabonesa Wu: Netspace Thrillseeker"
  {:abilities [{:action true
                :label "Install a non-virus program from the stack, lowering the cost by 1 [Credit]"
                :cost [(->c :click 1)]
                :prompt "Choose a program"
                :change-in-game-state {:req (req (seq (:deck runner)))}
                :choices (req (cancellable
                                (filter #(and (program? %)
                                              (not (has-subtype? % "Virus"))
                                              (can-pay? state :runner (assoc eid :source card :source-type :runner-install) % nil
                                                        [(->c :credit (install-cost state side % {:cost-bonus -1}))]))
                                        (:deck runner))))
                :async true
                :effect (effect (trigger-event :searched-stack)
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
                                                {:cost-bonus -1
                                                 :msg-keys {:display-origin true
                                                            :include-cost-from-eid eid
                                                            :install-source card}}))}]})

(defcard "Kate \"Mac\" McCaffrey: Digital Tinker"
  ;; Effect marks Kate's ability as "used" if it has already met it's trigger condition this turn
  (letfn [(kate-type? [card] (or (hardware? card)
                                 (program? card)))
          (not-triggered? [state] (no-event? state :runner :runner-install #(kate-type? (:card (first %)))))]
    {:static-abilities [{:type :install-cost
                         :req (req (and (kate-type? target)
                                        (not-triggered? state)))
                         :value -1}]}))

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
                                                     [(->c :credit (install-cost state side % {:cost-bonus -1}))]))
                                     (:hand runner))
                           {:prompt "Choose an icebreaker to install"
                            :choices
                            {:req (req (and (in-hand*? state target)
                                            (has-subtype? target "Icebreaker")
                                            (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                      [(->c :credit (install-cost state side target {:cost-bonus -1}))])))}
                            :async true
                            :effect (effect (runner-install eid target {:cost-bonus -1
                                                                        :msg-keys {:display-origin true
                                                                                   :install-source card}}))})
                         card nil))}]})

(defcard "Laramy Fisk: Savvy Investor"
  {:events [{:event :successful-run
             :skippable true
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
             :async true
             :effect
             (effect (continue-ability
                       {:optional {:req (req (= (count (:hand runner)) (count (:hand corp))))
                        :autoresolve (get-autoresolve :auto-fire)
                        :waiting-prompt true
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
                                     (corp? %))
                         :all true}
               :change-in-game-state {:silent true
                                      :req (req (some #(and (not (faceup? %))
                                                            (installed? %))
                                                      (all-installed state :corp)))}
               :msg (msg "add " (card-str state target) " to HQ")
               :effect (effect (move :corp target :hand))}]
    {:events [(assoc leela :event :agenda-scored)
              (assoc leela :event :agenda-stolen)]}))

(defcard "LEO Construction: Labor Solutions"
  {:abilities [{:cost [(->c :bioroid-run-server 1)]
                :once :per-turn
                :label "end the run"
                :msg "end the run"
                :async true
                :effect (req (end-run state side eid card))}]})

(defcard "Liza Talking Thunder: Prominent Legislator"
  {:events [{:event :successful-run
             :automatic :draw-cards
             :async true
             :interactive (req true)
             :msg "draw 2 cards and take 1 tag"
             :req (req (and (is-central? (:server context))
                            (first-event? state side :successful-run
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (is-central? (:server context)))))))
             :effect (req (wait-for (draw state :runner 2 {:suppress-checkpoint true})
                                    (gain-tags state :runner eid 1)))}]})

(defcard "Los: Data Hijacker"
  {:events [{:event :rez
             :req (req (and (ice? (:card context))
                            (first-event? state side :rez #(ice? (:card (first %))))))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 2))}]})

(defcard "Magdalene Keino-Chemutai: Cryptarchitect"
  {:events [{:event :runner-discard-to-hand-size
             :async true
             :effect (req (let [installable (filterv (fn [c]
                                                       (and (or (hardware? c) (program? c))
                                                            (runner-can-pay-and-install?
                                                              state :runner eid c {:no-toast true})))
                                                     (:cards context))]
                            (if (seq installable)
                              (continue-ability
                                state side
                                {:prompt "Install a discarded program or piece of hardware?"
                                 :choices (req (cancellable installable :sorted))
                                 :async true
                                 :effect (req (runner-install state side eid target {:msg-keys {:install-source card
                                                                                                :display-origin true}}))}
                                card nil)
                              (effect-completed state side eid))))}]})

(defcard "MaxX: Maximum Punk Rock"
  (let [ability {:msg (msg (let [deck (:deck runner)]
                             (if (pos? (count deck))
                               (str "trash " (enumerate-str (map :title (take 2 deck))) " from the stack and draw 1 card")
                               "trash the top 2 cards from the stack and draw 1 card - but the stack is empty")))
                 :label "trash and draw cards"
                 :once :per-turn
                 :automatic :post-draw-cards
                 :async true
                 :effect (req (wait-for (mill state :runner :runner 2)
                                        (draw state :runner eid 1)))}]
    {:flags {:runner-turn-draw true
             :runner-phase-12 (req (and (not (:disabled card))
                                        (not (is-disabled? state side card))
                                        (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner))))}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Mercury: Chrome Libertador"
  {:events [{:event :breach-server
             :automatic :pre-breach
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
  (let [relevant-keys (fn [context] {:cid (get-in context [:card :cid])
                                     :idx (:ability-idx context)})
        mm-clear {:prompt "Manually fix Mirrormorph"
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
     :events [{:event :action-resolved
               :async true
               :req (req (= :corp side))
               :effect (req (let [ctx-keys (relevant-keys context)
                                  prev-actions (get-in card [:special :mm-actions] [])
                                  actions (conj prev-actions ctx-keys)]
                              (update! state side (assoc-in card [:special :mm-actions] actions))
                              (update! state side (assoc-in (get-card state card) [:special :mm-click] false))
                              (if (and (= 3 (count actions))
                                       (= 3 (count (distinct actions))))
                                (continue-ability state side mm-ability (get-card state card) nil)
                                (effect-completed state side eid))))}
              {:event :runner-turn-begins
               :silent (req true)
               :effect (effect
                        (update! (assoc-in card [:special :mm-actions] []))
                        (update! (assoc-in (get-card state card) [:special :mm-click] false)))}
              {:event :corp-turn-ends
               :silent (req true)
               :effect (effect
                        (update! (assoc-in card [:special :mm-actions] []))
                        (update! (assoc-in (get-card state card) [:special :mm-click] false)))}]
     :static-abilities [{:type :prevent-paid-ability
                         :req (req (and (get-in card [:special :mm-click])
                                        (let [ctx {:cid (:cid target)
                                                   :idx (nth targets 2 nil)}
                                              prev-actions (get-in card [:special :mm-actions] [])
                                              actions (conj prev-actions ctx)]
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
                                :msg "install a piece of ice from HQ at the innermost position of this server. Runner is now approaching that piece of ice"
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
                              :prompt "You have no piece of ice to install"
                              :choices ["Carry on!"]
                              :prompt-type :bogus
                              :effect (effect (effect-completed eid))}
                             card nil)))}]})

(defcard "MuslihaT: Multifarious Marketeer"
  {:events [{:event :runner-turn-begins
             :req (req (seq (:deck runner)))
             :msg (msg "look at the top card of the stack")
             :async true
             :effect (req (let [top-card (first (:deck runner))]
                            (continue-ability
                              state side
                              (if (or (and (event? top-card) (has-subtype? top-card "Run"))
                                      (and (program? top-card) (has-subtype? top-card "Icebreaker")))
                                {:optional
                                 {:prompt (str "Add " (:title top-card) " to the grip?")
                                  :waiting-prompt true
                                  :yes-ability {:async true
                                                :effect (req (wait-for
                                                               (reveal-loud state side card {:and-then " and add it to the grip"} top-card)
                                                               (move state side top-card :hand)
                                                               (effect-completed state side eid)))}}}
                                {:prompt (str "The top card of the stack is " (:title top-card))
                                 :choices ["OK"]
                                 :waiting-prompt true
                                 :async true})
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
                 :automatic :pre-draw-cards
                 :effect (req (if (and (> 3 (count (:hand runner)))
                                       (:runner-phase-12 @state))
                                (do (system-msg state :runner (str "uses " (:title card) " to gain 1 [Credits]"))
                                    (gain-credits state :runner eid 1))
                                (effect-completed state side eid)))}]
    {:flags {:drip-economy true
             :runner-phase-12 (req (and (not (:disabled card))
                                        (not (is-disabled? state side card))
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

(defcard "Nebula Talent Management: Making Stars"
  (let [flip-effect
        {:effect (effect (update! (if (:flipped card)
                                    (assoc card
                                           :flipped false
                                           :face :front
                                           :code (subs (:code card) 0 5))
                                    (assoc card
                                           :flipped true
                                           :face :back
                                           :code (str (subs (:code card) 0 5) "flip")))))}]
    {:abilities [(assoc flip-effect
                        :label "Manually flip identity"
                        :msg "Manually flip identity"
                        :force-menu true)]
     :events [{:event :pre-first-turn
               :req (req (= side :corp))
               :effect (effect (update! (assoc card :flipped false :face :front)))}
              {:event :corp-turn-ends
               :req (req (and (not (no-event? state side :play-operation))
                              (not (:flipped card))))
               :msg (msg "flip [their] identity to Gemilang Arena: Burning Bright and gain 1 [Credits]")
               :async true
               :effect (req (wait-for (gain-credits state side 1)
                                      (continue-ability state side flip-effect card nil)))}
              {:event :successful-run
               :req (req (and (or (= :rd (target-server context)) (= :hq (target-server context)))
                              (:flipped card)))
               :msg (msg "flip [their] identity to Nebula Talent Management: Making Stars")
               :async true
               :effect (req (continue-ability state side flip-effect card targets))}
              {:event :play-operation-resolved
               :req (req (and (first-event? state side :play-operation-resolved)
                              (not (has-subtype? (:card context) "Terminal"))
                              (:flipped card)))
               :interactive (req true)
               :msg (msg "gain [click]")
               :effect (req (gain-clicks state :corp 1))}]}))

(defcard "Nero Severn: Information Broker"
  {:events [{:event :encounter-ice
             :skippable true
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
                             :async true
                             :effect (req (wait-for (corp-install state side target nil {:msg-keys {:install-source card
                                                                                                    :display-origin true}})
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
             :interactive (req true)
             :req (req (has-subtype? (:card context) "Virus"))
             :msg "force the Corp to trash the top card of R&D"
             :effect (effect (mill :corp eid :corp 1))}]})

(defcard "Null: Whistleblower"
  {:events [{:event :encounter-ice
             :skippable true
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
  (let [abi2 {:event :corp-trash
              :req (req (and (= :corp (:active-player @state))
                             (= [:deck] (:zone (:card target)))
                             (first-event? state side :corp-trash #(= [:deck] (:zone (:card (first %)))))))
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
  {:events [mark-changed-event
            (assoc identify-mark-ability :event :runner-turn-begins)
            {:event :successful-run
             :automatic :gain-clicks
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
                (let [add-costs (rez-additional-cost-bonus state side target #(not (= :credit (:cost/type %))))
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
    {:abilities [(choose-one-helper
                   {:label "Always pause at start of turn"}
                   [{:option "Always pause at turn start"
                     :ability {:effect (req (update! state side (assoc-in card [:special :pause-at-phase-12] true))
                                            (toast state :corp "The game will always pause at the start of the turn"))}}
                    {:option "Only if triggered by cards in play"
                     :ability {:effect (req (update! state side (dissoc-in card [:special :pause-at-phase-12]))
                                            (toast state :corp "The game only pause at turn start if triggered by cards in play"))}}])]
     :flags {:corp-phase-12 (req (get-in card [:special :pause-at-phase-12]))}
     :events [{:event :corp-trash
               :req (req (and
                           (installed? (:card context))
                           (not (:during-installation context))
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
  {:abilities [(run-server-ability
                 :archives
                 {:action true
                  :cost [(->c :click 1)]
                  :once :per-turn
                  :events [{:event :pre-successful-run
                            :interactive (req true)
                            :duration :end-of-run
                            :unregister-once-resolved true
                            :req (req (= :archives (-> run :server first)))
                            :prompt "Choose one"
                            :choices ["HQ" "R&D"]
                            :msg (msg "change the attacked server to " target)
                            :effect (req (let [target-server (if (= target "HQ") :hq :rd)]
                                           (swap! state assoc-in [:run :server] [target-server])))}]})]})

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

(defcard "Poétrï Luxury Brands: All the Rage"
  (let [remote-choice
        (fn [chosen]
          {:async true
           :waiting-prompt true
           :effect (req (let [target-position (first (positions #{chosen} (take 3 (:deck corp))))]
                          (corp-install state side eid chosen nil
                                        {:msg-keys {:install-source card
                                                    :origin-index target-position
                                                    :display-origin true}})))})
        opts-fn (fn [cards]
                  (mapv #(when (and (not (operation? %))
                                    (not (agenda? %)))
                           {:option (str "Install " (:title %))
                            :ability (remote-choice %)})
                        cards))
        ev {:prompt (msg "The top of R&D is (in order): "
                         (enumerate-str (map :title (take 3 (:deck corp)))))
            :async true
            :msg (msg "look at the top 3 cards of R&D")
            :effect (req (let [top-3 (take 3 (:deck corp))]
                           (continue-ability
                             state side
                             (choose-one-helper
                               {:prompt (str "The top of R&D is (in order): "
                                             (enumerate-str (map :title top-3)))
                                :optional true}
                               (opts-fn top-3))
                             card nil)))}
        score-ev {:event :agenda-scored
                  :skippabe true
                  :interactive (req true)
                  :optional {:prompt "Look at the top 3 cards of R&D?"
                             :req (req (seq (:deck corp)))
                             :yes-ability ev}}]
    {:events [{:event :agenda-stolen
               :interactive (req true)
               :skippable true
               :async true
               :prompt "Install a non-agenda from HQ?"
               :change-in-game-state {:silent true :req (req (seq (:hand corp)))}
               :waiting-prompt true
               :choices {:card (every-pred corp? in-hand? (complement agenda?) (complement operation?))}
               :effect (req (corp-install state side eid target nil {:msg-keys {:install-source card}}))}
              score-ev]}))

(defcard "Pravdivost Consulting: Political Solutions"
  {:events [{:event :successful-run
             :skippable true
             :req (req (first-event? state side :successful-run))
             :interactive (req true)
             :async true
             :waiting-prompt true
             :prompt "Choose a card that can be advanced to place 1 advancement counter on"
             :choices {:req (req (and (installed? target) (can-be-advanced? state target)))}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (effect (add-prop :corp eid target :advance-counter 1 {:placed true}))
             :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                    (effect-completed eid))}]})

(defcard "PT Untaian: Life's Building Blocks"
  {:events [{:event :corp-turn-ends
             :interactive (req true)
             :skippable true
             :req (req (<= (count (:hand corp)) 3))
             :change-in-game-state {:silent true
                                    :req (req (some #(or ((complement rezzed?) %)
                                                         (can-be-advanced? state %))
                                                    (all-installed state :corp)))}
             :prompt "Pay 1 [Credits]: place 1 advancement counter on an unrezzed advanceable card?"
             :waiting-prompt true
             :choices {:req (req (and (installed? target)
                                      (not (rezzed? target))
                                      (can-be-advanced? state target)))}
             :cost [(->c :credit 1)]
             :async true
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (add-prop state side eid target :advance-counter 1 {:placed true}))}]})

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
             :req (req (first-event? state side :encounter-ice))
             :msg (msg "make " (:title (:ice context))
                       " gain Code Gate until the end of the run")
             :effect (effect (register-lingering-effect
                               card
                               (let [ice (:ice context)]
                                 {:type :gain-subtype
                                  :duration :end-of-run
                                  :req (req (same-card? ice target))
                                  :value "Code Gate"})))}]})

(defcard "Ryō \"Phoenix\" Ōno: Out of the Ashes"
  {:events [{:event :successful-run
             :req (req (letfn [(valid-ctx? [[ctx]] (pos? (or (:subroutines-fired ctx) 0)))]
                         (and (valid-ctx? [context])
                              (first-event? state side :successful-run valid-ctx?))))
             :interactive (req true)
             :automatic :force-discard
	     :msg "gain 1 [Credits]"
             :async true
             :once :per-turn
             :effect (req (wait-for (gain-credits state side 1)
                                    (if-not (seq (:hand corp))
                                      (effect-completed state side eid)
                                      (continue-ability
	                                state :corp
                                        {:display-side :corp
                                         :waiting-prompt true
                                         :player :corp
                                         :cost [(->c :trash-from-hand 1)]
                                         :msg :cost}
                                        card nil))))}]})

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
                                  true)))
                            (corp-install state side eid chosen target {:counters {:advance-counter 1}
                                                                        :msg-keys {:install-source card
                                                                                   :display-origin true}})))})]
    {:abilities [{:action true
                  :async true
                  :label "Install a card from HQ"
                  :cost [(->c :click 1) (->c :credit 1)]
                  :change-in-game-state {:req (req (seq (:hand corp)))}
                  :prompt "Choose a card to install from HQ"
                  :choices {:card #(and (or (asset? %) (agenda? %) (upgrade? %))
                                        (corp? %)
                                        (in-hand? %))}
                  :msg (msg "install a card in a remote server and place 1 advancement token on it")
                  :effect (effect (continue-ability (install-card target) card nil))}]
     :events [{:event :corp-turn-begins
               :silent (req true)
               :effect (req (clear-persistent-flag! state side card :can-rez))}]}))

(defcard "Sebastião Souza Pessoa: Activist Organizer"
  {:static-abilities [{:type :basic-ability-additional-trash-cost
                       :req (req (and (resource? target) (has-subtype? target "Connection") (= :corp side)))
                       :value [(->c :trash-from-hand 1)]}]
   :events [{:event :runner-gain-tag
             :async true
             :req (req (and (not (install-locked? state side))
                            (= (:amount context) (count-tags state)))) ;; every tag is one that was just gained
             :prompt "Choose a connection to install, paying 2 [Credits] less"
             :choices
             {:req (req (and (has-subtype? target "Connection")
                             (resource? target)
                             (in-hand*? state target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [(->c :credit (install-cost state side target {:cost-bonus -2}))])))}
             :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -2
                                                                                                           :msg-keys {:display-origin true
                                                                                                                      :install-source card}}))}]})

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
             :skippable true
             :interactive (req (some #(not (rezzed? %)) (all-installed state :corp)))
             :async true
             :req (req (and (= :hq (target-server context))
                            (first-successful-run-on-server? state :hq)))
             :choices {:card #(and (installed? %)
                                   (not (rezzed? %)))}
             :effect (effect (expose eid [target]))}]})

(defcard "Skorpios Defense Systems: Persuasive Power"
  (let [set-resolution-mode
        (fn [x] {:label x
                 :effect (req (update! state side (assoc-in card [:special :resolution-mode] x))
                              (toast state :corp (str "Set Skorpios resolution to " x " mode"))
                              (update! state side (assoc (get-card state card) :card-target x)))})
        grip-or-stack-trash?
        (fn [ctx]
          (some #(and (runner? (:card %))
                      (or (in-hand? (:card %))
                          (in-deck? (:card %))))
                ctx))
        relevant-cards-general #{"Labor Rights" "The Price"}
        relevant-cards-trashed #{"I've Had Worse" "Strike Fund" "Steelskin Scarring" "Crowdfunding"}
        trigger-ability-req (req (let [res-type (get-in (get-card state card) [:special :resolution-mode])
                                       valid-cards (mapv #(get-card state %) (filter runner? context))]
                                   (and (some runner? context)
                                        (cond
                                          ;; manual: do nothing
                                          (= res-type "Automatic") true
                                          ;; there's either:
                                          (= res-type "Smart")
                                          (or
                                            ;; 1) a relevant card resoluton
                                            (contains? relevant-cards-general (->> runner :play-area first :title))
                                            ;; 2) a buffer drive that may resolve
                                            (and (some #(= (:title %) "Buffer Drive") (all-installed state :runner))
                                                 (grip-or-stack-trash? (map (fn [x] {:card x}) context))
                                                 (zero? (+ (event-count state nil :runner-trash grip-or-stack-trash?)
                                                           (event-count state nil :corp-trash   grip-or-stack-trash?)
                                                           (event-count state nil :game-trash   grip-or-stack-trash?))))
                                            ;; 3) a program among the trashed cards
                                            (some #(->> % program?) context)
                                            ;; 4) a relevant card is trashed (Steelskin, Strike fund, I've Had Worse)
                                            (some #(contains? relevant-cards-trashed %) (map #(->> % :title) context)))
                                          :else nil))))
        triggered-ability {:once :per-turn
                           :player :corp
                           :event :pre-trash-interrupt
                           :waiting-prompt true
                           :req trigger-ability-req
                           :prompt "Remove a card from the game?"
                           :choices (req (cancellable context))
                           :msg (msg "remove " (:title target) " from the game")
                           :async true
                           :effect (req (move state :runner target :rfg)
                                        (effect-completed state side eid))}]
    {:implementation "Switch between Manual, \"Smart\", and Automatic resolution by using the ability on the card"
     :events [(assoc (set-resolution-mode "Smart")
                     :event :pre-first-turn
                     :req (req (= side :corp)))
              triggered-ability]
     :abilities [(choose-one-helper
                   {:optional true
                    :label "Set resolution mode"}
                   (mapv (fn [x] {:option x :ability (set-resolution-mode x)}) ["Manual" "Smart" "Automatic"]))
                 {:label "Remove a card in the Heap that was just trashed from the game"
                  :waiting-prompt true
                  :prompt "Choose a card in the Heap that was just trashed"
                  :once :per-turn
                  :choices (req (cancellable (:discard runner)))
                  :msg (msg "remove " (:title target) " from the game")
                  :effect (req (move state :runner target :rfg))}]}))

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
                                   :async true
                                   :effect (effect (add-prop eid target :advance-counter agenda-points {:placed true}))}
                                  card nil)))}}}]
     :abilities [(set-autoresolve :auto-fire "SSO Industries: Fueling Innovation")]}))

(defcard "Steve Cambridge: Master Grifter"
  {:events [{:event :successful-run
             :skippable true
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
                            (not (is-disabled? state side card))
                            (has-most-faction? state :corp "Haas-Bioroid")))
             :async true
             :effect (req (if (empty? (:discard corp))
                            (do (shuffle-cards-into-deck! state :corp card [])
                                (effect-completed state side eid))
                            (continue-ability
                              state side
                              {:prompt "Choose a card in Archives to shuffle into R&D"
                               :choices {:card #(and (corp? %)
                                                     (in-discard? %))
                                         :all true}
                               :player :corp
                               :show-discard true
                               :effect (req (shuffle-cards-into-deck! state :corp card [target]))}
                              card nil)))}]})

(defcard "Sunny Lebeau: Security Specialist"
  ;; No special implementation
  {})

(defcard "SYNC: Everything, Everywhere"
  {:static-abilities [{:type :card-ability-cost
                       :req (req (and (not (:sync-flipped card))
                                      (same-card? (:card context) (:basic-action-card runner))
                                      (= "Remove 1 tag" (:label (:ability context)))))
                       :value (->c :credit 1)}
                      {:type :card-ability-cost
                       :req (req (and (:sync-flipped card)
                                      (same-card? (:card context) (:basic-action-card corp))
                                      (= "Trash 1 resource if the Runner is tagged" (:label (:ability context)))))
                       :value (->c :credit -2)}]
   :abilities [{:action true
                :cost [(->c :click 1)]
                :effect (req (if (:sync-flipped card)
                               (update! state side (-> card (assoc :sync-flipped false :face :front :code "09001")))
                               (update! state side (-> card (assoc :sync-flipped true :face :back :code "sync")))))
                :label "Flip this identity"
                :msg (msg "flip [their] identity")}]})

(defcard "Synapse Global: Faster than Thought"
  {:events [{:event :runner-lose-tag
             :req (req (letfn [(valid-ctx? [[ctx]] (pos? (:amount ctx)))]
                         (and (valid-ctx? targets)
                              (first-event? state side :runner-lose-tag valid-ctx?))))
             :prompt "Reveal and install a card from HQ?"
             :change-in-game-state {:req (req (seq (:hand corp))) :silent true}
             :choices {:req (req (and (corp? target)
                                      (in-hand? target)
                                      (not (operation? target))))}
             :async true
             :effect (req (wait-for (reveal-loud state side card nil [target])
                                    (corp-install state side eid target nil {:ignore-install-cost true
                                                                             :msg-keys {:install-source card}})))}]
   :abilities [{:label "Gain 2 [Credits]"
                :action :true
                :async true
                :cost [(->c :tag 1) (->c :click 1)]
                :effect (req (gain-credits state side eid 2))}]})

(defcard "Synthetic Systems: The World Re-imagined"
  (let [abi {:prompt "Choose 2 installed pieces of ice to swap"
             :label "swap 2 installed pieces of ice"
             :choices {:card #(and (installed? %)
                                   (ice? %))
                       :max 2
                       :all true}
             :once :per-turn
             :effect (req (apply swap-ice state side targets))
             :msg (msg "swap the positions of " (card-str state (first targets))
                       " and " (card-str state (second targets)))}]
    {:events [{:event :pre-start-game
               :effect draft-points-target}
              {:events :corp-turn-begins
               :optional {:req (req (and (has-most-faction? state :corp "Jinteki")
                                         (<= 2 (count (filter ice? (all-installed state :corp))))))
                          :prompt "Swap two ice?"
                          :waiting-prompt true
                          :yes-ability abi}}]
     :flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                      (not (is-disabled? state side card))
                                      (has-most-faction? state :corp "Jinteki")
                                      (<= 2 (count (filter ice? (all-installed state :corp))))))}
     :abilities [abi]}))

(defcard "Tāo Salonga: Telepresence Magician"
  (let [swap-ability
        {:interactive (req true)
         :change-in-game-state {:silent true :req (req (<= 2 (count (filter ice? (all-installed state :corp)))))}
         :optional
         {:prompt "Swap 2 pieces of ice?"
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
                                    (not (is-disabled? state side card))
                                    (not-last-turn? state :runner :successful-run)))}
   :abilities [{:msg (msg "place 1 advancement token on " (card-str state target))
                :label "Place 1 advancement token on a card if the Runner did not make a successful run last turn"
                :choices {:card installed?}
                :req (req (and (:corp-phase-12 @state)
                               (not-last-turn? state :runner :successful-run)))
                :once :per-turn
                :async true
                :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}]})

(defcard "The Catalyst: Convention Breaker"
  ;; No special implementation
  {})

(defcard "The Collective: Williams, Wu, et al."
  (let [gain-click-abi {:label "Manually gain [Click]"
                        :once :per-turn
                        :msg (msg "gain [Click]")
                        :effect (req (gain-clicks state side 1))}
        relevant-keys (fn [context] {:cid (get-in context [:card :cid])
                                     :idx (:ability-idx context)})]
    {:events [{:event :action-resolved
               :req (req (= :runner side))
               :silent (req true)
               :async true
               :effect (req (let [current-queue (get-in card [:special :previous-actions])
                                  filtered-context (relevant-keys context)]
                              (if (and (seq current-queue)
                                       (= (first current-queue) filtered-context))
                                (let [new-queue (concat current-queue [filtered-context])]
                                  (update! state side (assoc-in card [:special :previous-actions] new-queue))
                                  (if (= 3 (count new-queue))
                                    (continue-ability state side gain-click-abi card nil)
                                    (effect-completed state side eid)))
                                (do (update! state side (assoc-in card [:special :previous-actions] [filtered-context]))
                                    (effect-completed state side eid)))))}
              {:event :runner-turn-begins
               :silent (req true)
               :effect (req (update! state side (assoc-in card [:special :previous-actions] nil)))}]}))

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

(defcard "The Zwicky Group: Invisible Hands"
  {:events [{:event :corp-credit-gain
             :async true
             :req (req (letfn [(valid-ctx? [[ctx]]
                                 (or (agenda? (:source ctx))
                                     (operation? (:source ctx))))]
                         (and (valid-ctx? targets) (first-event? state side :corp-credit-gain valid-ctx?))))
             :effect (effect (maybe-draw eid card 1))}]})

(defcard "Thule Subsea: Safety Below"
  {:events [{:event :agenda-stolen
             :async true
             :effect (effect (continue-ability
                               {:prompt "Choose one"
                                :player :runner
                                :choices (req [(when (can-pay? state :runner eid card nil [(->c :credit 2) (->c :click 1)])
                                                 "Pay [Click] and 2 [Credits]")
                                               "Suffer 1 core damage"])
                                :async true
                                :waiting-prompt true
                                :msg (msg (if (= target "Pay [Click] and 2 [Credits]")
                                            (str "force the runner to " (decapitalize target))
                                            "do 1 core damage"))
                                :effect (req (if (= target "Pay [Click] and 2 [Credits]")
                                               (wait-for (pay state side (make-eid state eid) card [(->c :click 1) (->c :credit 2)])
                                                         (system-msg state side (:msg async-result))
                                                         (effect-completed state :runner eid))
                                               (damage state side eid :brain 1 {:card card})))}
                               card nil))}]})

(defcard "Thunderbolt Armaments: Peace Through Power"
  (let [thunderbolt-sub
        {:player :runner
         :async true
         :label "End the run unless the Runner trashes 1 of their installed cards"
         :prompt "Choose one"
         :waiting-prompt true
         :choices (req ["End the run"
                        (when (can-pay? state :runner eid card nil [(->c :trash-installed 1)])
                          (capitalize (cost->string (->c :trash-installed 1))))])
         :msg (msg (if (= "End the run" target)
                     (decapitalize target)
                     (str "force the runner to " (decapitalize target))))
         :effect (req (if (= "End the run" target)
                        (end-run state :corp eid card)
                        (wait-for (pay state :runner (make-eid state eid) card (->c :trash-installed 1))
                                  (when-let [payment-str (:msg async-result)]
                                    (system-msg state :runner
                                                (str payment-str
                                                     " due to " (:title card)
                                                     " subroutine")))
                                  (effect-completed state side eid))))}]
    {:events [{:event :rez
               :req (req (and run
                              (ice? (:card context))
                              (or (has-subtype? (:card context) "AP")
                                  (has-subtype? (:card context) "Destroyer"))))
               :msg (msg "give " (card-str state (:card context))
                         " +1 strength and \""
                         (:label thunderbolt-sub)
                         "\" after its other subroutines")
               :async true
               :effect (effect (register-lingering-effect
                                 card
                                 (let [t (:card context)]
                                   {:type :additional-subroutines
                                    :duration :end-of-run
                                    :req (req (and (rezzed? target)
                                                   (same-card? t target)))
                                    :value {:subroutines [thunderbolt-sub]}}))
                               (pump-ice (:card context) 1 :end-of-run)
                               (effect-completed eid))}]}))

(defcard "Titan Transnational: Investing In Your Future"
  {:events [{:event :agenda-scored
             :msg (msg "place 1 agenda counter on " (:title (:card context)))
             :async true
             :effect (effect (add-counter eid (:card context) :agenda 1 nil))}]})

(defcard "Topan: Ormas Leader"
  (letfn [(installable? [state side eid target]
            (and (in-hand*? state target)
                 (or (hardware? target)
                     (resource? target)
                     (program? target))
                 (runner-can-pay-and-install? state side eid target {:cost-bonus -2})))]
  {:abilities [{:cost [(->c :click 1)]
                :action true
                :once :per-turn
                :async true
                :prompt "Install a card, paying 2 [Credits] less"
                :waiting-prompt true
                :choices {:req (req (installable? state side eid target))}
                :label "Install 1 card from your grip, paying 2{c} less. When you install that card, suffer 1 meat damage."
                :effect (req (let [evs (register-events
                                         state side card
                                         [{:event :runner-install
                                           :unregister-once-resolved true
                                           :async true
                                           :interactive (req true)
                                           :msg "suffer 1 meat damage"
                                           :effect (req (damage state side eid :meat 1))}])]
                               (wait-for
                                 (runner-install state side target {:cost-bonus -2
                                                                    :msg-keys {:include-cost-from-eid eid
                                                                               :install-source card}})
                                 ;; just incase the install failed for some reason!
                                 (unregister-event-by-uuid state side (:uuid (first evs)))
                                 (effect-completed state side eid))))}]}))

(defcard "Valencia Estevez: The Angel of Cayambe"
  {:events [{:event :pre-start-game
             :req (req (and (= side :runner)
                            (zero? (count-bad-pub state))))
             ;; This doesn't use `gain-bad-publicity` to avoid the event
             :effect (effect (gain :corp :bad-publicity 1))}]})

(defcard "Weyland Consortium: Because We Built It"
  {:recurring 1
   :interactions {:pay-credits {:req (req (let [ab-target (:card (get-ability-targets eid))]
                                            (and (ice? ab-target)
                                                 (or (= :advance (:source-type eid))
                                                     (is-basic-advance-action? eid)))))
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
                        (- (get-counters (:card context) :advancement) (:amount context 0))))
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
                            (corp? (:card target))))
             :effect (req (shuffle-cards-into-deck! state :runner card [(last (:discard runner))]))}]})

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
