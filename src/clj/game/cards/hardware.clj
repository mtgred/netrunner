(ns game.cards.hardware
  (:require
   [clojure.set :as set]
   [game.core.access :refer [access-bonus access-card access-n-cards breach-server
                             get-only-card-to-access]]
   [game.core.actions :refer [play-ability]]
   [game.core.board :refer [all-active all-active-installed all-installed]]
   [game.core.card :refer [agenda? corp? event? facedown? get-card get-counters get-title
                           get-zone hardware? has-subtype? has-any-subtype? ice? in-deck? in-discard?
                           in-hand? in-scored? installed? is-type? program? resource? rezzed?
                           runner? virus-program? faceup?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [install-cost rez-additional-cost-bonus rez-cost trash-cost]]
   [game.core.damage :refer [chosen-damage damage
                             enable-runner-damage-choice runner-can-choose-damage?]]
   [game.core.def-helpers :refer [all-cards-in-hand* in-hand*? breach-access-bonus defcard draw-abi offer-jack-out play-tiered-sfx
                                  reorder-choice run-any-server-ability spend-credits take-credits trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [any-effects register-lingering-effect
                              unregister-effect-by-uuid unregister-effects-for-card unregister-lingering-effects]]
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.engine :refer [can-trigger? not-used-once? register-events
                             register-once register-suppress resolve-ability trigger-event
                             unregister-floating-events unregister-suppress-by-uuid]]
   [game.core.events :refer [event-count first-event? first-run-event? first-trash? no-event?
                             run-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-card find-latest]]
   [game.core.flags :refer [can-trash? card-flag? in-corp-scored? register-run-flag!
                            zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [all-subs-broken? any-subs-broken? auto-icebreaker break-sub pump
                          reset-all-ice update-all-ice update-all-icebreakers
                          update-breaker-strength]]
   [game.core.installing :refer [runner-can-pay-and-install? runner-install]]
   [game.core.link :refer [get-link link+]]
   [game.core.memory :refer [caissa-mu+ expected-mu mu+ update-mu virus-mu+]]
   [game.core.moving :refer [as-agenda mill move swap-agendas trash trash-cards]]
   [game.core.optional :refer [get-autoresolve never? set-autoresolve]]
   [game.core.payment :refer [build-cost-string can-pay? cost-value ->c]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prevention :refer [damage-name damage-type preventable? prevent-damage prevent-encounter prevent-end-run prevent-tag prevent-up-to-n-damage]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [can-pay-to-rez? derez rez]]
   [game.core.runs :refer [bypass-ice end-run
                           get-current-encounter jack-out make-run
                           successful-run-replace-breach total-cards-accessed]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.servers :refer [target-server is-central?]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [count-virus-programs]]
   [game.core.winning :refer [win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [game.core.set-aside :refer [set-aside get-set-aside]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.mark :refer [identify-mark-ability mark-changed-event]]))

;; Card definitions

(defcard "Acacia"
  {:events [{:event :purge
             :optional
             {:waiting-prompt true
              :prompt "Trash Acacia to gain 1 [Credits] for each purged virus counter?"
              :yes-ability
              {:async true
               :effect (req (let [counters (:total-purged-counters context)]
                              (wait-for (trash state side card {:cause-card card})
                                        (system-msg state side (str "trashes Acacia and gains " counters " [Credit]"))
                                        (gain-credits state side eid counters))))}}}]})

(defcard "Adjusted Matrix"
  {:implementation "Click Adjusted Matrix to use the ability"
   :on-install {:req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
                :prompt "Choose an icebreaker"
                :choices {:card #(and (runner? %)
                                      (has-subtype? % "Icebreaker")
                                      (installed? %))}
                :msg (msg "host itself on " (card-str state target))
                :effect (effect (host (get-card state target) (get-card state card)))}
   :static-abilities [{:type :gain-subtype
                       :req (req (same-card? target (:host card)))
                       :value "AI"}]
   :abilities [(break-sub [(->c :lose-click 1)] 1 "All" {:req (req true)})]})

(defcard "AirbladeX (JSRF Ed.)"
  {:data {:counter {:power 3}}
   :prevention [{:prevents :damage
                 :type :ability
                 :ability {:async true
                           :cost [(->c :power 1)]
                           :msg "prevent 1 net damage"
                           :req (req (and run
                                          (= :net (:type context))
                                          (preventable? context)))
                           :effect (req (prevent-damage state side eid 1))}}
                {:prevents :encounter
                 :type :ability
                 :ability {:async true
                           :cost [(->c :power 1)]
                           :req (req (pos? (:remaining context)))
                           :msg (msg "prevent the encounter ability on " (:title current-ice))
                           :effect (req (prevent-encounter state side eid))}}]
   :events [(trash-on-empty :power)]})

(defcard "Akamatsu Mem Chip"
  {:static-abilities [(mu+ 1)]})

(defcard "Alarm Clock"
  (let [ability {:once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :msg "make a run on HQ"
                 :makes-run true
                 :async true
                 :effect (req (register-events
                                  state side card
                                  [{:event :encounter-ice
                                    :skippable true
                                    :unregister-once-resolved true
                                    :duration :end-of-run
                                    :optional
                                    {:prompt "Spend [Click][Click] to bypass encountered ice?"
                                     :req (req (first-run-event? state side :encounter-ice))
                                     :yes-ability {:cost [(->c :click 2)]
                                                   :req (req (>= (:click runner) 2))
                                                   :msg (msg "bypass " (card-str state (:ice context)))
                                                   :effect (req (bypass-ice state))}}}])
                              (make-run state side eid :hq card))}]
    {:flags {:runner-phase-12 (req true)}
     :events [{:event :runner-turn-begins
               :skippable true
               :interactive (req true)
               :optional
               {:once :per-turn
                :prompt "Make a run on HQ?"
                :yes-ability ability}}]
     :abilities [ability]}))

(defcard "Amanuensis"
  {:static-abilities [(mu+ 1)]
   :events [{:event :runner-lose-tag
             :optional {:prompt "Remove 1 power counter to draw 2 cards?"
                        :waiting-prompt true
                        :req (req (and (= :runner (:side context))
                                       (pos? (:amount context))
                                       (pos? (get-counters card :power))))
                        :yes-ability (draw-abi 2 nil {:cost [(->c :power 1)]})}}
            {:event :runner-turn-ends
             :req (req tagged)
             :msg "place 1 power counter on itself"
             :async true
             :effect (req (add-counter state side eid card :power 1))}]})


(defcard "Aniccam"
  (let [ability {:async true
                 :once-per-instance true
                 :req (req (and (some #(event? (:card %)) targets)
                                (letfn [(event-targets? [targets]
                                          (some #(event? (:card %)) targets))]
                                  (first-trash? state event-targets?))))
                 :change-in-game-state {:silent true :req (req (seq (:deck runner)))}
                 :msg "draw 1 card"
                 :effect (effect (draw :runner eid 1))}]
    {:static-abilities [(mu+ 1)]
     :events [(assoc ability :event :corp-trash)
              (assoc ability :event :runner-trash)
              (assoc ability :event :game-trash)]}))

(defcard "Archives Interface"
  {:events
   [{:event :breach-server
     :automatic :pre-breach
     :async true
     :interactive (req true)
     :req (req (and (= target :archives)
                    (not= (:max-access run) 0)
                    (not-empty (:discard corp))))
     :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                  (continue-ability
                    state side
                    {:optional
                     {:prompt "Remove a card from the game instead of accessing it?"
                      :yes-ability {:prompt "Choose a card in Archives"
                                    :choices (req (:discard corp))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :corp target :rfg))}}} card nil))}]})

(defcard "Astrolabe"
  {:static-abilities [(mu+ 1)]
   :events [(draw-abi 1 nil {:event :server-created})]})

(defcard "Autoscripter"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (and (program? (:card context))
                            ;; only trigger on Runner's turn
                            (= (:active-player @state) :runner)
                            ;; only trigger when playing a Program from grip
                            (some #{:hand} (:previous-zone (:card context)))
                            ;; check that we haven't played a Program from the grip this turn
                            ;; which translates to just one case of playing a Program in turn-events
                            (first-event? state :runner :runner-install
                                          (fn [[context]]
                                            (and (some #{:hand} (:previous-zone (:card context)))
                                                 (program? (:card context)))))))
             :msg "gain [Click]"
             :effect (effect (gain-clicks 1))}
            {:event :unsuccessful-run
             :async true
             :msg "trash itself"
             :effect (effect (trash eid card {:cause-card card}))}]})

(defcard "Basilar Synthgland 2KVJ"
  {:on-install {:async true
                :effect (effect (damage eid :brain 2 {:card card}))}
   :in-play [:click-per-turn 1]})

(defcard "Blackguard"
  (letfn [(force-a-rez [c]
            {:msg (msg "attempt to force the rez of " (:title c))
             :async true
             :effect (req (let [cname (:title c)
                                cost (rez-cost state side c)
                                additional-costs (rez-additional-cost-bonus state side c)
                                payable? (can-pay-to-rez? state :corp eid c)]
                            (cond
                              (not payable?)
                              (effect-completed state side eid)
                              (seq additional-costs)
                              (continue-ability
                                state side
                                {:optional
                                 {:waiting-prompt true
                                  :prompt (msg (build-cost-string [(->c :credit cost)])
                                               ", plus " (decapitalize (build-cost-string additional-costs))
                                               " as an additional cost to rez " cname "?")
                                  :player :corp
                                  :yes-ability {:async true
                                                :effect (effect (rez :corp eid c))}
                                  :no-ability {:msg (msg "declines to pay additional costs"
                                                         " and is not forced to rez " cname)}}}
                                card nil)
                              :else (rez state :corp eid c))))})
          (choose-a-card [cards]
            (if (= 1 (count cards))
              (force-a-rez (first cards))
              {:prompt "Force the Corp to rez which card?"
               :req (req (seq cards))
               :choices (req cards)
               :async true
               :effect (req (wait-for (resolve-ability
                                        state side
                                        (force-a-rez target)
                                        card nil)
                                      (continue-ability
                                        state side
                                        (choose-a-card (filterv #(not (same-card? % target)) cards))
                                        card nil)))}))]
  {:static-abilities [(mu+ 2)]
   :events [{:event :expose
             :req (req (seq (:cards context)))
             :async true
             :effect (req (continue-ability state side (choose-a-card (:cards context)) card nil))}]}))

(defcard "Bling"
  (letfn [(is-no-creds? [costs]
            (not-any? #(and
                         (= (:cost/type %) :credit)
                         (pos? (:cost/amount %)))
                      costs))]
    {:static-abilities [(mu+ 1)
                        {:type :can-play-as-if-in-hand
                         :req (req (same-card? (:host target) card))
                         :value true}]
     :events [{:event :runner-install
               :skippable true
               :optional {:waiting-prompt true
                          :req (req (and (is-no-creds? (:costs context))
                                         (seq (:deck runner))))
                          :prompt "Host the top card of your stack on Bling?"
                          :yes-ability {:msg (msg "host " (:title (first (:deck runner))))
                                        :effect (req (trigger-event state side :bling-hosted)
                                                     (let [times-hosted (min (event-count state nil :bling-hosted) 10)]
                                                       (play-sfx state side (str "bling-" times-hosted)))
                                                     (host state side card (first (:deck runner))))}}}
              {:event :runner-turn-ends
               :req (req (seq (:hosted card)))
               :msg (msg "trash " (enumerate-cards (:hosted (get-card state card)) :sorted))
               :async true
               :effect (req (trash-cards state :runner eid (:hosted card)))}]}))

(defcard "BMI Buffer"
  (let [grip-program-trash?
        (fn [card]
          (and (runner? card)
               (program? card)
               (in-discard? card)
               (= (first (:previous-zone card)) :hand)))
        triggered-ability
        {:async true
         :effect (req (doseq [c (filter grip-program-trash? (mapv #(find-latest state (:card %)) targets))]
                        (host state side (get-card state card) c))
                      (effect-completed state side eid))}]
    {:events [(assoc triggered-ability :event :runner-trash)
              (assoc triggered-ability :event :corp-trash)]
     :abilities [{:action true
                  :cost [(->c :click 2)]
                  :label "Install a hosted program"
                  :prompt "Choose a program to install"
                  :choices (req (cancellable (filter #(runner-can-pay-and-install? state side (assoc eid :source card) %) (:hosted card))))
                  :msg (msg "install " (:title target))
                  :async true
                  :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target))}]}))

(defcard "BMI Buffer 2"
  (let [grip-program-trash?
        (fn [card]
          (and (runner? card)
               (program? card)
               (in-discard? card)
               (= (first (:previous-zone card)) :hand)))
        triggered-ability
        {:async true
         :effect (req (doseq [c (filter grip-program-trash? (mapv #(find-latest state (:card %)) targets))]
                        (host state side (get-card state card) c))
                      (effect-completed state side eid))}]
    {:events [(assoc triggered-ability :event :runner-trash)
              (assoc triggered-ability :event :corp-trash)]
     :abilities [{:action true
                  :cost [(->c :click 2)]
                  :label "Install a hosted program"
                  :prompt "Choose a program to install"
                  :choices (req (:hosted card))
                  :msg (msg "install " (:title target))
                  :async true
                  :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:ignore-all-cost true}))}]}))

(defcard "Bookmark"
  {:abilities [{:action true
                :label "Host up to 3 cards from the grip facedown"
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :msg "host up to 3 cards from the grip facedown"
                :choices {:max 3
                          :card #(and (runner? %)
                                      (in-hand? %))}
                :effect (req (doseq [c targets]
                               (host state side (get-card state card) c {:facedown true})))}
               {:action true
                :label "Add all hosted cards to the grip"
                :cost [(->c :click 1)]
                :msg "add all hosted cards to the grip"
                :effect (req (doseq [c (:hosted card)]
                               (move state side c :hand)))}
               {:label "Add all hosted cards to the grip"
                :fake-cost [(->c :trash-can)]
                :async true
                :effect (req (let [hosted-cards (:hosted card)]
                               (doseq [c hosted-cards]
                                 (move state side c :hand))
                               (continue-ability
                                 state side
                                 {:cost [(->c :trash-can)]
                                  :msg (msg "add " (quantify (count hosted-cards) "hosted card") " to the grip")}
                                 card nil)))}]})

(defcard "Boomerang"
  (auto-icebreaker
    {:on-install {:prompt "Choose an installed piece of ice"
                  :msg (msg "target " (card-str state target))
                  :choices {:card #(and (installed? %)
                                        (ice? %))}
                  :effect (effect (add-icon card target "B" (faction-label card))
                                  (update! (assoc-in (get-card state card) [:special :boomerang-target] target)))}
     :leave-play (effect (remove-icon card))
     :abilities [(break-sub
                   [(->c :trash-can)] 2 "All"
                   {:req (req (if-let [boomerang-target (get-in card [:special :boomerang-target])]
                                (some #(same-card? boomerang-target (:ice %)) (:encounters @state))
                                true)) ; When eg. flipped by Assimilator
                    :additional-ability
                    {:effect (req (let [source (or card (first (get-in eid [:cost-paid :trash-can :paid/targets])))]
                                    ;; special note: since the source is trashed, auto-pump-impl doesn't pass it on
                                    ;; to the additional-abi in a nice way. This is a bit of a hack to fix that.
                                    ;; If we ever rework costs, this might need to be adjusted -nbk, 2025
                                    (register-events
                                      state side source
                                      [{:event :run-ends
                                        :duration :end-of-run
                                        :unregister-once-resolved true
                                        :optional
                                        {:req (req (and (:successful target)
                                                        (not (zone-locked? state :runner :discard))
                                                        (some #(= (:title card) (:title %)) (:discard runner))))
                                         :once :per-run
                                         :prompt (msg "Shuffle a copy of " (:title card) " back into the Stack?")
                                         :yes-ability
                                         {:msg (msg "shuffle a copy of " (:title card) " back into the Stack")
                                          :effect (effect (move (some #(when (= (:title card) (:title %)) %)
                                                                      (:discard runner))
                                                                :deck)
                                                          (shuffle! :deck))}}}])))}})
                 {:label "Break 0 subroutines"
                  :cost [(->c :trash-can)]
                  :msg "break 0 subroutines"
                  :req (req (if-let [boomerang-target (get-in card [:special :boomerang-target])]
                                (some #(same-card? boomerang-target (:ice %)) (:encounters @state))
                                true))
                  :effect (req (let [source (or card (first (get-in eid [:cost-paid :trash-can :paid/targets])))]
                                 ;; special note: since the source is trashed, auto-pump-impl doesn't pass it on
                                 ;; to the additional-abi in a nice way. This is a bit of a hack to fix that.
                                 ;; If we ever rework costs, this might need to be adjusted -nbk, 2025
                                 (register-events
                                   state side source
                                   [{:event :run-ends
                                     :duration :end-of-run
                                     :unregister-once-resolved true
                                     :optional
                                     {:req (req (and (:successful target)
                                                     (not (zone-locked? state :runner :discard))
                                                     (some #(= (:title card) (:title %)) (:discard runner))))
                                      :once :per-run
                                      :prompt (msg "Shuffle a copy of " (:title card) " back into the Stack?")
                                      :yes-ability
                                      {:msg (msg "shuffle a copy of " (:title card) " back into the Stack")
                                       :effect (effect (move (some #(when (= (:title card) (:title %)) %)
                                                                   (:discard runner))
                                                             :deck)
                                                       (shuffle! :deck))}}}])))}]}))

(defcard "Box-E"
  {:static-abilities [(mu+ 2)
                      (runner-hand-size+ 2)]})

(defcard "Brain Cage"
  {:static-abilities [(runner-hand-size+ 3)]
   :on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}})

(defcard "Brain Chip"
  {:x-fn (req (max (get-in @state [:runner :agenda-point] 0) 0))
   :static-abilities [(mu+
                        (req (pos? ((get-x-fn) state side eid card targets)))
                        ;; [:regular N] is needed to make the mu system work
                        (req [:regular ((get-x-fn) state side eid card targets)]))
                      (runner-hand-size+ (get-x-fn))]})

(defcard "Buffer Drive"
  (let [grip-or-stack-trash? (fn [{:keys [card]}]
                               (and (runner? card)
                                    (or (in-hand? card)
                                        (in-deck? card))))
        triggered-ability
        {:once-per-instance true
         :req (req
                (and (some grip-or-stack-trash? targets)
                     (first-trash? state #(some grip-or-stack-trash? %))))
         :interactive (req true)
         :prompt "Choose 1 trashed card to add to the bottom of the stack"
         :choices (req (conj (sort (keep #(->> (:moved-card %) :title) (filter grip-or-stack-trash? targets))) "No action"))
         :async true
         :effect (req (if (= "No action" target)
                        (effect-completed state side eid)
                        (do (system-msg state side
                                        (str "uses " (:title card) " to add " target
                                             " to the bottom of the stack"))
                            ;; note - need to search in reverse order, to remove the NEWEST copy of the card
                            ;; this is for interactions with the price, etc
                            (move state side (find-card target (reverse (:discard (:runner @state)))) :deck)
                            (effect-completed state side eid))))}]
    {:events [(assoc triggered-ability :event :runner-trash)
              (assoc triggered-ability :event :corp-trash)]
     :abilities [{:req (req (not (zone-locked? state :runner :discard)))
                  :label "Add a card from the heap to the top of the stack"
                  :cost [(->c :remove-from-game)]
                  :show-discard true
                  :choices {:card #(and (runner? %)
                                        (in-discard? %))}
                  :msg (msg "add " (:title target) " to the top of the stack")
                  :effect (effect (move target :deck {:front true}))}]}))

(defcard "Capstone"
  {:abilities [{:action true
                :req (req (pos? (count (:hand runner))))
                :label "trash and install cards"
                :cost [(->c :click 1)]
                :async true
                :prompt "Choose any number of cards to trash from the grip"
                :choices {:max (req (count (:hand runner)))
                          :card #(and (runner? %)
                                      (in-hand? %))}
                :effect (req (let [trashed-card-names (keep :title targets)
                                   installed-card-names (keep :title (all-active-installed state :runner))
                                   overlap (set/intersection (set trashed-card-names)
                                                                 (set installed-card-names))]
                               (wait-for (trash-cards state side targets {:unpreventable true
                                                                          :cause-card card})
                                         (let [trashed-cards async-result]
                                           (wait-for (draw state side (count (filter overlap trashed-card-names)))
                                                     (system-msg state side
                                                                 (str "uses " (:title card) " to trash "
                                                                      (enumerate-cards trashed-cards)
                                                                      " from the grip and draw "
                                                                      (quantify (count async-result) "card")))
                                                     (effect-completed state side eid))))))}]})

(defcard "Capybara"
  {:events [{:event :bypassed-ice
             :async true
             :optional
             {:req (req true)
              :prompt (msg "Remove this hardware from the game to derez " (:title target) "?")
              :waiting-prompt true
              :yes-ability
              {:async true
               :cost [(->c :remove-from-game)]
               :effect (req (derez state side eid target {:msg-keys {:include-cost-from-eid eid}}))}}}]})

(defcard "Carnivore"
  {:static-abilities [(mu+ 1)]
   :interactions
   {:access-ability
    {:label "Trash card"
     :trash? true
     :req (req (and (can-trash? state :runner target)
                    (not (in-discard? target))
                    (not (get-in @state [:per-turn (:cid card)]))
                    (<= 2 (count (:hand runner)))))
     :cost [(->c :trash-from-hand 2)]
     :msg (msg "trash " (:title target) " at no cost")
     :once :per-turn
     :async true
     :effect (effect (trash eid (assoc target :seen true) {:accessed true
                                                           :cause-card card}))}}})

(defcard "Cataloguer"
  (let [index-ability (successful-run-replace-breach
                        {:target-server :rd
                         :mandatory false
                         :ability
                         {:async true
                          :msg "rearrange the top 4 cards of R&D"
                          :cost [(->c :power 1)]
                          :req (req (pos? (get-counters card :power)))
                          :waiting-prompt true
                          :effect (req (continue-ability
                                         state side
                                         (let [from (take 4 (:deck corp))]
                                           (when (pos? (count from))
                                             (reorder-choice :corp :corp from '() (count from) from)))
                                         card nil))}})
        access-ability {:action true
                        :cost [(->c :click 1) (->c :power 1)]
                        :req (req (some #{:rd} (:successful-run runner-reg)))
                        :label "Breach R&D"
                        :msg "breach R&D"
                        :keep-menu-open :while-power-tokens-left
                        :async true
                        :effect (effect (breach-server eid [:rd] #_{:no-root true}))}]
    {:data {:counter {:power 2}}
     :abilities [access-ability]
     :events [(trash-on-empty :power)
              index-ability]}))

(defcard "Chop Bot 3000"
  (let [ability {:req (req (>= (count (all-installed state :runner)) 2))
                 :label "Trash another installed card to draw 1 card or remove 1 tag"
                 :once :per-turn
                 :choices {:card #(and (runner? %)
                                       (installed? %))
                           :not-self true}
                 :async true
                 :effect (req (wait-for (trash state :runner target {:unpreventable true :cause-card card})
                                        (continue-ability
                                          state side
                                          (let [trashed-card target
                                                tags (pos? (count-real-tags state))]
                                            {:prompt "Choose one"
                                             :waiting-prompt true
                                             :choices ["Draw 1 card"
                                                       (when tags "Remove 1 tag")]
                                             :async true
                                             :msg (msg "trash " (:title trashed-card) " and " (decapitalize target))
                                             :effect (req (if (= target "Draw 1 card")
                                                            (draw state :runner eid 1)
                                                            (lose-tags state :runner eid 1)))})
                                          card nil)))}]
    {:flags {:runner-phase-12 (req (>= (count (all-installed state :runner)) 2))}
     :events [(assoc ability
                     :event :runner-turn-begins
                     :skippable true
                     :interactive (req true))]
     :abilities [ability]}))

(defcard "Clone Chip"
  {:abilities [{:prompt "Choose a program to install"
                :label "Install program from the heap"
                :show-discard true
                :change-in-game-state {:req (req (some #(and (program? %)
                                                             (runner-can-pay-and-install?
                                                               state side
                                                               (assoc eid :source card :source-type :runner-install) %
                                                               {:no-toast true}))
                                                       (:discard runner)))}
                :choices {:req (req (and (program? target)
                                         (in-discard? target)
                                         (runner-can-pay-and-install? state side (assoc eid :source card) target)))}
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:msg-keys {:install-source card
                                                                                                                         :display-origin true
                                                                                                                         :include-cost-from-eid eid}}))}]})

(defcard "Comet"
  {:static-abilities [(mu+ 1)]
   :events [{:event :play-event
             :req (req (first-event? state side :play-event))
             :effect (req (system-msg state :runner
                                      (str "can play another event without spending a [Click] by clicking on Comet"))
                          (update! state side (assoc card :comet-event true)))}]
   :abilities [{:async true
                :label "Play an event in the grip twice"
                :req (req (:comet-event card))
                :prompt "Choose an event to play"
                :choices {:card #(and (event? %)
                                      (in-hand? %))}
                :msg (msg "play " (:title target))
                :effect (req (let [eid (assoc eid :source-type :play)]
                               (update! state :runner (dissoc (get-card state card) :comet-event))
                               (play-instant state side eid target nil)))}]})

(defcard "Cortez Chip"
  {:abilities [{:prompt "Choose a piece of ice"
                :label "increase rez cost of ice"
                :choices {:card #(and (ice? %)
                                      (not (rezzed? %)))}
                :msg (msg "increase the rez cost of " (card-str state target)
                          " by 2 [Credits] until the end of the turn")
                :cost [(->c :trash-can)]
                :effect (effect (register-lingering-effect
                                  card
                                  (let [ice target]
                                    {:type :rez-additional-cost
                                     :duration :end-of-turn
                                     :req (req (same-card? target ice))
                                     :value [(->c :credit 2)]})))}]})

(defcard "Cyberdelia"
  {:static-abilities [(mu+ 1)]
   :events [{:event :subroutines-broken
             :req (req (and (:all-subs-broken context)
                            (first-event? state side :subroutines-broken #(:all-subs-broken (first %)))))
             :msg "gain 1 [Credits] for breaking all subroutines on a piece of ice"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Cyberfeeder"
  {:recurring 1
   :interactions
   {:pay-credits
    {:req (req (or (and (= :runner-install (:source-type eid))
                        (has-subtype? target "Virus")
                        (program? target))
                   (and (= :ability (:source-type eid))
                        (has-subtype? target "Icebreaker"))))
     :type :recurring}}})

(defcard "CyberSolutions Mem Chip"
  {:static-abilities [(mu+ 2)]})

(defcard "Cybsoft MacroDrive"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (program? target)))
                                :type :recurring}}})

(defcard "Daredevil"
  {:static-abilities [(mu+ 2)]
   :events [(draw-abi 2 nil {:event :run
                             :req (req (and (<= 2 (:position target))
                                            (first-event? state side :run #(<= 2 (:position (first %))))))})]})

(defcard "Dedicated Processor"
  {:implementation "Click Dedicated Processor to use ability"
   :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
   :hosting {:card #(and (has-subtype? % "Icebreaker")
                         (not (has-subtype? % "AI"))
                         (installed? %))}
   :abilities [{:cost [(->c :credit 2)]
                :label "add 4 strength for the remainder of the run"
                :req (req run)
                :effect (effect (pump (get-card state (:host card)) 4))
                :msg (msg "pump the strength of " (get-in card [:host :title]) " by 4")}]})

(defcard "Deep Red"
  {:static-abilities [(caissa-mu+ 3)]
   :events [{:event :runner-install
             :optional
             {:req (req (has-subtype? (:card context) "Caïssa"))
              :prompt "Trigger the [Click] ability of the just-installed Caïssa program?"
              :yes-ability
              {:async true
               :effect (effect (play-ability eid {:card (:card context)
                                                  :ability 0
                                                  :ignore-cost true}))}}}]})

(defcard "Demolisher"
  {:static-abilities [(mu+ 1)
                      {:type :trash-cost
                       :value -1}]
   :events [{:event :runner-trash
             :once-per-instance true
             :req (req (and (corp? (:card target))
                            (first-event? state side :runner-trash
                                          (fn [targets]
                                            (some #(corp? (:card %)) targets)))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}]})

(defcard "Desperado"
  {:static-abilities [(mu+ 1)]
   :events [{:event :successful-run
             :automatic :gain-credits
             :silent (req true)
             :async true
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits eid 1))}]})

(defcard "Detente"
  (let [return-cards
        {:action true
         :cost [(->c :click 1) (->c :hosted-to-hq 2)]
         :label "Runner may access 1 card from HQ"
         :msg :cost
         :async true
         :effect (req (continue-ability
                        state :runner
                        {:optional
                         {:prompt "Access 1 card from HQ?"
                          :waiting-prompt true
                          :yes-ability {:msg (msg "access 1 card from HQ")
                                        :async true
                                        :effect (req (access-card state :runner eid (-> corp :hand shuffle first)))}}}
                        card nil))}]
    {:static-abilities [(mu+ 1)]
     :events [{:event :successful-run
               :skippable true
               :interactive (req true)
               :optional {:req (req
                                 (let [valid-ctx? (fn [[ctx]] (-> ctx :server first (= :hq)))]
                                   (and (valid-ctx? [context])
                                        (first-event? state side :successful-run valid-ctx?)
                                        (seq (:hand corp)))))
                          :waiting-prompt true
                          :prompt "Reveal and host a card from HQ (at random)"
                          :yes-ability {:effect (req (let [target-card (first (shuffle (:hand corp)))]
                                                       (system-msg state side
                                                                   (str "uses Detente to reveal and host "
                                                                        (:title target-card)
                                                                        " from HQ"))
                                                       (wait-for
                                                         (reveal state :runner target-card)
                                                         (host state side card
                                                               (assoc target-card :seen true))
                                                         (effect-completed state side eid))))
                                        :async true}}}]
     :abilities [return-cards]
     :corp-abilities [(assoc return-cards :player :corp :display-side :corp)]}))

(defcard "Devil Charm"
  {:events [{:event :encounter-ice
             :skippable true
             :interactive (req true)
             :optional
             {:prompt "Remove Devil Charm from the game to give encountered ice -6 strength?"
              :yes-ability
              {:msg (msg "give -6 strength to " (card-str state (:ice context)) " for the remainder of the run")
               :cost [(->c :remove-from-game)]
               :effect (effect (register-lingering-effect
                                 card
                                 (let [ice (:ice context)]
                                   {:type :ice-strength
                                    :duration :end-of-run
                                    :req (req (same-card? target ice))
                                    :value -6}))
                               (update-all-ice))}}}]})

(defcard "Dinosaurus"
  {:static-abilities [{:type :can-host
                       :req (req (and (program? target)
                                      (has-subtype? target "Icebreaker")
                                      (not (has-subtype? target "AI"))))
                       :max-cards 1
                       :no-mu true}
                      {:type :breaker-strength
                       :req (req (same-card? target (first (:hosted card))))
                       :value 2}]})

(defcard "Docklands Pass"
  {:events [(breach-access-bonus
             :hq 1
             {:req (req (and (= :hq target)
                             (first-event? state side :breach-server #(= :hq (first %)))))
              :msg "access 1 additional card from HQ"})]})

(defcard "Doppelgänger"
  {:static-abilities [(mu+ 1)]
   :events [{:event :run-ends
             :interactive (req true)
             :change-in-game-state {:silent true :req (req (not-used-once? state {:once :per-turn} card))}
             :optional {:req (req (and (:successful target)
                                       (not-used-once? state {:once :per-turn} card)))
                        :prompt "Make another run?"
                        :yes-ability {:prompt "Choose a server"
                                      :once :per-turn
                                      :async true
                                      :choices (req runnable-servers)
                                      :msg (msg "make a run on " target)
                                      :makes-run true
                                      :effect (effect (unregister-lingering-effects :end-of-run)
                                                      (unregister-floating-events :end-of-run)
                                                      (register-once {:once :per-turn} card)
                                                      (update-all-icebreakers)
                                                      (update-all-ice)
                                                      (reset-all-ice)
                                                      (clear-wait-prompt :corp)
                                                      (make-run eid target (get-card state card)))}}}]})

(defcard "Dorm Computer"
  {:data {:counter {:power 4}}
   :static-abilities [{:type :forced-to-avoid-tag
                       :value true
                       :req (req this-card-is-run-source)}]
   :events [{:event :tag-interrupt
             :req (req (= (get-in run [:source-card :title]) (:title card)))
             :async true
             :msg "avoid all tags"
             :effect (req (prevent-tag state :runner eid :all))}]
   :abilities [(run-any-server-ability {:action true
                                        :cost [(->c :click 1) (->c :power 1)]
                                        :msg "make a run and avoid all tags for the remainder of the run"})]})

(defcard "Dyson Fractal Generator"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Fracter")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Dyson Mem Chip"
  {:static-abilities [(mu+ 1)
                      (link+ 1)]})

(defcard "DZMZ Optimizer"
  {:static-abilities [(mu+ 1)
                      {:type :install-cost
                       :req (req (and (program? target)
                                      (no-event? state :runner :runner-install #(program? (:card (first %))))))
                       :value -1}]
   :events [{:event :runner-install
             :req (req (and (program? target)
                            (first-event? state :runner :runner-install #(program? (first %)))))
             :silent (req true)
             :msg (msg "reduce the install cost of " (:title target) " by 1 [Credits]")}]})

(defcard "e3 Feedback Implants"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All" {:req (req (any-subs-broken? current-ice))})]}))

(defcard "Ekomind"
  (let [update-base-mu (fn [state n] (swap! state assoc-in [:runner :memory :base] n))]
    {:effect (req (update-base-mu state (count (get-in @state [:runner :hand])))
                  (add-watch state :ekomind (fn [_k ref old new]
                                              (let [hand-size (count (get-in new [:runner :hand]))]
                                                (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                  (update-base-mu ref hand-size))))))
     :leave-play (req (remove-watch state :ekomind))}))

(defcard "EMP Device"
  {:abilities [{:req (req run)
                :msg "prevent the Corp from rezzing more than 1 piece of ice for the remainder of the run"
                :cost [(->c :trash-can)]
                :effect (effect
                          (register-events
                            card
                            [{:event :rez
                              :duration :end-of-run
                              :unregister-once-resolved true
                              :req (req (ice? (:card context)))
                              :effect (effect (register-run-flag!
                                                card :can-rez
                                                (fn [state _side card]
                                                  (if (ice? card)
                                                    ((constantly false)
                                                     (toast state :corp "Cannot rez ice the rest of this run due to EMP Device"))
                                                    true))))}]))}]})

(defcard "Endurance"
  (auto-icebreaker
    {:data {:counter {:power 3}}
     :static-abilities [(mu+ 2)]
     :events [{:event :successful-run
               :req (req (first-event? state :runner :successful-run))
               :msg "place 1 power counter on itself"
               :async true
               :effect (effect (add-counter eid card :power 1))}]
     :abilities [(break-sub [(->c :power 2)] 2 "All")]}))

(defcard "Feedback Filter"
  {:prevention [{:prevents :damage
                 :type :ability
                 :label "Feedback Filter (Net)"
                 :ability {:async true
                           :cost [(->c :credit 3)]
                           :msg "prevent 1 net damage"
                           :req (req (and (= :net (:type context))
                                          (preventable? context)))
                           :effect (req (prevent-damage state side eid 1))}}
                {:prevents :damage
                 :type :ability
                 :label "Feedback Filter (Core)"
                 :ability (assoc (prevent-up-to-n-damage 2 #{:brain :core})
                                 :cost [(->c :trash-can)])}]})

(defcard "Flame-out"
  (let [register-flame-effect
        (fn [state card]
          (update! state :runner (assoc-in (get-card state card) [:special :flame-out-trigger] true)))
        maybe-turn-end {:async true
                        :automatic :last
                        :req (req (:flame-out-trigger (:special (get-card state card))))
                        :effect (req (update! state side (assoc-in (get-card state card) [:special :flame-out-trigger] nil))
                                     (if-let [hosted (first (:hosted card))]
                                       (do (system-msg state :runner (str "trashes " (:title hosted) " from Flame-out"))
                                           (trash state side eid hosted {:cause-card card}))
                                       (effect-completed state side eid)))}]
    {:implementation "Credit usage restriction not enforced"
     :static-abilities [{:type :can-host
                         :req (req (program? target))
                         :max-cards 1}]
     :data {:counter {:credit 9}}
     :abilities [{:label "Take 1 hosted [Credits]"
                  :req (req (and (not-empty (:hosted card))
                                 (pos? (get-counters card :credit))))
                  :async true
                  :effect (req (system-msg state :runner "takes 1 hosted [Credits] from Flame-out")
                               (register-flame-effect state card)
                               (spend-credits state side eid card :credit 1))}
                 {:label "Take all hosted [Credits]"
                  :req (req (and (not-empty (:hosted card))
                                 (pos? (get-counters card :credit))))
                  :async true
                  :effect (req (let [credits (get-counters card :credit)]
                                 (system-msg state :runner (str "takes " credits " hosted [Credits] from Flame-out"))
                                 (register-flame-effect state card)
                                 (take-credits state side eid card :credit :all)))}]
     :events [(assoc maybe-turn-end :event :runner-turn-ends)
              (assoc maybe-turn-end :event :corp-turn-ends)]
     :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                                 (same-card? card (:host target))
                                                 (pos? (get-counters card :credit))))
                                  :custom-amount 1
                                  :custom (req (wait-for
                                                 (add-counter state side card :credit -1 {:suppress-checkpoint true})
                                                 (register-flame-effect state card)
                                                 (effect-completed state side (make-result eid 1))))
                                  :type :custom}}}))

(defcard "Flip Switch"
  {:events [{:event :initialize-trace
             :optional
             {:req (req (= :runner (:active-player @state)))
              :waiting-prompt true
              :prompt "Trash Flip Switch to reduce the base trace strength to 0?"
              :yes-ability {:msg "reduce the base trace strength to 0"
                            :cost [(->c :trash-can)]
                            :effect (req (swap! state assoc-in [:trace :force-base] 0))}}}]
   :abilities [{:label "Jack out"
                :change-in-game-state {:req (req (and (or run
                                                          (get-current-encounter state))))}
                :req (req (= :runner (:active-player @state)))
                :msg "jack out"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (jack-out eid))}
               {:label "Remove 1 tag"
                :change-in-game-state {:req (req (pos? (count-real-tags state)))}
                :req (req (= :runner (:active-player @state)))
                :msg "remove 1 tag"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (lose-tags eid 1))}]})

(defcard "Forger"
  (let [avoid-ab {:msg "avoid 1 tag"
                  :label "Avoid 1 tag"
                  :async true
                  :cost [(->c :trash-can)]
                  :effect (effect (prevent-tag :runner eid 1))}]
    {:events [(choose-one-helper
                {:event :tag-interrupt
                 :req (req (and (pos? (get-in @state [:prevent :tag :remaining]))
                                (not (any-effects state side :prevent-paid-ability true? card [avoid-ab 0]))))
                 :optional true
                 :interactive (req true)}
                [{:option "Avoid 1 tag"
                  :cost [(->c :trash-can)]
                  :ability avoid-ab}])]
     :static-abilities [(link+ 1)]
     :abilities [{:msg "remove 1 tag"
                  :label "Remove 1 tag"
                  :cost [(->c :trash-can)]
                  :change-in-game-state {:req (req (pos? (count-real-tags state)))}
                  :async true
                  :effect (effect (lose-tags eid 1))}]}))

(defcard "Friday Chip"
  (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                 :skippable true
                 :req (req (and (pos? (get-counters card :virus))
                                (pos? (count-virus-programs state))))
                 :choices {:card virus-program?}
                 :async true
                 :effect (req (wait-for
                                (add-counter state :runner card :virus -1 {:suppress-checkpoint true})
                                (add-counter state :runner eid target :virus 1)))}]
    {:abilities [(set-autoresolve :auto-fire "Friday Chip placing virus counters on itself")]
     :special {:auto-fire :always}
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-trash
               :once-per-instance true
               :async true
               :req (req (some #(corp? (:card %)) targets))
               :effect (req (let [amt-trashed (count (filter #(corp? (:card %)) targets))
                                  sing-ab {:optional
                                           {:prompt (msg "Place a virus counter on " (:title card) "?")
                                            :autoresolve (get-autoresolve :auto-fire)
                                            :yes-ability {:async true
                                                          :effect (effect (system-msg
                                                                            :runner
                                                                            (str "uses " (:title card) " to place 1 virus counter on itself"))
                                                                          (add-counter :runner eid card :virus 1))}}}
                                  mult-ab {:prompt (msg "Place virus counters on " (:title card) "?")
                                           :choices {:number (req amt-trashed)
                                                     :default (req amt-trashed)}
                                           :async true
                                           :effect (effect (system-msg :runner
                                                                       (str "uses " (:title card) " to place "
                                                                            (quantify target "virus counter")
                                                                            " on itself"))
                                                           (add-counter :runner eid card :virus target))}
                                  ab (if (> amt-trashed 1) mult-ab sing-ab)]
                              (continue-ability state side ab card targets)))}]}))

(defcard "Gachapon"
  (letfn [(shuffle-end [remove-from-game shuffle-back]
            {:msg (msg "shuffle " (enumerate-cards shuffle-back :sorted) " into the stack"
                       " and remove " (enumerate-cards remove-from-game :sorted) " from the game")
             :effect (req
                       (doseq [c remove-from-game]
                         (move state side c :rfg))
                       (doseq [c shuffle-back]
                         (move state side c :deck))
                       (shuffle! state side :deck))})
          (shuffle-next [set-aside-cards target to-shuffle]
            (let [set-aside-cards (remove-once #(= % target) set-aside-cards)
                  to-shuffle (if target
                               (concat to-shuffle [target])
                               [])
                  finished? (or (= 3 (count to-shuffle))
                                (empty? set-aside-cards))]
              {:prompt (msg (if finished?
                              (str "Removing: " (if (not-empty set-aside-cards)
                                                  (enumerate-cards set-aside-cards :sorted)
                                                  "nothing")
                                   "[br]Shuffling: " (if (not-empty to-shuffle)
                                                       (enumerate-cards to-shuffle :sorted)
                                                       "nothing"))
                              (str "Choose " (- 3 (count to-shuffle)) " more cards to shuffle back."
                                   (when (not-empty to-shuffle)
                                     (str "[br]Currently shuffling back: " (enumerate-cards to-shuffle :sorted))))))
               :async true
               :not-distinct true ; show cards separately
               :choices (req (if finished?
                               ["Done" "Start over"]
                               (seq set-aside-cards)))
               :effect (req (if finished?
                              (if (= "Done" target)
                                (continue-ability state side
                                                  (shuffle-end set-aside-cards to-shuffle)
                                                  card nil)
                                (continue-ability state side
                                                  (shuffle-next (sort-by :title (concat set-aside-cards to-shuffle)) nil nil)
                                                  card nil))
                              (continue-ability state side
                                                (shuffle-next set-aside-cards target to-shuffle)
                                                card nil)))}))]
    {:abilities [{:label "Install a card from among the top 6 cards of the stack"
                  :change-in-game-state {:req (req (seq (:deck runner)))}
                  :cost [(->c :trash-can)]
                  :async true
                  :waiting-prompt true
                  :effect (req (set-aside state side eid (take 6 (:deck runner)))
                               (let [set-aside-cards (sort-by :title (get-set-aside state side eid))]
                                 (system-msg state side (str (:latest-payment-str eid) "to use " (get-title card)
                                                             " to set aside "
                                                             (enumerate-cards set-aside-cards)
                                                             " from the top of the stack"))
                                 (wait-for (resolve-ability state side
                                                            {:async true
                                                             :prompt (str "The set aside cards are: "
                                                                          (enumerate-cards set-aside-cards))
                                                             :choices ["OK"]}
                                                            card nil)
                                           (continue-ability
                                             state side
                                             {:prompt "Choose a card to install"
                                              :async true
                                              :choices (req (concat
                                                              (filter #(and (or (program? %)
                                                                                (and (resource? %)
                                                                                     (has-subtype? % "Virtual")))
                                                                            (runner-can-pay-and-install?
                                                                              state side
                                                                              (assoc eid :source card :source-type :runner-install)
                                                                              % {:cost-bonus -2
                                                                                 :no-toast true}))
                                                                      set-aside-cards)
                                                              ["Done"]))
                                              :effect (req (if (= "Done" target)
                                                             (continue-ability state side (shuffle-next set-aside-cards nil nil) card nil)
                                                             (let [set-aside-cards (remove-once #(= % target) set-aside-cards)
                                                                   new-eid (assoc eid :source card :source-type :runner-install)]
                                                               (wait-for (runner-install state side new-eid target {:cost-bonus -2
                                                                                                                    :msg-keys {:install-source card
                                                                                                                               :display-origin true}})
                                                                         (continue-ability state side (shuffle-next set-aside-cards nil nil) card nil)))))}
                                             card nil))))}]}))

(defcard "GAMEDRAGON™ Pro"
  (let [abi {:prompt "Choose an icebreaker to host GAMEDRAGON™ Pro"
             :event :runner-turn-begins
             :change-in-game-state {:silent true
                                    :req (req (some #(and (program? %)
                                                          (not (has-subtype? % "AI"))
                                                          (not (same-card? % (:host card)))
                                                          (has-subtype? % "Icebreaker"))
                                                    (all-installed state :runner)))}
             :waiting-prompt true
             :choices {:req (req (and
                                   (installed? target)
                                   (program? target)
                                   (not (has-subtype? target "AI"))
                                   (has-subtype? target "Icebreaker")))}
             :effect (req (host state side target card))
             :msg (msg "host itself on " (:title target))}]
    {:on-install abi
     :events [abi
              {:event :pump-breaker
               :req (req (same-card? (:card context) (:host card)))
               :effect
               (req (let [new-pump (assoc (:effect context)
                                          :duration :end-of-run)]
                      (swap! state assoc :effects
                             (->> (:effects @state)
                                  (remove #(= (:uuid %) (:uuid new-pump)))
                                  (#(conj % new-pump))
                                  (into []))))
                    (update-breaker-strength state side (:card context)))}]
     :static-abilities [{:type :breaker-strength
                         :req (req (same-card? target (:host card)))
                         :value 1}]}))

(defcard "Gebrselassie"
  {:abilities [{:action true
                :msg "host itself on an installed non-AI icebreaker"
                :cost [(->c :click 1)]
                :choices {:card #(and (installed? %)
                                      (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI")))}
                :effect (req (when-let [host (get-card state (:host card))]
                               (swap! state assoc :effects
                                      (reduce
                                        (fn [effects e]
                                          (conj effects
                                                (if (and (same-card? host (:card e))
                                                         (= :breaker-strength (:type e))
                                                         (:original-duration e))
                                                  (-> e
                                                      (assoc :duration (:original-duration e))
                                                      (dissoc :original-duration))
                                                  e)))
                                        []
                                        (:effects @state)))
                               (update-breaker-strength state side host))
                             (host state side target card))}]
   :events [{:event :pump-breaker
             :req (req (same-card? (:card context) (:host card)))
             :effect (req (let [last-pump (assoc (:effect context)
                                                 :duration :end-of-turn
                                                 :original-duration (:duration (last (:effects @state))))]
                            (swap! state assoc :effects
                                   (->> (:effects @state)
                                        (remove #(= (:uuid last-pump) (:uuid %)))
                                        (#(conj % last-pump))
                                        (into []))))
                          (update-breaker-strength state side (:card context)))}]
   :leave-play (req (when-let [host (get-card state (:host card))]
                      (swap! state assoc :effects
                             (reduce
                               (fn [effects e]
                                 (conj effects
                                       (if (and (same-card? host (:card e))
                                                (= :breaker-strength (:type e))
                                                (:original-duration e))
                                         (-> e
                                             (assoc :duration (:original-duration e))
                                             (dissoc :original-duration))
                                         e)))
                               []
                               (:effects @state)))
                      (update-breaker-strength state side host)))})

(defcard "Ghosttongue"
  {:on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :static-abilities [{:type :play-cost
                       :req (req (event? target))
                       :value -1}]})

(defcard "GPI Net Tap"
  {:abilities [{:req (req (and (= :approach-ice (:phase run))
                               (ice? current-ice)
                               (not (rezzed? current-ice))))
                :label "expose approached ice"
                :async true
                :effect (req (wait-for (expose state side (make-eid state eid) [current-ice])
                                       (continue-ability state side (offer-jack-out) card nil)))}]})

(defcard "Grimoire"
  {:static-abilities [(mu+ 2)]
   :events [{:event :runner-install
             :interactive (req true)
             :req (req (has-subtype? (:card context) "Virus"))
             :async true
             :effect (effect (add-counter eid (:card context) :virus 1))}]})

(defcard "Heartbeat"
  {:static-abilities [(mu+ 1)]
   :prevention [{:prevents :damage
                 :type :ability
                 :label "Heartbeat"
                 :ability {:async true
                           :cost [(->c :trash-installed 1)]
                           :msg (msg "prevent 1 " (damage-name state) " damage")
                           :req (req (preventable? context))
                           :effect (req (prevent-damage state side eid 1))}}]})

(defcard "Hermes"
  (let [ab {:interactive (req true)
            :prompt "Choose an unrezzed card"
            :change-in-game-state {:silent true
                                   :req (req (some #(and (not (faceup? %))
                                                         (installed? %))
                                                   (all-installed state :corp)))}
            :waiting-prompt true
            :choices {:card #(and (not (faceup? %))
                                  (installed? %)
                                  (corp? %))
                      :all true}
            :msg (msg "add " (card-str state target) " to HQ")
            :effect (effect (move :corp target :hand))}]
    {:static-abilities [(mu+ 1)]
     :events [(assoc ab :event :agenda-scored)
              (assoc ab :event :agenda-stolen)]}))

(defcard "Hijacked Router"
  {:events [{:event :server-created
             :msg "force the Corp to lose 1 [Credits]"
             :async true
             :effect (effect (lose-credits :corp eid 1))}
            {:event :successful-run
             :skippable true
             :optional
             {:req (req (= :archives (target-server context)))
              :prompt (msg "Trash " (:title card) " to force the Corp to lose 3 [Credits]?")
              :yes-ability
              {:async true
               :msg "force the Corp to lose 3 [Credits]"
               :effect (req (wait-for (trash state :runner card {:unpreventable true
                                                                 :cause-card card})
                                      (lose-credits state :corp eid 3)))}}}]})

(defcard "Hippo"
  {:events [{:event :subroutines-broken
             :optional
             {:req (req
                     (let [pred (every-pred :all-subs-broken :outermost :during-run :on-attacked-server)]
                       (and (pred context)
                            (get-card state (:ice context))
                            (first-event? state side :subroutines-broken #(pred (first %))))))
              :prompt (msg "Remove this hardware from the game to trash " (:title (:ice context)) "?")
              :yes-ability
              {:async true
               :cost [(->c :remove-from-game)]
               :msg (msg "trash " (card-str state (:ice context)))
               :effect (effect (trash eid (:ice context) {:cause-card card}))}}}]})

(defcard "Hippocampic Mechanocytes"
  {:on-install {:async true
                :msg "suffer 1 meat damage"
                :effect (effect (damage eid :meat 1 {:unboostable true :card card}))}
   :data {:counter {:power 2}}
   :static-abilities [(runner-hand-size+ (req (get-counters card :power)))]})

(defcard "HQ Interface"
  {:events [(breach-access-bonus :hq 1)]})

(defcard "Jeitinho"
  {:events [{:event :bypassed-ice
             :location :discard
             :interactive (req true)
             :req (req (and (threat-level 3 state)
                            (in-discard? card)))
             :async true
             :effect (req (continue-ability
                            state side
                            {:optional
                             {:prompt "Install this hardware from the heap?"
                              :yes-ability {:cost [(->c :lose-click 1)]
                                            :async true
                                            :effect (req (let [target-card (first (filter #(= (:printed-title %) (:printed-title card)) (:discard runner)))]
                                                           (runner-install state side (assoc eid :source card :source-type :runner-install) target-card {:msg-keys {:display-origin true
                                                                                                                                                                    :install-source card}})))}}}
                            card nil))}
            {:event :runner-turn-ends
             :req (req (and
                         (installed? card)
                         (some #{:hq} (:successful-run runner-reg))
                         (some #{:rd} (:successful-run runner-reg))
                         (some #{:archives} (:successful-run runner-reg))))
             :msg "add itself to the score area as an assassination agenda worth 0 agenda points"
             :async true
             :effect (req (as-agenda state :runner card 0)
                          (if (= 3 (count (filter #(= (:printed-title %) (:printed-title card))
                                                  (get-in @state [:runner :scored]))))
                            (do (system-msg state side "wins the game")
                                (win state :runner "assassination plot (Jeitinho)")
                                (effect-completed state side eid))
                            (effect-completed state side eid)))}]})

(defcard "Keiko"
  (letfn [(companion? [card]
            (and (not (facedown? card)) (has-subtype? card "Companion")))
          (valid-ctx? [[{:keys [card] :as ctx} & rem]]
            (or ((every-pred runner? installed? companion?) card)
                (and rem (valid-ctx? rem))))]
    {:static-abilities [(mu+ 2)]
     :events [{:event :spent-credits-from-card
               :once-per-instance true
               :req (req (and (some #(valid-ctx? [%]) targets)
                              (first-event? state side :spent-credits-from-card valid-ctx?)
                              (no-event? state side :runner-install #(companion? (:card (first %))))))
               :msg "gain 1 [Credit]"
               :async true
               :effect (effect (gain-credits :runner eid 1))}
              {:event :runner-install
               :req (req (and (companion? (:card context))
                              (first-event? state side :runner-install #(companion? (:card (first %))))
                              (no-event? state side :spent-credits-from-card valid-ctx?)))
               :msg "gain 1 [Credit]"
               :async true
               :effect (effect (gain-credits :runner eid 1))}]}))

(defcard "Knobkierie"
  {:static-abilities [(virus-mu+ 3)]
   :events [{:event :successful-run
             :skippable true
             :interactive (req true)
             :optional {:req (req (and (first-event? state :runner :successful-run)
                                       (pos? (count-virus-programs state))))
                        :prompt "Place 1 virus counter?"
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability {:prompt "Choose an installed virus program to place 1 virus counter on"
                                      :choices {:card #(and (installed? %)
                                                            (has-subtype? % "Virus")
                                                            (program? %))}
                                      :msg (msg "place 1 virus counter on " (:title target))
                                      :async true
                                      :effect (effect (add-counter eid target :virus 1))}}}]
   :abilities [(set-autoresolve :auto-fire "Knobkierie")]})

(defcard "Lemuria Codecracker"
  {:abilities [{:action true
                :async true
                :cost [(->c :click 1) (->c :credit 1)]
                :req (req (some #{:hq} (:successful-run runner-reg)))
                :choices {:card installed?}
                :label "Expose a card"
                :effect (effect (expose eid [target] {:card card}))}]})

(defcard "LilyPAD"
  {:events [{:event :runner-install
             :optional {:prompt "Draw 1 card?"
                        :waiting-prompt true
                        :req (req (and
                                    (program? (:card target))
                                    (first-event? state :runner :runner-install #(program? (:card (first %))))))
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability (draw-abi 1)
                        :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
   :static-abilities [(mu+ 2)]
   :abilities [(set-autoresolve :auto-fire "LilyPAD")]})

(defcard "LLDS Memory Diamond"
  {:static-abilities [(link+ 1)
                      (runner-hand-size+ 1)
                      (mu+ 1)]})

(defcard "LLDS Processor"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (has-subtype? (:card context) "Icebreaker"))
             :effect (effect (pump (:card context) 1 :end-of-turn))}]})

(defcard "Lockpick"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Decoder")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Logos"
  {:static-abilities [(mu+ 1)
                      (runner-hand-size+ 1)]
   :events [{:event :agenda-scored
             :prompt "Choose a card"
             :msg "add 1 card from the stack to the grip"
             :choices (req (cancellable (:deck runner)))
             :effect (effect (trigger-event :searched-stack)
                             (shuffle! :deck)
                             (move target :hand))}]})

(defcard "Lucky Charm"
  {:prevention [{:prevents :end-run
                 :type :ability
                 :ability {:req (req (and (some #{:hq} (:successful-run runner-reg))
                                          (pos? (:remaining context))
                                          (= :corp (get-in @state [:prevent :end-run :source-player]))))
                           :cost [(->c :remove-from-game)]
                           :async true
                           :msg "prevent the run from ending"
                           :effect (req (prevent-end-run state side eid))}}]})

(defcard "Mâché"
  (letfn [(pred [{:keys [card accessed]}]
            (and accessed (corp? card)))]
    {:abilities [(draw-abi 1 nil {:cost [(->c :power 3)]
                                  :keep-menu-open :while-3-power-tokens-left})]
     :events [{:event :runner-trash
               :once-per-instance true
               :req (req (and (some pred targets)
                              (first-event? state side :runner-trash (fn [targets] (some pred targets)))))
               :async true
               :effect (req (let [target (some #(when (pred %) (:card %)) targets)
                                  cost (trash-cost state side target)]
                              (if cost
                                (do (system-msg state side (str "uses " (:title card) " to place "
                                                            (quantify cost "power counter")
                                                            " on itself"))
                                    (add-counter state side eid card :power cost))
                                (effect-completed state side eid))))}]}))

(defcard "Madani"
  {:static-abilities []
   :abilities [{:cost [(->c :click 1)]
                :label "Host any number of programs"
                :prompt "Choose any number of program"
                :action true
                :choices {:req (req (and (in-hand? target) (program? target) ))
                          :max (req (count (filter program? (:hand runner))))}
                :msg (msg "host " (enumerate-cards targets :sorted))
                :effect (req (doseq [t targets] (host state side card t)))}
               {:cost [(->c :credit 0)]
                :label "Install a hosted program"
                :async true
                :once :per-turn
                :prompt "Choose a hosted program to install"
                :choices {:req (req (and (program? target)
                                         (runner-can-pay-and-install? state side eid target {:no-toast true})
                                         (same-card? (:host target) card)))}
                :effect (req (runner-install state side eid target {:display-origin true
                                                                    :install-source card}))}]})

(defcard "Maglectric Rapid (748 Mod)"
  {:events [{:event :successful-run
             :prompt "Derez a card?"
             :skippable true
             ;; not - agendas cannot be rezzed, but faceup agendas are marked as 'rezzed' for the
             ;; diff system, so this is just a safeguard
             :req (req (and
                         (= :hq (target-server context))
                         (some (every-pred rezzed? (complement agenda?))
                               (all-installed state :corp))))
             :choices {:card (every-pred installed? corp? rezzed? (complement agenda?))}
             :cost [(->c :trash-self 1)]
             :async true
             :effect (req (derez state side eid target))}]})

(defcard "Marrow"
  {:static-abilities [(mu+ 1)
                      (runner-hand-size+ 3)]
   :on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :events [(assoc (sabotage-ability 1)
                   :event :agenda-scored
                   :interactive (req true))]})

(defcard "Masterwork (v37)"
  {:static-abilities [(mu+ 1)]
   :events [{:event :run
             :interactive (req true)
             :optional
             {:req (req (some #(and (hardware? %)
                                    (runner-can-pay-and-install? state side (assoc eid :source card) % {:cost-bonus 1}))
                              (all-cards-in-hand* state :runner)))
              :prompt "Pay 1 [Credit] to install a piece of hardware?"
              :yes-ability {:async true
                            :prompt "Choose a piece of hardware"
                            :choices {:req (req (and (in-hand*? state target)
                                                     (hardware? target)
                                                     (runner-can-pay-and-install? state side (assoc eid :source card) target {:cost-bonus 1})))}
                            :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus 1
                                                                                                                          :msg-keys {:display-origin true
                                                                                                                                     :install-source card}}))}}}
            (draw-abi 1 nil {:event :runner-install
                             :interactive (req true)
                             :req (req (and (hardware? (:card context))
                                            (first-event? state side :runner-install #(hardware? (:card (first %))))))})]})

(defcard "Māui"
  {:x-fn (req (count (get-in corp [:servers :hq :ices])))
   :static-abilities [(mu+ 2)]
   :recurring (get-x-fn)
   :interactions {:pay-credits {:req (req (= [:hq] (get-in @state [:run :server])))
                                :type :recurring}}})

(defcard "Maw"
  {:static-abilities [(mu+ 2)]
   :events [{:event :post-access-card
             :label "Trash a card from HQ"
             :async true
             ;; todo - no-trash-or-steal to an actual event.
             :req (req (and (= 1 (get-in @state [:runner :register :no-trash-or-steal]))
                            (pos? (count (:hand corp)))
                            (not (in-discard? target))
                            (not (in-scored? target))))
             :once :per-turn
             :effect (req (let [card-to-trash (first (shuffle (:hand corp)))
                                card-seen? (same-card? target card-to-trash)
                                card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                  card-to-trash)]
                            (continue-ability
                              state side
                              {:effect (req (trash state :corp eid card-to-trash {:cause-card card :cause :forced-to-trash}))
                               :async true
                               :msg (str "force the Corp to trash a random card from HQ"
                                         (when card-seen? " (" (:title card-to-trash)")"))}
                              card nil)))}]})

(defcard "Maya"
  {:static-abilities [(mu+ 2)]
   :events [{:event :post-access-card
             :optional
             {:req (req (in-deck? (second targets)))
              :once :per-turn
              :prompt (msg "Move " (:title target) " to the bottom of R&D?")
              :yes-ability
              {:msg "move the card just accessed to the bottom of R&D"
               :async true
               :effect (req (move state :corp target :deck)
                            (gain-tags state :runner eid 1))}}}]})

(defcard "MemStrips"
  {:static-abilities [(virus-mu+ 3)]})

(defcard "Mind's Eye"
  {:implementation "Power counters added automatically"
   :static-abilities [(mu+ 1)]
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :rd (target-server context)))
             :async true
             :effect (effect (add-counter eid card :power 1))}]
   :abilities [{:action true
                :async true
                :cost [(->c :click 1) (->c :power 3)]
                :msg "breach R&D"
                :effect (req (breach-server state side eid [:rd] {:no-root true}))}]})

(defcard "Mirror"
  {:static-abilities [(mu+ 2)]
   :events [{:event :successful-run
             :skippable true
             :async true
             :req (req (= :rd (target-server context)))
             :effect (effect (continue-ability
                               {:prompt "Choose a card and replace 1 spent [Recurring Credits] on it"
                                :choices {:card #(< (get-counters % :recurring) (:recurring (card-def %) 0))}
                                :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                :async true
                                :effect (effect (add-counter eid target :recurring 1))}
                               card nil))}]})

(defcard "Monolith"
  (let [mhelper
        (fn mh [n]
          {:prompt "Choose a program to install"
           :choices {:req (req (and (program? target)
                                    (in-hand*? state target)
                                    (runner-can-pay-and-install? state side (assoc eid :source card) target {:cost-bonus -4})))}
           :async true
           :effect (req (wait-for (runner-install state side target {:cost-bonus -4
                                                                     :msg-keys {:install-source card
                                                                                :display-origin true}})
                                  (continue-ability state side (when (< n 3) (mh (inc n))) card nil)))})]
    {:static-abilities [(mu+ 3)]
     :on-install {:async true
                  :effect (effect (continue-ability (mhelper 1) card nil))}
     :prevention [{:prevents :damage
                   :type :ability
                   :ability {:async true
                             :cost [(->c :trash-program-from-hand 1)]
                             :msg (msg "prevent 1 " (damage-name state) " damage")
                             :req (req (and (not (= :meat (:type context)))
                                            (preventable? context)))}}]}))

(defcard "Mu Safecracker"
  {:implementation "Stealth credit restriction not enforced"
   :events [{:event :successful-run
             :skippable true
             :optional
             {:req (req (and (= :hq (target-server context))
                             (some #(has-subtype? % "Stealth")
                                   (all-active state :runner))))
              :prompt "Pay 1 [Credits] to access 1 additional card?"
              :yes-ability
              {:cost [(->c :credit 1 {:stealth 1})]
               :msg "access 1 additional card from HQ"
               :effect (effect (register-events
                                card [(breach-access-bonus :hq 1 {:duration :end-of-run})]))}}}
            {:event :successful-run
             :skippable true
             :optional
             {:req (req (and (= :rd (target-server context))
                             (some #(has-subtype? % "Stealth")
                                   (all-active state :runner))))
              :prompt "Pay 2 [Credits] to access 1 additional card?"
              :yes-ability
              {:cost [(->c :credit 2 {:stealth :all-stealth})]
               :msg "access 1 additional card from R&D"
               :effect (effect (register-events
                                card [(breach-access-bonus :rd 1 {:duration :end-of-run})]))}}}]})

(defcard "Muresh Bodysuit"
  {:prevention [{:prevents :damage
                 :type :event
                 :max-uses 1
                 :mandatory true
                 :ability {:async true
                           :req (req (and (= :meat (:type context))
                                          (first-event? state side :pre-damage-flag
                                                        #(= :meat (:type (first %))))
                                          (preventable? context)))
                           :msg "reduce the pending meat damage by 1"
                           :effect (req (prevent-damage state side eid 1))}}]})

(defcard "Net-Ready Eyes"
  {:on-install {:async true
                :msg "suffer 2 meat damage"
                :effect (effect (damage eid :meat 2 {:unboostable true :card card}))}
   :events [{:event :run
             :req (req (some #(and (program? %)
                                   (has-subtype? % "Icebreaker"))
                             (all-active-installed state :runner)))
             :choices {:card #(and (installed? %)
                                   (has-subtype? % "Icebreaker"))}
             :msg (msg "give " (:title target) " +1 strength")
             :effect (effect (pump target 1 :end-of-run))}]})

(defcard "NetChip"
  (letfn [(netchip-count
            [state]
            (count (filter #(= (:title %) "NetChip") (all-active-installed state :runner))))]
    {:enforce-conditions {:req (req (let [first-program (first (filter program? (:hosted card)))]
                                    (and first-program (> (expected-mu state first-program) (netchip-count state)))))
                          :silent (req true)
                          :msg (msg "trash " (card-str state (first (filter program? (:hosted card)))) " for violating hosting restrictions")
                          :async true
                          :effect (req (let [first-program (first (filter program? (:hosted card)))]
                                         (system-msg state nil (card-str state (first (filter program? (:hosted card)))) " is trashed for violating hosting restrictions")
                                         (trash-cards state side eid [first-program] {:unpreventable true :game-trash true})))}
     :static-abilities [{:type :can-host
                        :req (req (and (program? target)
                                       (<= (expected-mu state target) (netchip-count state))))
                         :max-mu (req (netchip-count state))
                         :max-cards 1
                         :no-mu true}]}))

(defcard "Obelus"
  {:static-abilities [(mu+ 1)
                      (runner-hand-size+ (req (count-tags state)))]
   :events [{:event :run-ends
             :interactive (req true) ;;interact with other run-ends effects which modify the deck (ie boomerang)
             :req (req (and (:successful target)
                            (#{:rd :hq} (target-server target))
                            (first-event? state side :run-ends
                                          #(and (:successful (first %))
                                                (#{:rd :hq} (target-server (first %)))))))
             :msg (msg "draw " (quantify (total-cards-accessed target) "card"))
             :async true
             :effect (effect (draw eid (total-cards-accessed target)))}]})

(defcard "Omni-drive"
  {:recurring 1
   :enforce-conditions {:req (req (let [first-program (first (filter program? (:hosted card)))]
                                    (and first-program (> (expected-mu state first-program) 1))))
                        :silent (req true)
                        :msg (msg "trash " (card-str state (first (filter program? (:hosted card)))) " for violating hosting restrictions")
                        :async true
                        :effect (req (let [first-program (first (filter program? (:hosted card)))]
                                       (system-msg state nil (card-str state (first (filter program? (:hosted card)))) " is trashed for violating hosting restrictions")
                                       (trash-cards state side eid [first-program] {:unpreventable true :game-trash true})))}
   :static-abilities [{:type :can-host
                       :req (req (and (program? target)
                                      (<= (expected-mu state target) 1)))
                       :max-mu 1
                       :max-cards 1
                       :no-mu true}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (program? target)
                                               (same-card? card (:host target))))
                                :type :recurring}}})

(defcard "PAN-Weave"
  {:on-install {:async true
                :msg "suffer 1 meat damage"
                :effect (effect (damage eid :meat 1 {:unboostable true :card card}))}
   :events [{:event :successful-run
             :automatic :drain-credits
             :req (req (and
                         (= :hq (first (:server target)))
                         (first-event? state side :successful-run #(= :hq (first (:server (first %)))))))
             :msg "force the Corp to lose 1 [Credits]"
             :async true
             :effect (req (if (pos? (:credit corp))
                            (wait-for (lose-credits state :corp 1)
                                      (system-msg state side (str "uses " (:title card) " to gain 1 [Credits]"))
                                      (gain-credits state :runner eid 1))
                            (effect-completed state side eid)))}]})

(defcard "Pantograph"
  (let [install-ability
        {:async true
         :prompt "Choose a card to install"
         :waiting-prompt true

         :change-in-game-state {:req (req (seq (all-cards-in-hand* state :runner))) :silent true}
         :choices {:req (req (and (runner? target)
                                  (in-hand*? state target)
                                  (not (event? target))
                                  (runner-can-pay-and-install? state side eid target {:no-toast true})))}
         :effect (effect (runner-install
                          (assoc eid :source card :source-type :runner-install)
                          target {:msg-keys {:install-source card
                                             :display-origin true}}))
         :cancel-effect (effect (system-msg :runner (str "declines to use " (:title card) " to install a card"))
                                (effect-completed eid))}
        gain-credit-ability
        {:interactive (req true)
         :async true
         :msg "gain 1 [Credits]"
         :effect (req (wait-for (gain-credits state :runner 1)
                                (continue-ability state side install-ability card nil)))}]
    {:static-abilities [(mu+ 1)]
     :events [(assoc gain-credit-ability :event :agenda-scored)
              (assoc gain-credit-ability :event :agenda-stolen)]}))

(defcard "Paragon"
  {:static-abilities [(mu+ 1)]
   :events [{:event :successful-run
             :automatic :pre-draw
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional
             {:req (req (first-event? state side :successful-run))
              :autoresolve (get-autoresolve :auto-fire)
              :waiting-prompt true
              :prompt "Gain 1 [Credit] and look at the top card of the stack?"
              :yes-ability
              {:msg "gain 1 [Credit] and look at the top card of the stack"
               :async true
               :effect
               (req
                 (wait-for (gain-credits state :runner 1)
                           (continue-ability
                             state :runner
                             {:optional
                              {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of the stack?")
                               :yes-ability
                               {:msg "add the top card of the stack to the bottom"
                                :effect (effect (move :runner (first (:deck runner)) :deck))}
                               :no-ability
                               {:effect (effect (system-msg "does not add the top card of the the stack to the bottom"))}}}
                             card nil)))}
              :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
   :abilities [(set-autoresolve :auto-fire "Paragon")]})

(defcard "Patchwork"
  (let [install-word (fn [c] (if (event? c) "play" "install"))
        patchwork-ability {:once :per-turn
                           :effect (effect (update! (assoc-in card [:special :patchwork] true)))}
        patchwork-manual-prognosis {:cost [(->c :click 1)]
                                    :action true
                                    :once :per-turn
                                    :label "Manually resolve patchwork"
                                    :req (req (and (seq (:hand runner))
                                                   (can-trigger? state side eid patchwork-ability card targets)))

                                    :prompt "Designate a card to play or install"
                                    :choices {:req (req (and (runner? target)
                                                             (in-hand*? state target)))}
                                    :waiting-prompt true
                                    :async true
                                    :effect
                                    (req (let [to-play target]
                                           (continue-ability
                                             state side
                                             {:prompt (msg "Designate a card to trash")
                                              :choices {:card (every-pred runner? in-hand?)
                                                        :all true}
                                              :async true
                                              :effect (req
                                                        (register-once state side patchwork-ability card)
                                                        (let [to-trash target]
                                                          (continue-ability
                                                            state side
                                                            (if (same-card? to-trash to-play)
                                                              {:msg (msg "trash " (:title to-trash) " from the Grip, and is no longer able to " (install-word to-trash) " it")
                                                               :async true
                                                               :effect (req (trash state side eid to-trash {:cause-card card}))}
                                                              {:msg (msg "trash " (:title to-trash) " to " (install-word to-play) " " (:title to-play) " from the Grip, paying 2 [Credits] less")
                                                               :async true
                                                               :effect (req (wait-for
                                                                              (trash state side to-trash {:cause-card card})
                                                                              (if (event? to-play)
                                                                                (play-instant state :runner eid to-play {:cost-bonus -2})
                                                                                (runner-install state :runner eid to-play {:cost-bonus -2}))))})
                                                            card nil)))}
                                             card nil)))}]
    {:static-abilities [(mu+ 1)]
     :abilities [patchwork-manual-prognosis]
     :implementation "click on patchwork to manually resolve it (for tricks)"
     :interactions
     {:pay-credits
      {:req (req (and (#{:play :runner-install} (:source-type eid))
                      ;; We need at least one card (that is not the card played) in hand
                      (not-empty (remove (partial same-card? target) (:hand runner)))
                      ;; Patchwork wasn't used in the traditional way
                      (not (get-in card [:special :patchwork]))
                      ;; Check if Patchwork can trigger
                      (can-trigger? state side eid patchwork-ability card targets)))
       :custom-amount 2
       :custom (req (let [cost-type (str (when (= :play (:source-type eid)) "play")
                                         (when (= :runner-install (:source-type eid)) "install"))
                          targetcard target]
                      (continue-ability
                        state side
                        {:prompt (str "Trash a card to lower the " cost-type
                                      " cost of " (:title targetcard) " by 2 [Credits]")
                         :async true
                         :choices {:card #(and (in-hand? %)
                                               (runner? %)
                                               (not (same-card? % targetcard)))}
                         :msg (msg "trash " (:title target) " to lower the " cost-type " cost of "
                                   (:title targetcard) " by 2 [Credits]")
                         ; provide 2 credits
                         :effect (req (wait-for (trash state side target {:unpreventable true
                                                                          :cause-card card})
                                                (register-once state side patchwork-ability card)
                                                (effect-completed state side (make-result eid 2))))
                         ; provide 0 credits
                         :cancel-effect (effect (effect-completed (make-result eid 0)))}
                        card nil)))
       :type :custom
       :cost-reduction true}}}))

(defcard "Pennyshaver"
  {:static-abilities [(mu+ 1)]
   :events [{:event :successful-run
             :silent (req true)
             :async true
             :msg "place 1 [Credits]"
             :effect (req (add-counter state :runner eid card :credit 1))}]
   :abilities [{:action true
                :cost [(->c :click 1)]
                :label "Gain 1 [Credits]. Take all hosted credits"
                :async true
                :msg (msg "gain " (inc (get-counters card :credit)) " [Credits]")
                :effect (req (let [credits (inc (get-counters card :credit))]
                               (play-tiered-sfx state side "click-credit" credits 3)
                               (wait-for
                                 (add-counter state side card :credit (-(dec credits)))
                                 (gain-credits state :runner eid credits))))}]})

(defcard "Plascrete Carapace"
  {:data {:counter {:power 4}}
   :prevention [{:prevents :damage
                 :type :ability
                 :ability {:async true
                           :cost [(->c :power 1)]
                           :msg "prevent 1 meat damage"
                           :req (req (and (preventable? context)
                                          (= :meat (:type context))))
                           :effect (req (prevent-damage state side eid 1))}}]
   :events [(trash-on-empty :power)]})

(defcard "Poison Vial"
  (auto-icebreaker
    {:data {:counter {:power 3}}
     :events [(trash-on-empty :power)]
     :abilities [(break-sub [(->c :power 1)] 2 "All" {:req (req (any-subs-broken? current-ice))})]}))

(defcard "Polyhistor"
  (let [abi {:optional
             {:prompt "Draw 1 card to force the Corp to draw 1 card?"
              :waiting-prompt true
              :yes-ability {:msg "draw 1 card and force the Corp to draw 1 card"
                            :async true
                            :effect (req (wait-for (draw state :runner 1)
                                                   (draw state :corp eid 1)))}
              :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
    {:static-abilities [(mu+ 1)
                        (link+ 1)]
     :events [{:event :pass-ice
               :req (req (and (= (:server run) [:hq])
                              (= (:position run) 0) ; trigger when last piece of ice is passed
                              (pos? (count (:deck runner)))))
               :async true
               :once :per-turn
               :effect (req (continue-ability state :runner abi card nil))}
              {:event :run
               :req (req (and (= (:server target) [:hq])
                              (zero? (:position target)) ; trigger on unprotected HQ
                              (pos? (count (:deck runner)))))
               :async true
               :once :per-turn
               :effect (req (continue-ability state :runner abi card nil))}]}))

(defcard "Prepaid VoicePAD"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and
                                           (event? target)
                                           (or (= 0 (count (:cost-paid eid)))
                                               (:x-cost eid))
                                           (= :play (:source-type eid))))
                                :type :recurring}}})

(defcard "Prognostic Q-Loop"
  {:events [{:event :run
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional {:req (req (and (first-event? state side :run)
                                       (pos? (count (:deck runner)))))
                        :autoresolve (get-autoresolve :auto-fire)
                        :prompt "Look at top 2 cards of the stack?"
                        :yes-ability
                        {:msg "look at the top 2 cards of the stack"
                         :choices ["OK"]
                         :prompt (msg "The top 2 cards of the stack are "
                                      (enumerate-cards (take 2 (:deck runner))))}}}]
   :abilities [(set-autoresolve :auto-fire "Prognostic Q-Loop")
               {:label "Reveal and install top card of the stack"
                :once :per-turn
                :cost [(->c :credit 1)]
                :change-in-game-state {:req (req (pos? (count (:deck runner))))}
                :msg (msg "reveal " (:title (first (:deck runner))) " from the top of the stack")
                :async true
                :effect
                (req
                  (wait-for
                    (reveal state side (first (:deck runner)))
                    (continue-ability
                      state side
                      (let [top-card (first (:deck runner))]
                        {:optional
                         {:req (req (and (or (program? top-card)
                                             (hardware? top-card))
                                         (runner-can-pay-and-install? state side (assoc eid :source-type :runner-install) top-card)))
                          :prompt (msg "Install " (:title top-card) "?")
                          :yes-ability
                          {:async true
                           :effect (effect (runner-install (assoc eid :source-type :runner-install) top-card {:msg-keys {:display-origin true
                                                                                                                         :origin-index 0
                                                                                                                         :install-source card}}))}}})
                      card nil)))}]})

(defcard "Public Terminal"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :play (:source-type eid))
                                               (has-subtype? target "Run")))
                                :type :recurring}}})

(defcard "Q-Coherence Chip"
  {:static-abilities [(mu+ 1)]
   :events (let [e {:async true
                    :interactive (req true)
                    :req (req (and (installed? (:card target))
                                   (program? (:card target))))
                    :msg "trash itself"
                    :effect (effect (trash eid card {:cause-card card}))}]
             [(assoc e :event :runner-trash)
              (assoc e :event :corp-trash)])})

(defcard "Qianju PT"
  {:flags {:runner-phase-12 (req true)}
   :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                :once :per-turn
                :req (req (:runner-phase-12 @state))
                :cost [(->c :lose-click 1)]
                :msg "avoid the first tag received until [their] next turn"
                :effect (req (let [current-turn (:turn @state)
                                   lingering (register-lingering-effect
                                               state side card
                                               {:type :forced-to-avoid-tag
                                                :duration :until-next-runner-turn-begins
                                                :value true})]
                               (register-events
                                 state side card
                                 [{:event :tag-interrupt
                                   :unregister-once-resolved true
                                   :duration :until-next-runner-turn-begins
                                   :async true
                                   :msg "avoid 1 tag"
                                   :effect (req
                                             (unregister-effect-by-uuid state side lingering)
                                             (prevent-tag state :runner eid 1))}])))}]})

(defcard "R&D Interface"
  {:events [(breach-access-bonus :rd 1)]})

(defcard "Rabbit Hole"
  {:static-abilities [(link+ 1)]
   :on-install
   {:optional
    {:req (req (some #(when (= (:title %) (:title card)) %) (:deck runner)))
     :prompt (msg "Install another copy of " (:title card) "?")
     :yes-ability {:async true
                   :effect (req (trigger-event state side :searched-stack)
                                (shuffle! state :runner :deck)
                                (if-let [c (some #(when (= (:title %) (:title card)) %)
                                                   (:deck runner))]
                                  (runner-install state side eid c {:msg-keys {:install-source card
                                                                               :display-origin true}})
                                  (effect-completed state side eid)))}}}})

(defcard "Ramujan-reliant 550 BMI"
  (letfn [(max-trash [state] (inc (count (filter #(= (:title %) "Ramujan-reliant 550 BMI") (all-installed state :runner)))))]
    {:prevention [{:prevents :damage
                   :type :ability
                   :ability {:async true
                             :cost [(->c :trash-can)]
                             :msg (msg "prevent up to " (max-trash state) " damage")
                             :req (:req (prevent-up-to-n-damage 1 #{:net :core :brain}))
                             :effect (req (let [prevented (:prevented context)]
                                            (wait-for (resolve-ability
                                                        state side
                                                        (prevent-up-to-n-damage (max-trash state) #{:net :core :brain})
                                                        card targets)
                                                      (let [prevented-this-instance (- (get-in @state [:prevent :damage :prevented]) prevented)]
                                                        (system-msg state side (str "uses " (:title card) " to trash the top " prevented-this-instance " cards of the stack"))
                                                        (mill state :runner eid :runner prevented-this-instance)))))}}]}))

(defcard "Recon Drone"
  {:prevention [{:prevents :damage
                 :type :ability
                 :ability {:async true
                           :fake-cost [(->c :trash-can)]
                           :req (req (and (preventable? context)
                                          (same-card? (:source-card context) (:access @state))))
                           :effect (req (continue-ability
                                          state side
                                          {:cost [(->c :trash-can) (->c :x-credits 0 {:maximum (:remaining context)})]
                                           :msg (msg "prevent " (cost-value eid :x-credits) " " (damage-type state) " damage")
                                           :async true
                                           :effect (req (prevent-damage state side eid (cost-value eid :x-credits)))}
                                          card nil))}}]})

(defcard "Record Reconstructor"
  {:events [(successful-run-replace-breach
              {:target-server :archives
               :ability
               {:prompt "Choose one faceup card to add to the top of R&D"
                :req (req (seq (filter faceup? (:discard corp))))
                :choices (req (filter faceup? (:discard corp)))
                :msg (msg "add " (:title target) " to the top of R&D")
                :effect (effect (move :corp target :deck {:front true}))}})]})

(defcard "Reflection"
  {:static-abilities [(mu+ 1)
                      (link+ 1)]
   :events [{:event :jack-out
             :async true
             :effect (req (let [card (first (shuffle (:hand corp)))]
                            (system-msg state :runner (str  "force the Corp to reveal " (:title card) " from HQ"))
                            (reveal state :corp eid card)))}]})

(defcard "Replicator"
  ;; TODO - NOTE: this should allow you to
  ;; 1) shuffle the deck even if you have no copy of the hardware
  ;; 2) fail to find the hardware and shuffle the deck even if a copy exists
  ;;  -nbkelly, 2025
  (letfn [(hardware-and-in-deck? [target runner]
            (and (hardware? target)
                 (some #(= (:title %) (:title target)) (:deck runner))))]
    {:events [{:event :runner-install
               :interactive (req (hardware-and-in-deck? (:card context) runner))
               :silent (req (not (hardware-and-in-deck? (:card context) runner)))
               :optional
               {:prompt (msg "Search the stack for another copy of " (:title (:card context)) " and add it to the grip?")
                :req (req (hardware-and-in-deck? (:card context) runner))
                :yes-ability
                {:msg (msg "add a copy of " (:title (:card context)) " from the stack to the grip")
                 :effect (effect (trigger-event :searched-stack)
                           (shuffle! :deck)
                           (move (some #(when (= (:title %) (:title (:card context))) %) (:deck runner)) :hand))}}}]}))

(defcard "Respirocytes"
  (let [ability {:once :per-turn
                 :msg "draw 1 card and place a power counter"
                 :async true
                 :effect (req (wait-for
                                (draw state :runner 1)
                                (wait-for
                                  (add-counter state side (get-card state card) :power 1)
                                  (if (= 3 (get-counters (get-card state card) :power))
                                    (do (system-msg state :runner (str "trashes " (:title card) " as it reached 3 power counters"))
                                        (trash state side eid card {:unpreventable true
                                                                    :cause-card card}))
                                    (effect-completed state side eid)))))}
        event {:req (req (zero? (count (:hand runner))))
               :async true
               :effect (effect (continue-ability ability card targets))}]
    {:implementation "Only watches trashes, playing events, and installing. Doesn't know about your hand size pre-install."
     :on-install {:async true
                  :msg "suffer 1 meat damage"
                  :effect (effect (damage eid :meat 1 {:unboostable true
                                                       :card card}))}
     :events [(assoc event :event :play-event)
              (assoc event :event :runner-hand-changed?)
              (assoc event
                     :event :runner-trash
                     :once-per-instance true
                     :req (req (and (some #(and (runner? (:card %))
                                                (in-hand? (:card %)))
                                          targets)
                                    (zero? (count (:hand runner))))))
              (assoc event
                     :event :corp-trash
                     :once-per-instance true
                     :req (req (and (some #(and (runner? (:card %))
                                                (in-hand? (:card %)))
                                          targets)
                                    (zero? (count (:hand runner))))))
              (assoc event
                     :event :runner-install
                     :req (req (and (some #{:hand} (:previous-zone (:card context)))
                                    (zero? (count (:hand runner))))))
              {:event :runner-turn-begins
               :automatic :draw-cards
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}
              {:event :corp-turn-begins
               :automatic :draw-cards
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}]
     :abilities [ability]}))

(defcard "Rubicon Switch"
  {:abilities [{:action true
                :cost [(->c :click 1)(->c :x-credits)]
                :label "Derez a piece of ice rezzed this turn"
                ;; TODO - once elevation is out and the ncigs changes are in, add an ncigs catch for if the player just wastes their money
                :once :per-turn
                :async true
                :effect (req (let [payment-eid eid
                                   spent-credits (cost-value eid :x-credits)]
                               (continue-ability
                                 state side
                                 {:choices {:req (req (and (ice? target)
                                                           (= :this-turn (:rezzed target))
                                                           (<= (rez-cost state :corp target nil) spent-credits)))}
                                  :async true
                                  :effect (req (derez state side eid target {:msg-keys {:include-cost-from-eid payment-eid}}))}
                                 card nil)))}]})

(defcard "Security Chip"
  {:abilities [{:label "Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                :msg (msg "add " (get-link state)
                          " strength to " (:title target)
                          " until the end of the run")
                :req (req (:run @state))
                :prompt "Choose one non-Cloud icebreaker"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (installed? %))}
                :cost [(->c :trash-can)]
                :effect (effect (pump target (get-link state) :end-of-run))}
               {:label "Add [Link] strength to any Cloud icebreakers until the end of the run"
                :msg (msg "add " (get-link state)
                          " strength to " (count targets)
                          " Cloud icebreakers until the end of the run")
                :req (req (:run @state))
                :prompt "Choose any number of Cloud icebreakers"
                :choices {:max 50
                          :card #(and (has-subtype? % "Icebreaker")
                                      (has-subtype? % "Cloud")
                                      (installed? %))}
                :cost [(->c :trash-can)]
                :effect (req (doseq [t targets]
                               (pump state side t (get-link state) :end-of-run)
                               (update-breaker-strength state side t)))}]})

(defcard "Security Nexus"
  {:static-abilities [(mu+ 1)
                      (link+ 1)]
   :events [{:event :encounter-ice
             :skippable true
             :interactive (req true)
             :optional
             {:prompt "Trace 5 to bypass current ice?"
              :once :per-turn
              :yes-ability
              {:msg "force the Corp to initiate a trace"
               :trace {:base 5
                       :successful {:msg "give the Runner 1 tag and end the run"
                                    :async true
                                    :effect (req (wait-for (gain-tags state :runner 1)
                                                           (end-run state side eid card)))}
                       :unsuccessful {:msg (msg "bypass " (card-str state current-ice))
                                      :effect (req (bypass-ice state))}}}}}]})

(defcard "Severnius Stim Implant"
  (letfn [(implant-fn [srv kw]
            {:prompt "Choose at least 2 cards to trash"
             :cost [(->c :click 1)]
             :choices {:max (req (count (:hand runner)))
                       :card #(and (runner? %)
                                   (in-hand? %))}
             :msg (msg "trash " (quantify (count targets) "card")
                       " and access " (quantify (quot (count targets) 2) "additional card"))
             :async true
             :effect (req (let [bonus (quot (count targets) 2)]
                            (wait-for (trash-cards state side targets {:unpreventable true
                                                                       :cause-card card})
                                      (register-events
                                        state side card
                                        [(breach-access-bonus kw bonus {:duration :end-of-run})])
                                      (make-run state side eid srv card))))})]
    {:abilities [{:action true
                  :req (req (<= 2 (count (:hand runner))))
                  :label "Run HQ or R&D"
                  :prompt "Choose one"
                  :waiting-prompt true
                  :choices ["HQ" "R&D"]
                  :async true
                  :effect (effect (continue-ability (implant-fn target (if (= target "HQ") :hq :rd)) card nil))}]}))

(defcard "Şifr"
  (letfn [(index-of [pred coll]
            (some (fn [[idx item]] (when (pred item) idx))
                  (map-indexed vector coll)))
          (gather-pre-sifr-effects [sifr state side eid target targets]
            ;; This is needed because of the stupid ass rulings about how Sifr modifies
            ;; ice strength: Sifr only lowers the ice to 0 at the point it's activated,
            ;; and then other abilities (Sandburg, etc) can raise it back after, which
            ;; is DUMB, so that means we have to on the fly calculate what the strength
            ;; of the ice is at the moment Sifr would affect it.
            (if (card-flag? target :cannot-lower-strength true)
              0
              (->> (:effects @state)
                   (filter #(= :ice-strength (:type %)))
                   (filter #(if-not (:req %)
                              true
                              ((:req %) state side eid (get-card state (:card %)) (cons target targets))))
                   (#(split-at (index-of (fn [item] (same-card? sifr (:card item))) %) %))
                   (first)
                   (mapv #(if-not (fn? (:value %))
                            (:value %)
                            ((:value %) state side eid (get-card state (:card %)) (cons target targets))))
                   (reduce +)
                   (abs))))]
    {:static-abilities [(mu+ 2)]
     :events [{:event :encounter-ice
               :skippable true
               :interactive (req true)
               :optional
               {:prompt "Lower your maximum hand size by 1 to reduce the strength of encountered ice to 0?"
                :once :per-turn
                :yes-ability
                {:msg (msg "lower [their] maximum hand size by 1 and reduce the strength of " (:title current-ice) " to 0")
                 :effect (effect
                           (register-lingering-effect
                             card
                             {:type :hand-size
                              :duration :until-runner-turn-begins
                              :req (req (= :runner side))
                              :value -1})
                           (register-lingering-effect
                             :runner card
                             (let [ice current-ice]
                               {:type :ice-strength
                                :duration :end-of-encounter
                                :req (req (same-card? target ice))
                                :value (req (- (+ (:strength target)
                                                  (gather-pre-sifr-effects card state side eid target (rest targets)))))})))}}}]}))

(defcard "Silencer"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Killer")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Simulchip"
  {:static-abilities
   [{:type :card-ability-additional-cost
     :req (req (and (same-card? card (:card context))
                    (let [pred (fn [event]
                                 (some #(and (runner? (:card %))
                                             (installed? (:card %))
                                             (program? (:card %)))
                                       event))]
                      (zero? (+ (event-count state nil :runner-trash pred)
                                (event-count state nil :corp-trash pred)
                                (event-count state nil :game-trash pred))))))
     :value [(->c :program 1)]}]
   :abilities [{:async true
                :label "Install a program from the heap"
                :change-in-game-state {:req (req (some #(and (program? %)
                                                             (runner-can-pay-and-install?
                                                               state side
                                                               (assoc eid :source card :source-type :runner-install)
                                                               % {:cost-bonus -3
                                                                  :no-toast true}))
                                                       (:discard runner)))}
                :cost [(->c :trash-can)]
                :effect
                (effect
                  (continue-ability
                    {:show-discard true
                     :waiting-prompt true
                     :choices {:req (req (and (in-discard? target)
                                              (program? target)
                                              (runner-can-pay-and-install? state side (assoc eid :source card) target {:cost-bonus -3})))}
                     :async true
                     :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3
                                                                                                                   :msg-keys {:display-origin true
                                                                                                                              :install-source card
                                                                                                                              :include-cost-from-eid eid}}))}
                    card nil))}]})

(defcard "Skulljack"
  {:on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :static-abilities [{:type :trash-cost
                       :value -1}]})

(defcard "Solidarity Badge"
  {:events [{:event :runner-turn-begins
             :skippable true
             :req (req (pos? (get-counters (get-card state card) :power)))
             :async true
             :interactive (req (pos? (get-counters (get-card state card) :power)))
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req ["Draw 1 card"
                            (when (pos? (count-real-tags state)) "Remove 1 tag")
                            "Done"])
             :effect (req (if (= target "Draw 1 card")
                            (wait-for
                              (add-counter state side card :power -1)
                              (system-msg state side (str "uses " (:title card)
                                                          " to draw 1 card"))
                              (draw state :runner eid 1))
                            (if (= target "Remove 1 tag")
                              (wait-for
                                (add-counter state side card :power -1)
                                (system-msg state side (str "uses " (:title card)
                                                            " to remove 1 tag"))
                                (lose-tags state :runner eid 1))
                              (effect-completed state :runner eid))))}
            {:event :runner-trash
             :async true
             :interactive (req true)
             :req (req (and (some #(corp? (:card %)) targets)
                            (first-event? state side :runner-trash
                                          (fn [targets]
                                            (some #(corp? (:card %)) targets)))))
             :msg "place 1 power counter on itself"
             :effect (effect (add-counter :runner eid card :power 1))}]})

(defcard "Spinal Modem"
  {:static-abilities [(mu+ 1)]
   :recurring 2
   :events [{:event :successful-trace
             :req (req run)
             :msg "suffer 1 core damage"
             :async true
             :effect (effect (damage eid :brain 1 {:card card}))}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Sports Hopper"
  {:static-abilities [(link+ 1)]
   :abilities [(draw-abi 3 nil {:change-in-game-state {:req (req (seq (:deck runner)))}
                                :cost [(->c :trash-can)]})]})

(defcard "Spy Camera"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :change-in-game-state {:req (req (seq (:deck runner)))}
                :async true
                :label "Look at the top X cards of the stack"
                :msg "look at the top X cards of the stack and rearrange them"
                :waiting-prompt true
                :effect (req (let [n (count (filter #(= (:title %) (:title card))
                                                    (all-active-installed state :runner)))
                                   from (take n (:deck runner))]
                               (continue-ability
                                 state side
                                 (when (pos? (count from))
                                   (reorder-choice :runner :corp from '() (count from) from))
                                 card nil)))}
               {:label "Look at the top card of R&D"
                :msg "look at the top card of R&D"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (continue-ability
                                  {:prompt (req (->> corp :deck first :title (str "The top card of R&D is ")))
                                   :choices ["OK"]}
                                  card nil))}]})

(defcard "Supercorridor"
  {:static-abilities [(mu+ 2)
                      (runner-hand-size+ 1)]
   :events [{:event :runner-turn-ends
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional
             {:req (req (= (:credit runner) (:credit corp)))
              :waiting-prompt true
              :prompt "Gain 2 [Credits]?"
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability {:msg "gain 2 [Credits]"
                            :async true
                            :effect (effect (gain-credits eid 2))}
              :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
   :abilities [(set-autoresolve :auto-fire "Supercorridor")]})

(defcard "Swift"
  {:static-abilities [(mu+ 1)]
   :events [{:event :play-event
             :req (req (and (has-subtype? (:card context) "Run")
                            (first-event? state side :play-event #(has-subtype? (:card (first %)) "Run"))))
             :msg "gain a [click]"
             :effect (effect (gain-clicks 1))}]})

(defcard "T400 Memory Diamond"
  {:static-abilities [(mu+ 1)
                      {:type :hand-size
                       :req (req (= :runner side))
                       :value 1}]})

(defcard "The Gauntlet"
  {:static-abilities [(mu+ 2)]
   :events [{:event :breach-server
             :automatic :pre-breach
             :req (req (= :hq target))
             :effect (req (let [evs (run-events state side :subroutines-broken)
                                relevant (filter #(let [context (first %)
                                                        t (get-card state (:ice context))]
                                                    (and (:all-subs-broken context)
                                                         (= :hq (second (get-zone t)))))
                                                 evs)
                                by-cid (map #(get-in (first %) [:card :cid]) relevant)
                                bonus-count (count (distinct by-cid))]
                            (access-bonus state :runner :hq bonus-count)))}]})

(defcard "The Personal Touch"
  {:hosting {:card #(and (has-subtype? % "Icebreaker")
                         (installed? %))}
   :on-install {:effect (effect (update-breaker-strength (:host card)))}
   :static-abilities [{:type :breaker-strength
                       :req (req (same-card? target (:host card)))
                       :value 1}]})

(defcard "The Toolbox"
  {:static-abilities [(mu+ 2)
                      (link+ 2)]
   :recurring 2
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "The Wizard's Chest"
  (letfn [(install-choice [state side eid card rev-str first-card second-card]
            (continue-ability
              state side
              {:prompt "Choose one"
               :choices [(str "Install " (:title first-card))
                         (when second-card (str "Install " (:title second-card)))
                         "No install"]
               :msg (msg "reveal " rev-str " from the top of the stack")
               :async true
               :effect (req (if-not (= target "No install")
                              (wait-for (runner-install
                                          state side
                                          (make-eid state {:source card :source-type :runner-install})
                                          (if (= target (str "Install " (:title first-card)))
                                            first-card second-card)
                                          {:ignore-all-cost true
                                           :msg-keys {:display-origin true
                                                      :install-source card}})
                                        (shuffle! state side :deck)
                                        (system-msg state side "shuffles the Stack")
                                        (effect-completed state side eid))
                              (do (shuffle! state side :deck)
                                  (system-msg state side "shuffles the Stack")
                                  (effect-completed state side eid))))}
              card nil))
          (wiz-search-fn [state side eid card remainder type rev-str first-card]
            (if (seq remainder)
              (let [revealed-card (first remainder)
                    rest-of-deck (rest remainder)
                    rev-str (if (= "" rev-str)
                              (:title revealed-card)
                              (str rev-str ", " (:title revealed-card)))]
                (if (is-type? revealed-card type)
                  (if-not first-card
                    (wiz-search-fn state side eid card rest-of-deck type rev-str revealed-card)
                    (install-choice state side eid card rev-str first-card revealed-card))
                  (wiz-search-fn state side eid card rest-of-deck type rev-str first-card)))
              (if-not first-card
                (continue-ability
                  state side
                  {:msg (msg "reveal " rev-str " from the top of the stack")
                   :effect (effect (shuffle! :deck)
                                   (system-msg "shuffles the Stack"))}
                  card nil)
                (install-choice state side eid card rev-str first-card nil))))]
    {:abilities [{:cost [(->c :trash-can)]
                  :change-in-game-state {:req (req (seq (:deck runner)))}
                  :label "Set aside cards from the top of the stack"
                  :prompt "Choose a card type"
                  :waiting-prompt true
                  :choices (req (cancellable ["Hardware" "Program" "Resource"]))
                  :req (req (and (some #{:hq} (:successful-run runner-reg))
                                 (some #{:rd} (:successful-run runner-reg))
                                 (some #{:archives} (:successful-run runner-reg))))
                  :async true
                  :effect (effect (wiz-search-fn eid card (:deck runner) target "" nil))}]}))

(defcard "Time Bomb"
  {:data {:counter {:power 1}}
   :req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
   :events [{:event :runner-turn-begins
             :automatic :force-discard
             :async true
             :effect (req (if (<= 3 (get-counters (get-card state card) :power))
                            (wait-for (trash state side card {:cause-card card})
                                      (continue-ability state side
                                                        (sabotage-ability 3)
                                                        card nil))
                            (do (system-msg state side (str "uses " (:title card) " to place 1 power counter on itself"))
                                (add-counter state side eid card :power 1))))}]})

(defcard "Titanium Ribs"
  {:on-install {:async true
                :msg "suffer 2 meat damage"
                :effect (effect (enable-runner-damage-choice)
                                (damage eid :meat 2 {:unboostable true :card card}))}
   :leave-play (req (swap! state update :damage dissoc :damage-choose-runner))
   :events [{:event :pre-resolve-damage
             :async true
             :req (req (and (pos? (last targets))
                            (runner-can-choose-damage? state)
                            (not (get-in @state [:damage :damage-replace]))))
             :effect (req (let [dtype target
                                dmg (last targets)
                                hand (:hand runner)]
                            (continue-ability
                              state :runner
                              (if (< (count hand) dmg)
                                {:effect (effect (chosen-damage :runner hand))}
                                {:waiting-prompt true
                                 :prompt (msg "Choose " (quantify dmg "card") " to trash for the " (name dtype) " damage")
                                 :choices {:max dmg
                                           :all true
                                           :card #(and (in-hand? %)
                                                       (runner? %))}
                                 :msg (msg "trash " (enumerate-cards targets :sorted))
                                 :effect (effect (chosen-damage :runner targets))})
                              card nil)))}]})

(defcard "Top Hat"
  {:events [(successful-run-replace-breach
              {:target-server :rd
               :ability {:req (req (and (not= (:max-access run) 0)
                                        (pos? (count (:deck corp)))))
                         :prompt "Which card from the top of R&D would you like to access? (Card 1 is on top)"
                         :choices (req (map str (take (count (:deck corp)) (range 1 6))))
                         :msg (msg "only access the card at position " target " of R&D")
                         :async true
                         :effect (req (if (get-only-card-to-access state)
                                        (effect-completed state nil eid)
                                        (access-card state side eid (nth (:deck corp) (dec (str->int target))) "an unseen card")))}})]})

(defcard "Turntable"
  {:static-abilities [(mu+ 1)]
   :events [{:event :agenda-stolen
             :interactive (req true)
             :req (req (seq (:scored corp)))
             :async true
             :effect (effect
                       (continue-ability
                         (let [stolen (:card context)]
                           {:optional
                            {:prompt (msg "Swap " (:title stolen) " for an agenda in the Corp's score area?")
                             :yes-ability
                             {:prompt (str "Choose a scored Corp agenda to swap with " (:title stolen))
                              :choices {:card #(in-corp-scored? state side %)}
                              :msg (msg "swap " (:title stolen) " for " (:title target))
                              :effect (effect (swap-agendas target stolen))}}})
                         card targets))}]})

(defcard "Ubax"
  (let [ability {:req (req (:runner-phase-12 @state))
                 :automatic :draw-cards
                 :msg "draw 1 card"
                 :label "Draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (draw eid 1))}]
    {:static-abilities [(mu+ 1)]
     :flags {:runner-turn-draw true
             :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                       (cons (get-in @state [:runner :identity])
                                                             (all-active-installed state :runner))))))}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Unregistered S&W '35"
  {:abilities
   [{:action true
     :cost [(->c :click 2)]
     :req (req (and (some #{:hq} (:successful-run runner-reg))
                    (seq (filter
                           #(and (rezzed? %)
                                 (installed? %)
                                 (has-any-subtype? % ["Bioroid" "Clone" "Executive" "Sysop"]))
                           (all-active-installed state :corp)))))
     :label "trash a Bioroid, Clone, Executive or Sysop"
     :prompt "Choose a Bioroid, Clone, Executive, or Sysop to trash"
     :choices {:card #(and (rezzed? %)
                           (installed? %)
                           (has-any-subtype? % ["Bioroid" "Clone" "Executive" "Sysop"]))}
     :async true
     :msg (msg "trash " (:title target))
     :effect (effect (trash eid target {:cause-card card}))}]})

(defcard "Vigil"
  (let [ability {:req (req (and (:runner-phase-12 @state)
                                (= (count (:hand corp)) (hand-size state :corp))))
                 :automatic :draw-cards
                 :msg "draw 1 card"
                 :label "Draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (draw eid 1))}]
    {:static-abilities [(mu+ 1)]
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Virtuoso"
  {:static-abilities [(mu+ 1)]
   :events [mark-changed-event
            (assoc identify-mark-ability :event :runner-turn-begins)
            {:event :successful-run
             :req (req (and (:marked-server target)
                            (first-event? state side :successful-run #(:marked-server (first %)))))
             :async true
             :effect (req (if (= :hq (first (:server target)))
                            (do
                              (system-msg state side (str "uses " (:title card) " to access 1 additional card from HQ this run"))
                              (register-events
                                  state side
                                  card [(breach-access-bonus :hq 1 {:duration :end-of-run})])
                                (effect-completed state side eid))
                            (do (system-msg state side (str "will use " (:title card) " to breach HQ when this run ends"))
                                (register-events
                                  state side
                                  card
                                  [{:event :run-ends
                                    :duration :end-of-run
                                    :async true
                                    :interactive (req true)
                                    :msg (msg "breach HQ")
                                    :effect (req (breach-server state :runner eid [:hq] nil))}])
                                (effect-completed state side eid))))}]})

(defcard "WAKE Implant v2A-JRJ"
  {:on-install {:async true
                :msg "suffer 1 meat damage"
                :effect (effect (damage eid :meat 1 {:unboostable true :card card}))}
   :events [{:event :successful-run
             :async true
             :req (req (= :hq (target-server context)))
             :msg "place 1 power counter on itself"
             :effect (req (add-counter state :runner eid card :power 1 {:placed true}))}
            {:event :breach-server
             :automatic :pre-breach
             :async true
             :req (req (and (= :rd target)
                            (pos? (get-counters card :power))))
             :effect (req (continue-ability
                            state side
                            {:prompt "How many additional R&D accesses do you want to make?"
                             :choices {:number (req (min 3 (get-counters card :power)))
                                       :default (req (min 3 (get-counters card :power)))}
                             :msg (msg "access " (quantify target "additional card") " from R&D")
                             :waiting-prompt true
                             :async true
                             :effect (req (access-bonus state :runner :rd (max 0 target))
                                          (add-counter state :runner eid card :power (- target) {:placed true}))}
                            card nil))}]})

(defcard "Window"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :change-in-game-state {:req (req (seq (:deck runner)))}
                :keep-menu-open :while-clicks-left
                :msg "draw 1 card from the bottom of the stack"
                :effect (effect (move (last (:deck runner)) :hand))}]})

(defcard "Zamba"
  {:special {:auto-gain-credits :always}
   :implementation "Credit gain is automatic"
   :static-abilities [(mu+ 2)]
   :abilities [(set-autoresolve :auto-gain-credits "Zamba gaining credits on expose")]
   :events [{:event :expose
             :interactive (get-autoresolve :auto-gain-credits (complement never?))
             :silent (get-autoresolve :auto-gain-credits never?)
             :async true
             :optional
             {:waiting-prompt true
              :prompt (msg "Gain " (count (:cards context)) " [Credits]?")
              :autoresolve (get-autoresolve :auto-gain-credits)
              :yes-ability {:msg (msg "gain " (count (:cards context)) " [Credits]")
                            :async true
                            :effect (effect (gain-credits eid (count (:cards context))))}}}]})

(defcard "Zenit Chip JZ-2MJ"
  {:on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :events [{:event :successful-run
             :automatic :draw-cards
             :async true
             :req (req (and (is-central? (:server context))
                            (first-event? state side :successful-run
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (is-central? (:server context)))))))
             :msg "draw 1 card"
             :effect (req (draw state :runner eid 1))}]})

(defcard "Zer0"
  {:abilities [{:action true
                :cost [(->c :click 1) (->c :net 1)]
                :once :per-turn
                :msg "gain 1 [Credits] and draw 2 cards"
                :async true
                :effect (req (play-sfx state side "professional-contacts")
                             (wait-for (gain-credits state side 1 {:suppress-checkpoint true})
                                       (draw state side eid 2)))}]})
