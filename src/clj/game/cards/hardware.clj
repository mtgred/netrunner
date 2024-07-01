(ns game.cards.hardware
  (:require
   [clojure.set :as set]
   [game.core.access :refer [access-bonus access-card breach-server
                             get-only-card-to-access]]
   [game.core.actions :refer [play-ability]]
   [game.core.board :refer [all-active all-active-installed all-installed]]
   [game.core.card :refer [corp? event? facedown? get-card get-counters get-title
                           get-zone hardware? has-subtype? ice? in-deck? in-discard?
                           in-hand? in-scored? installed? is-type? program? resource? rezzed?
                           runner? virus-program? faceup?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [install-cost rez-additional-cost-bonus rez-cost trash-cost]]
   [game.core.damage :refer [chosen-damage damage damage-prevent
                             enable-runner-damage-choice runner-can-choose-damage?]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out
                                  reorder-choice trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect
                              unregister-effects-for-card unregister-lingering-effects]]
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.engine :refer [can-trigger? register-events
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
   [game.core.ice :refer [all-subs-broken? any-subs-broken? break-sub pump
                          reset-all-ice update-all-ice update-all-icebreakers
                          update-breaker-strength]]
   [game.core.installing :refer [runner-can-install? runner-can-pay-and-install? runner-install]]
   [game.core.link :refer [get-link link+]]
   [game.core.memory :refer [caissa-mu+ expected-mu mu+ update-mu virus-mu+]]
   [game.core.moving :refer [as-agenda mill move swap-agendas trash trash-cards]]
   [game.core.optional :refer [get-autoresolve never? set-autoresolve]]
   [game.core.payment :refer [build-cost-string can-pay? cost-value ->c]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [bypass-ice end-run end-run-prevent
                           get-current-encounter jack-out make-run
                           successful-run-replace-breach total-cards-accessed]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [target-server is-central?]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags tag-prevent]]
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
             {:player :runner
              :waiting-prompt true
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
   :interactions {:prevent [{:type #{:net}
                             :req (req run)}]}
   :events [(trash-on-empty :power)
            {:event :prevent-encounter-ability
             :interactive (req true)
             :req (req (not (get-in @state [:run :prevent-encounter-ability])))
             :async true
             :effect (req
                       (if (get-in @state [:run :prevent-encounter-ability])
                         (effect-completed state side eid)
                         (continue-ability
                           state side
                           {:optional {:prompt (msg "Prevent a \"when encountered\" ability on " (:title current-ice) (when (:ability-name target)
                                                                                                                        (str " (" (:ability-name target) ")")))
                                       :yes-ability {:cost [(->c :power 1)]
                                                     :msg (msg "prevent the encounter ability on " (:title current-ice) (when (:ability-name target)
                                                                                                                          (str " (" (:ability-name target) ")")))
                                                     :effect (req (swap! state assoc-in [:run :prevent-encounter-ability] true))}}}
                           card targets)))}]
     :abilities [{:cost [(->c :power 1)]
                  :req (req run)
                  :msg "prevent 1 net damage"
                  :effect (effect (damage-prevent :net 1))}]})

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
                                    :req (req (first-run-event? state side :encounter-ice))
                                    :unregister-once-resolved true
                                    :duration :end-of-run
                                    :optional
                                    {:prompt "Spend [Click][Click] to bypass encountered ice?"
                                     :yes-ability {:cost [(->c :click 2 {:allowed-during-run true})]
                                                   :req (req (>= (:click runner) 2))
                                                   :msg (msg "bypass " (card-str state (:ice context)))
                                                   :effect (req (bypass-ice state))}}}])
                              (wait-for
                                (make-run state :runner (make-eid state eid) :hq card)
                                (effect-completed state side eid)))}]
    {:flags {:runner-phase-12 (req true)}
     :events [{:event :runner-turn-begins
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
                                       (pos? (get-counters card :power))))
                        :yes-ability {:cost [(->c :power 1)]
                                      :msg "draw 2 cards"
                                      :async true
                                      :effect (req (draw state :runner eid 2))}}}
            {:event :runner-turn-ends
             :req (req tagged)
             :msg "place 1 power counter on itself"
             :effect (req (add-counter state side (get-card state card) :power 1))}]})


(defcard "Aniccam"
  (let [ability {:async true
                 :once-per-instance true
                 :req (req (and (some #(event? (:card %)) targets)
                                (letfn [(event-targets? [targets]
                                          (some #(event? (:card %)) targets))]
                                  (first-trash? state event-targets?))))
                 :msg "draw 1 card"
                 :effect (effect (draw :runner eid 1))}]
    {:static-abilities [(mu+ 1)]
     :events [(assoc ability :event :corp-trash)
              (assoc ability :event :runner-trash)
              (assoc ability :event :game-trash)]}))

(defcard "Archives Interface"
  {:events
   [{:event :breach-server
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
   :events [{:event :server-created
             :msg "draw 1 card"
             :async true
             :effect (effect (draw :runner eid 1))}]})

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
  {:static-abilities [(mu+ 2)]
   :events [{:event :expose
             :msg (msg "attempt to force the rez of " (:title target))
             :async true
             :effect (req (let [c target
                                cname (:title c)
                                cost (rez-cost state side target)
                                additional-costs (rez-additional-cost-bonus state side target)]
                            (if (seq additional-costs)
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
                              (rez state :corp eid target))))}]})

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
     :abilities [{:cost [(->c :click 2)]
                  :label "Install a hosted program"
                  :prompt "Choose a program to install"
                  :choices (req (cancellable (filter #(can-pay? state side (assoc eid :source card :source-type :runner-install)
                                                                % nil [(->c :credit (install-cost state side %))])
                                                     (:hosted card))))
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
     :abilities [{:cost [(->c :click 2)]
                  :label "Install a hosted program"
                  :prompt "Choose a program to install"
                  :choices (req (:hosted card))
                  :msg (msg "install " (:title target))
                  :async true
                  :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:ignore-all-cost true}))}]}))

(defcard "Bookmark"
  {:abilities [{:label "Host up to 3 cards from the grip facedown"
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :msg "host up to 3 cards from the grip facedown"
                :choices {:max 3
                          :card #(and (runner? %)
                                      (in-hand? %))}
                :effect (req (doseq [c targets]
                               (host state side (get-card state card) c {:facedown true})))}
               {:label "Add all hosted cards to the grip"
                :cost [(->c :click 1)]
                :msg "add all hosted cards to the grip"
                :effect (req (doseq [c (:hosted card)]
                               (move state side c :hand)))}
               {:label "Add all hosted cards to the grip"
                :trash-icon true
                :effect (req (doseq [c (:hosted card)]
                               (move state side c :hand))
                             (continue-ability
                               state side
                               {:cost [(->c :trash-can)]
                                :msg "add all hosted cards to the grip"}
                               (get-card state card) nil))}]})

(defcard "Boomerang"
  {:on-install {:prompt "Choose an installed piece of ice"
                :msg (msg "target " (card-str state target))
                :choices {:card #(and (installed? %)
                                      (ice? %))}
                :effect (effect (add-icon card target "B" (faction-label card))
                                (update! (assoc-in (get-card state card) [:special :boomerang-target] target)))}
   :leave-play (effect (remove-icon card))
   :abilities [(assoc
                 (break-sub
                   [(->c :trash-can)] 2 "All"
                   {:req (req (if-let [boomerang-target (get-in card [:special :boomerang-target])]
                                (some #(same-card? boomerang-target (:ice %)) (:encounters @state))
                                true))}) ; When eg. flipped by Assimilator
                 :effect
                 (req (wait-for
                        (trash state side (make-eid state eid) card
                               {:cause :ability-cost
                                :cause-card card
                                :unpreventable true})
                        (continue-ability
                          state :runner
                          (when-let [[boomerang] async-result]
                            (break-sub
                              nil 2 "All"
                              {:additional-ability
                               {:effect
                                (effect
                                  (register-events
                                    boomerang
                                    [{:event :run-ends
                                      :duration :end-of-run
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
                                                        (shuffle! :deck))}}}]))}}))
                          card nil))))]})

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
  (let [grip-or-stack-trash?
        (fn [targets]
          (some #(and (runner? (:card %))
                      (or (in-hand? (:card %))
                          (in-deck? (:card %))))
                targets))
        triggered-ability
        {:once-per-instance true
         :req (req (and (grip-or-stack-trash? targets)
                        (first-trash? state grip-or-stack-trash?)))
         :prompt "Choose 1 trashed card to add to the bottom of the stack"
         :choices (req (conj (sort (map :title (map :card targets))) "No action"))
         :async true
         :effect (req (if (= "No action" target)
                        (effect-completed state side eid)
                        (do (system-msg state side
                                        (str "uses " (:title card) " to add " target
                                             " to the bottom of the stack"))
                            (move state side (find-card target (:discard (:runner @state))) :deck)
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
  {:abilities [{:req (req (pos? (count (:hand runner))))
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
                                                                      (enumerate-str (map :title trashed-cards))
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
               :msg (msg "derez " (card-str state target))
               :effect (effect (derez target)
                               (effect-completed eid))}}}]})

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
                          :waiting-prompt true
                          :effect (req (continue-ability
                                         state side
                                         (let [from (take 4 (:deck corp))]
                                           (when (pos? (count from))
                                             (reorder-choice :corp :corp from '() (count from) from)))
                                         card nil))}})
        access-ability {:cost [(->c :click 1) (->c :power 1)]
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
                     :interactive (req true))]
     :abilities [ability]}))

(defcard "Clone Chip"
  {:abilities [{:prompt "Choose a program to install"
                :label "Install program from the heap"
                :show-discard true
                :req (req (some #(and (program? %)
                                      (runner-can-pay-and-install?
                                        state side
                                        (assoc eid :source card :source-type :runner-install) % nil))
                                (:discard runner)))
                :choices {:req (req (and (program? target)
                                         (in-discard? target)
                                         (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                   [(->c :credit (install-cost state side target))])))}
                :cost [(->c :trash-can)]
                :msg (msg "install " (:title target))
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]})

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
             :req (req (and (all-subs-broken? target)
                            (first-event? state side :subroutines-broken #(all-subs-broken? (first %)))))
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
   :events [{:event :run
             :once :per-turn
             :req (req (and (<= 2 (:position target))
                            (first-event? state side :run #(<= 2 (:position (first %))))))
             :msg "draw 2 cards"
             :async true
             :effect (effect (draw eid 2))}]})

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
             :silent (req true)
             :async true
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits eid 1))}]})

(defcard "Devil Charm"
  {:events [{:event :encounter-ice
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
              :msg "access 1 additional cards from HQ"})]})

(defcard "Doppelgänger"
  {:static-abilities [(mu+ 1)]
   :events [{:event :runner-install
             :req (req (same-card? card (:card context)))
             :silent (req true)
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :runner-turn-begins
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :run-ends
             :interactive (req true)
             :optional
             {:req (req (and (:successful target)
                             (:dopp-active (get-card state card))))
              :player :runner
              :prompt "Make another run?"
              :yes-ability {:prompt "Choose a server"
                            :async true
                            :choices (req runnable-servers)
                            :msg (msg "make a run on " target)
                            :makes-run true
                            :effect (effect (update! (dissoc card :dopp-active))
                                            (unregister-lingering-effects :end-of-run)
                                            (unregister-floating-events :end-of-run)
                                            (update-all-icebreakers)
                                            (update-all-ice)
                                            (reset-all-ice)
                                            (clear-wait-prompt :corp)
                                            (make-run eid target (get-card state card)))}}}]})

(defcard "Dorm Computer"
  {:flags {:forced-to-avoid-tag true}
   :data {:counter {:power 4}}
   :abilities [{:cost [(->c :click 1) (->c :power 1)]
                :req (req (not run))
                :prompt "Choose a server"
                :choices (req runnable-servers)
                :msg "make a run and avoid all tags for the remainder of the run"
                :makes-run true
                :async true
                :effect (effect (register-events
                                  card
                                  [{:event :pre-tag
                                    :duration :end-of-run
                                    :async true
                                    :msg "avoid all tags during the run"
                                    :effect (effect (tag-prevent :runner eid Integer/MAX_VALUE))}])
                                (make-run eid target card))}]})

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
  {:abilities [(break-sub 1 1 "All" {:req (req true)})]})

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
  {:data {:counter {:power 3}}
   :static-abilities [(mu+ 2)]
   :events [{:event :successful-run
             :req (req (first-event? state :runner :successful-run))
             :msg "place 1 power counter on itself"
             :async true
             :effect (effect (add-counter eid card :power 1 nil))}]
   :abilities [(break-sub [(->c :power 2)] 2 "All")]})

(defcard "Feedback Filter"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [(->c :credit 3)]
                :msg "prevent 1 net damage"
                :effect (effect (damage-prevent :net 1))}
               {:label "Prevent up to 2 core damage"
                :msg "prevent up to 2 core damage"
                :cost [(->c :trash-can)]
                :effect (effect (damage-prevent :brain 2))}]})

(defcard "Flame-out"
  (let [register-flame-effect
        (fn [state card]
          (update! state :runner (assoc-in (get-card state card) [:special :flame-out-trigger] true)))
        maybe-turn-end {:async true
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
                  :effect (req (add-counter state side card :credit -1)
                               (system-msg state :runner "takes 1 hosted [Credits] from Flame-out")
                               (register-flame-effect state card)
                               (gain-credits state :runner eid 1))}
                 {:label "Take all hosted [Credits]"
                  :req (req (and (not-empty (:hosted card))
                                 (pos? (get-counters card :credit))))
                  :async true
                  :effect (req (let [credits (get-counters card :credit)]
                                 (update! state :runner (dissoc-in card [:counter :credit]))
                                 (system-msg state :runner (str "takes " credits " hosted [Credits] from Flame-out"))
                                 (register-flame-effect state card)
                                 (gain-credits state :runner eid credits)))}]
     :events [(assoc maybe-turn-end :event :runner-turn-ends)
              (assoc maybe-turn-end :event :corp-turn-ends)]
     :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                                 (same-card? card (:host target))
                                                 (pos? (get-counters card :credit))))
                                  :custom-amount 1
                                  :custom (req (add-counter state side card :credit -1)
                                               (register-flame-effect state card)
                                               (effect-completed state side (make-result eid 1)))
                                  :type :custom}}}))

(defcard "Flip Switch"
  {:events [{:event :initialize-trace
             :optional
             {:req (req (= :runner (:active-player @state)))
              :waiting-prompt true
              :prompt "Trash Flip Switch to reduce the base trace strength to 0?"
              :player :runner
              :yes-ability {:msg "reduce the base trace strength to 0"
                            :cost [(->c :trash-can)]
                            :effect (req (swap! state assoc-in [:trace :force-base] 0))}}}]
   :abilities [{:label "Jack out"
                :req (req (and (or run
                                   (get-current-encounter state))
                               (= :runner (:active-player @state))))
                :msg "jack out"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (jack-out eid))}
               {:label "Remove 1 tag"
                :req (req (and (pos? (count-real-tags state))
                               (= :runner (:active-player @state))))
                :msg "remove 1 tag"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (lose-tags eid 1))}]})

(defcard "Forger"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :static-abilities [(link+ 1)]
   :abilities [{:msg "avoid 1 tag"
                :label "Avoid 1 tag"
                :async true
                :cost [(->c :trash-can)]
                :effect (effect (tag-prevent :runner eid 1))}
               {:msg "remove 1 tag"
                :label "Remove 1 tag"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (lose-tags eid 1))}]})

(defcard "Friday Chip"
  (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                 :req (req (and (pos? (get-counters card :virus))
                                (pos? (count-virus-programs state))))
                 :choices {:card virus-program?}
                 :effect (req (add-counter state :runner card :virus -1)
                              (add-counter state :runner target :virus 1))}]
    {:abilities [(set-autoresolve :auto-fire "Friday Chip placing virus counters on itself")]
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-trash
               :once-per-instance true
               :async true
               :req (req (some #(corp? (:card %)) targets))
               :effect (req (let [amt-trashed (count (filter #(corp? (:card %)) targets))
                                  sing-ab {:optional
                                           {:prompt (msg "Place a virus counter on " (:title card) "?")
                                            :autoresolve (get-autoresolve :auto-fire)
                                            :yes-ability {:effect (effect (system-msg
                                                                            :runner
                                                                            (str "uses " (:title card) " to place 1 virus counter on itself"))
                                                                          (add-counter :runner card :virus 1))}}}
                                  mult-ab {:prompt (msg "Place virus counters on " (:title card) "?")
                                           :choices {:number (req amt-trashed)
                                                     :default (req amt-trashed)}
                                           :effect (effect (system-msg :runner
                                                                       (str "uses " (:title card) " to place "
                                                                            (quantify target "virus counter")
                                                                            " on itself"))
                                                           (add-counter :runner card :virus target))}
                                  ab (if (> amt-trashed 1) mult-ab sing-ab)]
                              (continue-ability state side ab card targets)))}]}))

(defcard "Gachapon"
  (letfn [(shuffle-end [remove-from-game shuffle-back]
            {:msg (msg "shuffle " (enumerate-str (map :title shuffle-back)) " into the stack"
                       " and remove " (enumerate-str (map :title remove-from-game)) " from the game")
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
                                                  (enumerate-str (map :title set-aside-cards))
                                                  "nothing")
                                   "[br]Shuffling: " (if (not-empty to-shuffle)
                                                       (enumerate-str (map :title to-shuffle))
                                                       "nothing"))
                              (str "Choose " (- 3 (count to-shuffle)) " more cards to shuffle back."
                                   (when (not-empty to-shuffle)
                                     (str "[br]Currently shuffling back: " (enumerate-str (map :title to-shuffle)))))))
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
                  :cost [(->c :trash-can)]
                  :async true
                  :waiting-prompt true
                  :effect (req (set-aside state side eid (take 6 (:deck runner)))
                               (let [set-aside-cards (sort-by :title (get-set-aside state side eid))]
                                 (system-msg state side (str "uses " (get-title card)
                                                             " to set aside "
                                                             (enumerate-str (map get-title set-aside-cards))
                                                             " from the top of the stack"))
                                 (wait-for (resolve-ability state side
                                                            {:async true
                                                             :prompt (str "The set aside cards are: "
                                                                          (enumerate-str (map get-title set-aside-cards)))
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
                                                                              % {:cost-bonus -2}))
                                                                      set-aside-cards)
                                                              ["Done"]))
                                              :effect (req (if (= "Done" target)
                                                             (continue-ability state side (shuffle-next set-aside-cards nil nil) card nil)
                                                             (let [set-aside-cards (remove-once #(= % target) set-aside-cards)
                                                                   new-eid (assoc eid :source card :source-type :runner-install)]
                                                               (wait-for (runner-install state side new-eid target {:cost-bonus -2})
                                                                         (continue-ability state side (shuffle-next set-aside-cards nil nil) card nil)))))}
                                             card nil))))}]}))

(defcard "Gebrselassie"
  {:abilities [{:msg "host itself on an installed non-AI icebreaker"
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
                :msg "expose the approached ice"
                :async true
                :effect (req (wait-for (expose state side (make-eid state eid) current-ice)
                                       (continue-ability state side (offer-jack-out) card nil)))}]})

(defcard "Grimoire"
  {:static-abilities [(mu+ 2)]
   :events [{:event :runner-install
             :interactive (req true)
             :req (req (has-subtype? (:card context) "Virus"))
             :effect (effect (add-counter (:card context) :virus 1))}]})

(defcard "Heartbeat"
  {:static-abilities [(mu+ 1)]
   :interactions {:prevent [{:type #{:net :brain :meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent 1 damage"
                :msg "prevent 1 damage"
                :cost [(->c :trash-installed 1)]
                :effect (effect (damage-prevent :brain 1)
                                (damage-prevent :meat 1)
                                (damage-prevent :net 1))}]})

(defcard "Hermes"
  (let [ab {:interactive (req true)
            :prompt "Choose an unrezzed card"
            :waiting-prompt true
            :choices {:card #(and (not (faceup? %))
                                  (installed? %)
                                  (corp? %))}
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
  (letfn [(build-hippo-pred [outermost-ices]
            (fn [events]
              (->> outermost-ices
                   (map #(and (same-card? % (first events))
                                            (all-subs-broken? (first events))))
                   (filter true?)
                   seq)))]
    {:events [{:event :subroutines-broken
               :optional
               {:req (req (let [servers (->> (:corp @state) :servers seq flatten)
                                outermost-ices (filter #(some? %) (map #(last (:ices %)) servers))
                                pred (build-hippo-pred outermost-ices)]
                            (and (same-card? (last run-ices) target)
                                 (all-subs-broken? target)
                                 (first-event? state side :subroutines-broken pred))))
                :prompt (msg "Remove this hardware from the game to trash " (:title target) "?")
                :yes-ability
                {:async true
                 :cost [(->c :remove-from-game)]
                 :msg (msg "trash " (card-str state target))
                 :effect (effect (trash eid target {:cause-card card}))}}}]}))

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
                                            :msg (msg "install " (get-title card) " from the heap")
                                            :async true
                                            :effect (req (let [target-card (first (filter #(= (:printed-title %) (:printed-title card)) (:discard runner)))]
                                                           (runner-install state side (assoc eid :source card :source-type :runner-install) target-card nil)))}}}
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
  {:static-abilities [(mu+ 2)]
   :events [{:event :spent-credits-from-card
             :req (req (and (not (facedown? target))
                            (has-subtype? target "Companion")
                            (= 1 (+ (event-count state :runner :spent-credits-from-card
                                                 #(and (not (facedown? (first %)))
                                                       (has-subtype? (first %) "Companion")))
                                    (event-count state :runner :runner-install
                                                 #(and (not (facedown? (:card (first %))))
                                                       (has-subtype? (:card (first %)) "Companion")))))))
             :msg "gain 1 [Credit]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}
            {:event :runner-install
             :req (req (and (not (:facedown context))
                            (has-subtype? (:card context) "Companion")
                            (= 1 (+ (event-count state :runner :spent-credits-from-card
                                                 #(and (not (facedown? (first %)))
                                                       (has-subtype? (first %) "Companion")))
                                    (event-count state :runner :runner-install
                                                 #(and (not (:facedown (first %)))
                                                       (has-subtype? (:card (first %)) "Companion")))))))
             :msg "gain 1 [Credit]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}]})

(defcard "Knobkierie"
  {:static-abilities [(virus-mu+ 3)]
   :events [{:event :successful-run
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
                                      :effect (effect (add-counter target :virus 1))}}}]
   :abilities [(set-autoresolve :auto-fire "Knobkierie")]})

(defcard "Lemuria Codecracker"
  {:abilities [{:cost [(->c :click 1) (->c :credit 1)]
                :req (req (some #{:hq} (:successful-run runner-reg)))
                :choices {:card installed?}
                :effect (effect (expose eid target))
                :msg "expose 1 card"}]})

(defcard "LilyPAD"
  {:events [{:event :runner-install
             :optional {:prompt "Draw 1 card?"
                        :waiting-prompt true
                        :req (req (and
                                    (program? (:card target))
                                    (first-event? state :runner :runner-install #(program? (:card (first %))))))
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability {:msg "draw 1 card"
                                      :async true
                                      :effect (req (draw state :runner eid 1))}
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
             :player :runner
             :prompt "Choose a card"
             :msg "add 1 card from the stack to the grip"
             :choices (req (cancellable (:deck runner)))
             :effect (effect (trigger-event :searched-stack)
                             (shuffle! :deck)
                             (move target :hand))}]})

(defcard "Lucky Charm"
  {:interactions {:prevent [{:type #{:end-run}
                             :req (req (and (some #{:hq} (:successful-run runner-reg))
                                            (corp? (:card-cause target))))}]}
   :abilities [{:msg "prevent the run from ending"
                :req (req (some #{:hq} (:successful-run runner-reg)))
                :cost [(->c :remove-from-game)]
                :effect (effect (end-run-prevent))}]})

(defcard "Mâché"
  (letfn [(pred [{:keys [card accessed]}]
            (and accessed (corp? card)))]
    {:abilities [{:label "Draw 1 card"
                  :msg "draw 1 card"
                  :cost [(->c :power 3)]
                  :keep-menu-open :while-3-power-tokens-left
                  :async true
                  :effect (effect (draw :runner eid 1))}]
     :events [{:event :runner-trash
               :once-per-instance true
               :req (req (and (some pred targets)
                              (first-event? state side :runner-trash (fn [targets] (some pred targets)))))
               :effect (req (let [target (some #(when (pred %) (:card %)) targets)
                                  cost (trash-cost state side target)]
                              (when cost
                                (system-msg state side (str "uses " (:title card) " to place "
                                                            (quantify cost "power counter")
                                                            " on itself"))
                                (add-counter state side card :power cost))))}]}))

(defcard "Marrow"
  {:static-abilities [(mu+ 1)
                      (runner-hand-size+ 3)]
   :on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :events [{:event :agenda-scored
             :async true
             :interactive (req true)
             :effect (effect (continue-ability (sabotage-ability 1) card nil))}]})

(defcard "Masterwork (v37)"
  {:static-abilities [(mu+ 1)]
   :events [{:event :run
             :interactive (req true)
             :optional
             {:req (req (some #(and (hardware? %)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) card %
                                              [(->c :credit (install-cost state side % {:cost-bonus 1}))]))
                              (:hand runner)))
              :prompt "Pay 1 [Credit] to install a piece of hardware?"
              :yes-ability {:async true
                            :prompt "Choose a piece of hardware"
                            :choices
                            {:req (req (and (in-hand? target)
                                            (hardware? target)
                                            (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                      [(->c :credit (install-cost state side target {:cost-bonus 1}))])))}
                            :msg (msg "install " (:title target) " from the grip, paying 1 [Credit] more")
                            :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus 1}))}}}
            {:event :runner-install
             :async true
             :interactive (req true)
             :req (req (and (hardware? (:card context))
                            (first-event? state side :runner-install #(hardware? (:card (first %))))))
             :msg "draw 1 card"
             :effect (effect (draw eid 1))}]})

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
             :msg "force the Corp to trash a random card from HQ"
             :effect (req (let [card-to-trash (first (shuffle (:hand corp)))
                                card-seen? (same-card? target card-to-trash)
                                card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                card-to-trash)]
                            (trash state :corp eid card-to-trash {:cause-card card :cause :forced-to-trash})))}]})

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
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:async true
                :cost [(->c :click 1) (->c :power 3)]
                :msg "breach R&D"
                :effect (req (breach-server state side eid [:rd] {:no-root true}))}]})

(defcard "Mirror"
  {:static-abilities [(mu+ 2)]
   :events [{:event :successful-run
             :async true
             :req (req (= :rd (target-server context)))
             :effect (effect (continue-ability
                               {:prompt "Choose a card and replace 1 spent [Recurring Credits] on it"
                                :choices {:card #(< (get-counters % :recurring) (:recurring (card-def %) 0))}
                                :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                :effect (effect (add-counter target :recurring 1))}
                               card nil))}]})

(defcard "Monolith"
  (let [mhelper
        (fn mh [n]
          {:prompt "Choose a program to install"
           :choices {:req (req (and (program? target)
                                    (in-hand? target)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                              [(->c :credit (install-cost state side target {:cost-bonus -4}))])))}
           :async true
           :effect (req (wait-for (runner-install state side target {:cost-bonus -4})
                                  (continue-ability state side (when (< n 3) (mh (inc n))) card nil)))})]
    {:interactions {:prevent [{:type #{:net :brain}
                               :req (req true)}]}
     :static-abilities [(mu+ 3)]
     :on-install {:async true
                  :effect (effect (continue-ability (mhelper 1) card nil))}
     :abilities [{:msg "prevent 1 brain or net damage"
                  :cost [(->c :trash-program-from-hand 1)]
                  :effect (effect (damage-prevent :brain 1)
                                  (damage-prevent :net 1))}]}))

(defcard "Mu Safecracker"
  {:implementation "Stealth credit restriction not enforced"
   :events [{:event :successful-run
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
  {:events [{:event :pre-damage
             :once :per-turn
             :once-key :muresh-bodysuit
             :req (req (= (:type context) :meat))
             :msg "prevent the first meat damage this turn"
             :effect (effect (damage-prevent :meat 1))}]})

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
             :once :per-turn
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
         :player :runner
         :req (req (pos? (count (:hand runner))))
         :choices {:req (req (and (runner? target)
                                  (in-hand? target)
                                  (not (event? target))
                                  (runner-can-pay-and-install? state side eid target {:no-toast true})))}
         :msg (msg "install " (:title target))
         :effect (effect (runner-install
                          (assoc eid :source card :source-type :runner-install)
                          target nil))
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
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional
             {:req (req (first-event? state side :successful-run))
              :player :runner
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
                             {:player :runner
                              :optional
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
  (let [patchwork-ability {:once :per-turn
                           :effect (effect (update! (assoc-in card [:special :patchwork] true)))}]
    {:static-abilities [(mu+ 1)]
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
                          patchwork card
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
                                                (register-once state side patchwork-ability patchwork)
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
             :effect (req (add-counter state :runner eid card :credit 1 nil))}]
   :abilities [{:cost [(->c :click 1)]
                :label "Gain 1 [Credits]. Take all hosted credits"
                :async true
                :msg (msg "gain " (inc (get-counters card :credit)) " [Credits]")
                :effect (req (let [credits (inc (get-counters card :credit))]
                               (add-counter state side card :credit (-(dec credits)))
                               (gain-credits state :runner eid credits)))}]})

(defcard "Plascrete Carapace"
  {:data {:counter {:power 4}}
   :interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :events [(trash-on-empty :power)]
   :abilities [{:cost [(->c :power 1)]
                :msg "prevent 1 meat damage"
                :effect (req (damage-prevent state side :meat 1))}]})

(defcard "Poison Vial"
  {:data {:counter {:power 3}}
   :events [(trash-on-empty :power)]
   :abilities [(break-sub [(->c :power 1)] 2 "All" {:req (req (any-subs-broken? current-ice))})]})

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
                        :player :runner
                        :prompt "Look at top 2 cards of the stack?"
                        :yes-ability
                        {:msg "look at the top 2 cards of the stack"
                         :choices ["OK"]
                         :prompt (msg "The top 2 cards of the stack are "
                                      (enumerate-str (map :title (take 2 (:deck runner)))))}}}]
   :abilities [(set-autoresolve :auto-fire "Prognostic Q-Loop")
               {:label "Reveal and install top card of the stack"
                :once :per-turn
                :cost [(->c :credit 1)]
                :req (req (pos? (count (:deck runner))))
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
                         {:req (req (or (program? top-card)
                                        (hardware? top-card)))
                          :prompt (msg "Install " (:title top-card) "?")
                          :yes-ability
                          {:async true
                           :effect (effect (runner-install (assoc eid :source-type :runner-install) top-card nil))}}})
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
  {:flags {:runner-phase-12 (req true)
           :forced-to-avoid-tag true}
   :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                :once :per-turn
                :req (req (:runner-phase-12 @state))
                :effect (effect (update! (assoc card :qianju-active true)))
                :msg (msg "lose [Click] and avoid the first tag received until [their] next turn")}]
   :events [{:event :corp-turn-ends
             :effect (effect (update! (dissoc card :qianju-active)))}
            {:event :runner-turn-begins
             :req (req (:qianju-active card))
             :effect (effect (lose-clicks 1))}
            {:event :pre-tag
             :async true
             :req (req (:qianju-active card))
             :msg "avoid the first tag received"
             :effect (effect (update! (dissoc card :qianju-active))
                             (tag-prevent :runner eid 1))}]})

(defcard "R&D Interface"
  {:events [(breach-access-bonus :rd 1)]})

(defcard "Rabbit Hole"
  {:static-abilities [(link+ 1)]
   :on-install
   {:optional
    {:req (req (some #(when (= (:title %) (:title card)) %) (:deck runner)))
     :prompt (msg "Install another copy of " (:title card) "?")
     :yes-ability {:async true
                   :msg "install another copy of itself"
                   :effect (req (trigger-event state side :searched-stack)
                                (shuffle! state :runner :deck)
                                (when-let [c (some #(when (= (:title %) (:title card)) %)
                                                   (:deck runner))]
                                  (runner-install state side eid c nil)))}}}})

(defcard "Ramujan-reliant 550 BMI"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:async true
                :label "prevent net or core damage"
                :trash-icon true
                :req (req (not-empty (:deck runner)))
                :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                               (continue-ability
                                 state side
                                 {:async true
                                  :prompt "How much damage do you want to prevent?"
                                  :choices {:number (req (min n (count (:deck runner))))}
                                  :msg (msg "trash " (enumerate-str (map :title (take target (:deck runner))))
                                            " from the stack and prevent " target " damage")
                                  :cost [(->c :trash-can)]
                                  :effect (effect (damage-prevent :net target)
                                                  (damage-prevent :brain target)
                                                  (mill :runner eid :runner target))}
                                 card nil)))}]})

(defcard "Recon Drone"
  ; eventmap uses reverse so we get the most recent event of each kind into map
  (letfn [(eventmap [s]
            (into {} (reverse (get s :turn-events))))]
    {:interactions {:prevent [{:type #{:net :brain :meat}
                               :req (req (and (:access @state)
                                              (= (:cid (:card (first (:pre-damage (eventmap @state)))))
                                                 (:cid (first (:pre-access-card (eventmap @state)))))))}]}
     :abilities [{:cost [(->c :x-credits) (->c :trash-can)]
                  :label "prevent damage"
                  :req (req (and (:access @state)
                                 (= (:cid (:card (first (:pre-damage (eventmap @state)))))
                                    (:cid (first (:pre-access-card (eventmap @state)))))))
                  :msg (msg "prevent " (cost-value eid :x-credits) " damage")
                  :effect (effect (damage-prevent (:type (first (:pre-damage (eventmap @state))))
                                                  (cost-value eid :x-credits)))}]}))

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
                 :effect (req (wait-for (draw state :runner 1)
                                        (add-counter state side (get-card state card) :power 1)
                                        (if (= 3 (get-counters (get-card state card) :power))
                                          (do (system-msg state :runner (str "trashes " (:title card) " as it reached 3 power counters"))
                                              (trash state side eid card {:unpreventable true
                                                                          :cause-card card}))
                                          (effect-completed state side eid))))}
        event {:req (req (zero? (count (:hand runner))))
               :async true
               :effect (effect (continue-ability ability card targets))}]
    {:implementation "Only watches trashes, playing events, and installing"
     :on-install {:async true
                  :msg "suffer 1 meat damage"
                  :effect (effect (damage eid :meat 1 {:unboostable true
                                                       :card card}))}
     :events [(assoc event :event :play-event)
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
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}
              {:event :corp-turn-begins
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}]
     :abilities [ability]}))

(defcard "Rubicon Switch"
  {:abilities [{:cost [(->c :click 1)]
                :label "Derez a piece of ice rezzed this turn"
                :once :per-turn
                :async true
                :prompt "How many credits do you want to spend?"
                :choices :credit
                :effect (effect (continue-ability
                                  (let [spent-credits target]
                                    {:choices {:card #(and (ice? %)
                                                          (= :this-turn (:rezzed %))
                                                          (<= (:cost %) target))}
                                    :effect (effect (derez target))
                                    :msg (msg "spend " spent-credits "[Credits] and derez " (:title target))})
                                    card nil))}]})

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
    {:abilities [{:req (req (<= 2 (count (:hand runner))))
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
                :req (req (some #(and (program? %)
                                      (runner-can-pay-and-install?
                                        state side
                                        (assoc eid :source card :source-type :runner-install)
                                        % {:cost-bonus -3}))
                                (:discard runner)))
                :cost [(->c :trash-can)]
                :msg "install a program"
                :effect
                (effect
                  (continue-ability
                    {:show-discard true
                     :choices {:req (req (and (in-discard? target)
                                              (program? target)
                                              (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                        [(->c :credit (install-cost state side target {:cost-bonus -3}))])))}
                     :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}
                    card nil))}]})

(defcard "Skulljack"
  {:on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :static-abilities [{:type :trash-cost
                       :value -1}]})

(defcard "Solidarity Badge"
  {:events [{:event :runner-turn-begins
             :req (req (pos? (get-counters (get-card state card) :power)))
             :async true
             :interactive (req (pos? (get-counters (get-card state card) :power)))
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req ["Draw 1 card"
                            (when (pos? (count-real-tags state)) "Remove 1 tag")
                            "Done"])
             :effect (req (if (= target "Draw 1 card")
                            (do (add-counter state side card :power -1)
                                (system-msg state side (str "uses " (:title card)
                                                            " to draw 1 card"))
                                (draw state :runner eid 1))
                            (if (= target "Remove 1 tag")
                              (do
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
             :once :per-turn
             :msg "place 1 power counter on itself"
             :effect (effect (add-counter :runner card :power 1)
                             (effect-completed eid))}]})

(defcard "Spinal Modem"
  {:static-abilities [(mu+ 1)]
   :recurring 2
   :events [{:event :successful-trace
             :req (req run)
             :msg "suffer 1 core damage"
             :effect (effect (damage eid :brain 1 {:card card}))}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Sports Hopper"
  {:static-abilities [(link+ 1)]
   :abilities [{:label "Draw 3 cards"
                :msg "draw 3 cards"
                :async true
                :cost [(->c :trash-can)]
                :effect (effect (draw :runner eid 3))}]})

(defcard "Spy Camera"
  {:abilities [{:cost [(->c :click 1)]
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
              :player :runner
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
             :req (req (= :hq target))
             :effect (req (let [broken-ice
                                (->> (run-events state side :subroutines-broken)
                                     (filter (fn [[ice _broken-subs]]
                                               (and (= :hq (second (get-zone ice)))
                                                    (all-subs-broken? ice)
                                                    (get-card state ice))))
                                     (keep #(:cid (first %)))
                                     (into #{}))
                                hq-ice
                                (->> (get-in @state (concat [:corp :servers :hq :ices]))
                                     (keep :cid)
                                     (filter broken-ice))]
                            (access-bonus state :runner :hq (count hq-ice))))}]})

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
               :msg (msg "reveal " rev-str " from the top of the stack"
                         (when-not (= target "No install")
                           (str " and " (decapitalize target) ", ignoring all costs")))
               :effect (req (if-not (= target "No install")
                              (wait-for (runner-install
                                          state side
                                          (make-eid state {:source card :source-type :runner-install})
                                          (if (= target (str "Install " (:title first-card)))
                                            first-card second-card)
                                          {:ignore-all-cost true})
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
             :async true
             :effect (req (if (<= 3 (get-counters (get-card state card) :power))
                            (wait-for (trash state side card {:unpreventable :true
                                                              :cause-card card})
                                      (continue-ability state side
                                                        (sabotage-ability 3)
                                                        card nil))
                            (do (system-msg state side (str "uses " (:title card) " to place 1 power counter on itself"))
                                (add-counter state side card :power 1)
                                (effect-completed state side eid))))}]})

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
                                 :msg (msg "trash " (enumerate-str (map :title targets)))
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
   [{:cost [(->c :click 2)]
     :req (req (and (some #{:hq} (:successful-run runner-reg))
                    (seq (filter
                           #(and (rezzed? %)
                                 (installed? %)
                                 (or (has-subtype? % "Bioroid")
                                     (has-subtype? % "Clone")
                                     (has-subtype? % "Executive")
                                     (has-subtype? % "Sysop")))
                           (all-active-installed state :corp)))))
     :label "trash a Bioroid, Clone, Executive or Sysop"
     :prompt "Choose a Bioroid, Clone, Executive, or Sysop to trash"
     :choices {:card #(and (rezzed? %)
                           (installed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop")))}
     :async true
     :msg (msg "trash " (:title target))
     :effect (effect (trash eid target {:cause-card card}))}]})

(defcard "Vigil"
  (let [ability {:req (req (and (:runner-phase-12 @state)
                                (= (count (:hand corp)) (hand-size state :corp))))
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
             :effect (req (add-counter state :runner card :power 1 {:placed true})
                          (effect-completed state side eid))}
            {:event :breach-server
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
                             :effect (effect (access-bonus :rd (max 0 target))
                                             (add-counter :runner card :power (- target) {:placed true})
                                             (effect-completed eid))}
                            card nil))}]})

(defcard "Window"
  {:abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :msg "draw 1 card from the bottom of the stack"
                :effect (effect (move (last (:deck runner)) :hand))}]})

(defcard "Zamba"
  {:implementation "Credit gain is automatic"
   :static-abilities [(mu+ 2)]
   :events [{:event :expose
             :async true
             :effect (effect (gain-credits :runner eid 1))
             :msg "gain 1 [Credits]"}]})

(defcard "Zenit Chip JZ-2MJ"
  {:on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :events [{:event :successful-run
             :async true
             :req (req (and (is-central? (:server context))
                            (first-event? state side :successful-run
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (is-central? (:server context)))))))
             :msg "draw 1 card"
             :effect (req (draw state :runner eid 1))}]})

(defcard "Zer0"
  {:abilities [{:cost [(->c :click 1) (->c :net 1)]
                :once :per-turn
                :msg "gain 1 [Credits] and draw 2 cards"
                :async true
                :effect (req (wait-for (gain-credits state side 1)
                                       (draw state side eid 2)))}]})
