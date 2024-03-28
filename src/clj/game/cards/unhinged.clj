(ns game.cards.unhinged
  (:require
   [clojure.string :as str]
   [clojure.string :as str]
   [game.cards.ice :refer [reset-variable-subs trace-ability trash-installed-sub end-the-run bioroid-break trash-installed-sub cannot-steal-or-trash-sub end-the-run-unless-runner-pays gain-variable-subs]]
   [game.cards.programs :refer [heap-breaker-auto-pump-and-break]]
   [game.core.access :refer [access-card breach-server get-only-card-to-access
                             num-cards-to-access]]
   [game.core.actions :refer [resolve-score score play-ability]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed server->zone card->server]]
   [game.core.card :refer [agenda? asset? assoc-host-zones can-be-advanced?
                           corp-installable-type? corp? event? faceup? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype?
                              identity? ice? in-discard? in-hand? in-play-area? in-set-aside? installed? is-type? operation? program?
                           resource? rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.charge :refer [can-charge charge-card]]
   [game.core.cost-fns :refer [ignore-install-cost? install-additional-cost-bonus install-cost rez-cost]]
   [game.core.damage :refer [damage damage-bonus damage-prevent]]
   [game.core.def-helpers :refer [combine-abilities corp-recur defcard
                                  do-brain-damage do-net-damage offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw draw-bonus]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.engine :refer [not-used-once? pay register-events dissoc-req
                             resolve-ability trigger-event trigger-event-simult
                             unregister-events unregister-floating-events]]
   [game.core.events :refer [first-event? last-turn? no-event? not-last-turn?
                             run-events turn-events]]
   [game.core.finding :refer [find-latest]]
   [game.core.flags :refer [in-runner-scored? in-corp-scored? can-host? register-turn-flag! can-score?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-credits lose-clicks]]
   [game.core.hand-size :refer [hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.identities :refer [disable-identity enable-identity disable-card enable-card]]
   [game.core.ice :refer [all-subs-broken-by-card? all-subs-broken? get-current-encounter
                          any-subs-broken-by-card? auto-icebreaker break-sub
                          break-subroutine! break-subroutines-msg breaker-strength-bonus dont-resolve-subroutine!
                          get-strength ice-strength pump pump-ice set-current-ice strength-pump update-current-encounter
                          unbroken-subroutines-choice update-all-ice update-all-icebreakers update-breaker-strength]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.installing :refer [corp-install corp-install-msg
                                 install-as-condition-counter install-locked?
                                 runner-can-install? runner-can-pay-and-install? runner-install]]
   [game.core.mark :refer [identify-mark-ability]]
   [game.core.memory :refer [available-mu caissa-mu+ mu+ update-mu virus-mu+]]
   [game.core.moving :refer [as-agenda flip-faceup forfeit mill move swap-ice swap-installed
                             remove-from-currently-drawing trash trash-cards
                             trash-prevent]]
   [game.core.payment :refer [can-pay?]]
   [game.core.play-instants :refer [can-play-instant? play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt clear-run-prompts]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon]]
   [game.core.revealing :refer [conceal-hand reveal reveal-hand]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [bypass-ice can-run-server? end-run encounter-ends force-ice-encounter gain-next-run-credits get-runnable-zones handle-end-run clear-encounter
                           make-run prevent-access successful-run-replace-breach redirect-run set-next-phase start-next-phase
                           total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [play-sfx system-msg system-say say]]
   [game.core.servers :refer [central->name is-central? is-remote? name-zone unknown->kw
                              same-server? target-server zone->name]]
   [game.core.set-aside :refer [set-aside get-set-aside set-aside-for-me]]
   [game.core.set-up :refer [build-card]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [show-error-toast toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters]]
   [game.core.winning :refer [check-win-by-agenda win tie]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [jinteki.validator :refer [legal?]]

   ))

(defn- roll-dice [sides]
  ;; TODO - I want to add a dice-rolling sound here!
  ;; Alternatively, have a jackpot and lowroll sound effect too!
  (inc (rand-int sides)))

(defn- alphabet-breaker-check
  [alpha card]
  (contains? (set alpha) (first (str/lower-case (:title card)))))

(defn- add-token-to-grip
  ([state side token]
   (try
     (let [s-card (server-card token)
           card (when s-card
                  (build-card s-card))]
       (when card
         (swap! state update-in [:runner :hand] #(concat % [(assoc card :zone [:hand])]))))
     (catch Exception ex
       (toast state side (str token " isn't a real card"))))))

(defcard "Alarm"
  ;; When you rez this ice, play the game over sound effect.
  ;; [sub] derez this
  ;; ice.
  {:on-rez {:effect (req
                      ;; note - the sound queue is limited to 3 entries
                      ;; this is a very easy way to cheat the "Rez" sound effect out of the queue!
                      (play-sfx state nil "game-end")
                      (play-sfx state nil "game-end")
                      (play-sfx state nil "game-end"))}
   :subroutines [{:msg (msg "derez " (:title card))
                  :label "Derez this ice"
                  :async true
                  :effect (req (derez state side card)
                               (encounter-ends state side eid))}]})


(defcard "Chained Cantrip"
  ;; Reveal the top two cards of the stack. You may play the first one, paying all costs, if able.
  ;; Then, if you did, and the first card revealed did not cost more than the second, and was not named 'Chained Cantrip',
  ;; repeat this process
  ;; able.
  ;; If you do, repeat this process.
  (letfn [(reveal-fn []
            {:req (req (seq (:deck runner)))
             :msg (msg "reveal " (:title (first (:deck runner))) " and " (:title (second (:deck runner))) " from the top of the Stack")
             :async true
             :effect (req (let [top-card (first (:deck runner))
                                second-card (second (:deck runner))]
                            (wait-for (reveal state side [top-card second-card])
                                      (if (event? top-card)
                                        (continue-ability
                                          state side
                                          {:optional
                                           {:req (req (can-play-instant? state side (assoc eid :source card :source-type :runner-install) top-card))
                                            :prompt (msg "Play " (:title top-card) ", paying all costs?")
                                            :yes-ability {:msg (msg "play " (:title top-card) ", paying all costs")
                                                          :async true
                                                          :effect (req (wait-for (play-instant state side top-card)
                                                                                 (if (and
                                                                                       second-card
                                                                                       (<= (:cost top-card) (:cost second-card))
                                                                                       (not= (:title card) (:title top-card)))
                                                                                   (continue-ability
                                                                                     state side
                                                                                     (reveal-fn)
                                                                                     card nil)
                                                                                   (effect-completed state side eid))))}
                                            :no-ability {:msg (msg "decline to play " (:title top-card))}}}
                                          card nil)
                                        ;; installable
                                        (if (can-pay? state side (assoc eid :source card :source-type :runner-install) top-card nil
                                                      [:credit (install-cost state side top-card)])
                                          (continue-ability
                                            state side
                                            {:optional
                                             {:prompt (msg "Install " (:title top-card) ", paying all costs?")
                                              :yes-ability {:msg (msg "install " (:title top-card) ", paying all costs")
                                                            :effect (req (wait-for (runner-install state side top-card)
                                                                                   (if (and
                                                                                         second-card
                                                                                         (<= (:cost top-card) (:cost second-card))
                                                                                         (not= (:title card) (:title top-card)))
                                                                                     (continue-ability
                                                                                       state side
                                                                                       (when async-result
                                                                                         (reveal-fn))
                                                                                       card nil)
                                                                                     (effect-completed state side eid))))}
                                              :no-ability {:msg (msg "decline to install " (:title top-card))}}}
                                            card nil)
                                          (effect-completed state side eid))))))})]
    {:on-play (reveal-fn)}))


(defcard "You have three cards"
  ;;
  {:events [
            {:event :post-corp-draw
             :req (req (> (count (:hand corp)) 3))
             :async true
             :effect (req (let [to-destroy (- (count (:hand corp)) 3)]
                            (continue-ability
                              state side
                              {:prompt (str "Choose " to-destroy " cards in HQ to discard")
                               :player :corp
                               :choices {:all true
                                         :max to-destroy
                                         :card #(and (in-hand? %)
                                                     (corp? %))}
                               :msg "discard down to hand size"
                               :async true
                               :effect (req (trash-cards state :corp eid targets nil))}
                              card nil)))}
            {:event :card-moved
             :req (req (and (< (count (:hand corp)) 3)
                            (= :hand (first (:previous-zone (second targets))))))
             :msg (msg "draw a card")
             :effect (req (let [to-draw (- 3 (count (:hand corp)))]
                            (draw state :corp eid to-draw)))}]})

(defcard "Kick Me"
  ;; Install "Kick Me" on an install Corporation card as a hosted condition
  ;; counter with the text
  ;; <em>"Whenever you pass or access this card, the
  ;; corporation discards a card from HQ at random unless they pay
  ;; [credit]"</em>
  (let [discard-fn {:player :corp
                    :condition :hosted
                    :prompt "Choose one"
                    :choices (req [(when (can-pay? state :corp
                                                   (assoc eid :source card :source-type :ability)
                                                   card nil [:credit 1])
                                     "Pay 1 [credit]")
                                   "Discard a card at random"])
                    :async true
                    :waiting-prompt true
                    :msg (msg "force the corp to " (str/lower-case target))
                    :effect (req (if (= target "Pay 1")
                                   (pay state :corp eid card :credit 1)
                                   (trash state :runner eid (first (shuffle (:hand corp)))
                                          {:cause-card card})))}]
    {:on-play {:choices {:card #(and (corp? %)
                                     (installed? %))}
               :msg (msg "give " (card-str state target {:visible false}) " additional text")
               :async true
               :effect (effect (install-as-condition-counter eid card target))}
     :events [(assoc discard-fn :event :pass-ice
                     :req (req (and (same-card? (:ice context) (:host card))
                                    (pos? (count (:hand corp))))))
              (assoc discard-fn :event :access
                     :req (req (and (same-card? target (:host card))
                                    (pos? (count (:hand corp))))))]}))

(defcard "D. Sporado: Compulsive Runner"
  ;; You cannot click for credits using the basic action card.
  ;; Whenever you make
  ;; a run using the basic action card, gain a credit.
  {:static-abilities [{:type :card-ability-additional-cost
                       :req (req (let [targetcard (first targets)
                                       target (second targets)]
                                   (and (same-card? targetcard (:basic-action-card runner))
                                        (= "Gain 1 [Credits]" (:label target)))))
                       :value [:credit 1000]}]
   ;; ^^ above is an EVIL hack
   :events [{:event :run
             :req (req (:click-run target))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "You should really be more careful"
  ;; This asset can be advanced.
  ;; When the runner accesses this card while it is
  ;; advanced, replace every card in their grip with a copy of
  ;; <strong>Infiltration</strong>.
  {:advanceable :always
   :on-access {:req (req (pos? (get-counters (get-card state card) :advancement)))
               :msg (msg "remove " (str/join ", " (map :title (:hand runner)))
                         " in the Grip from the game and add " (count (:hand runner))
                         " copies of infiltration to the grip")
               :effect (req (let [to-destroy (:hand runner)
                                  qty (count to-destroy)]
                              (doseq [c to-destroy]
                                (move state :runner c :rfg))
                              (dotimes [_ qty]
                                (add-token-to-grip state side "Infiltration"))))}})

(defcard "Awakened Dreamer"
  ;; 2[recurring]: use these credits for anything.
  ;; When your turn begins, lose
  ;; all clicks.
  ;; Then, take 10 basic actions at random, with random targets,
  ;; ignoring their costs (you still pay the play cost of events and
  ;; operations).
  (letfn [(resolve-bac-abi [state abi-num targets card eid remaining]
            (let [bac (get-in @state [:runner :basic-action-card])
                  abi (nth (:abilities bac) abi-num)]
              (wait-for (resolve-ability state :runner (assoc abi :cost nil) bac targets)
                        (continue-ability
                          state :runner
                          (choose-an-action (dec remaining))
                          card nil))))

          (choose-an-action [rem]
            (if (zero? rem)
              nil
              (let [target-action (first (shuffle [:draw :cred :remove-tag
                                                   :run :install :play]))]
                ;; :install :play]))]
                (condp = target-action

                  ;; play a random event
                  :play {:effect (req (let [cards (filter event? (:hand runner))
                                            playable (filter #(can-play-instant? state :runner (assoc eid :source :action :source-type :play) % {:no-additional-cost true}) cards)
                                            chosen (first playable)]
                                        (if chosen
                                          (wait-for (resolve-ability
                                                      state side
                                                      {:label "Play 1 event"
                                                       :async true
                                                       :effect (req (play-instant state :runner (assoc eid :source :action :source-type :play)
                                                                                  chosen {:no-additional-cost true}))}
                                                      card nil)
                                                    (continue-ability state side (choose-an-action (dec rem)) card nil))
                                          (continue-ability
                                            state :runner
                                            (choose-an-action rem)
                                            card nil))))
                         :async true}

                  :install {:effect (req (let [cards (filter (complement event?) (:hand runner))
                                               installable (filter #(runner-can-pay-and-install?
                                                                      state side
                                                                      (assoc eid :source card :source-type :runner-install)
                                                                      %
                                                                      nil) cards)
                                               chosen (first installable)]
                                           (if chosen
                                             (wait-for (resolve-ability
                                                         state side
                                                         {:label "Install 1 program, resource, or piece of hardware from the grip"
                                                          :async true
                                                          :effect (req (runner-install
                                                                         state :runner (assoc eid :source :action :source-type :runner-install)
                                                                         chosen {:no-toast true}))}
                                                         card nil)
                                                       (continue-ability
                                                         state :runner
                                                         (choose-an-action (dec rem))
                                                         card nil))
                                             (continue-ability
                                               state :runner
                                               (choose-an-action rem)
                                               card nil))))
                            :async true}

                  ;; run on a random server
                  :run {:effect (req (let [target-server (first (shuffle runnable-servers))]
                                       (system-msg state side "uses Runner Basic Action Card to make a run")
                                       (wait-for (make-run state :runner target-server card)
                                                 (continue-ability
                                                   state :runner
                                                   (choose-an-action (dec rem))
                                                   card nil))))
                        ;;(resolve-bac-abi state 4 [target-server] card eid rem)))
                        :async true}

                  ;; remove a tag
                  :remove-tag {:effect (req
                                         (if tagged
                                           (resolve-bac-abi state 5 nil card eid rem)
                                           (continue-ability
                                             state side
                                             (choose-an-action rem)
                                             card nil)))
                               :async true}

                  ;; gain a credit
                  :cred {:effect (req (resolve-bac-abi state 0 nil card eid rem))
                         :async true}

                  ;; draw a card
                  :draw {:effect (req
                                   (if (seq (:deck runner))
                                     (resolve-bac-abi state 1 nil card eid rem)
                                     (continue-ability
                                       state side
                                       (choose-an-action rem)
                                       card nil)))
                         :async true}))))]
    {;:data {:counter {:credit 2}}
     :interactions {:pay-credits {:req (req true)
                                  :type :recurring}}
     :events [{:event :runner-turn-begins
               :msg (msg "lose all clicks, gain 1 credit, and take 10 actions at random")
               :async true
               :effect (req (lose-clicks state side 15)
                            (wait-for (gain-credits state side card 1)
                                      (continue-ability state side (choose-an-action 10) (get-card state card) nil)))}]}))



(defcard "NBN: Personal Evolution"
  ;; Whenever an agenda is scored or stolen, do 1 net damage.
  (let [ability {:async true
                 :req (req (not (:winner @state)))
                 :msg "do 1 net damage"
                 :effect (effect (damage eid :net 1 {:card card}))}]
    {:events [(assoc ability
                     :event :agenda-scored
                     :interactive (req true))
              (assoc ability :event :agenda-stolen)]}))

(defcard "Mary \"Many\" Consoles"
  ;; You may only include one copy of each card in your deck.
  ;; Ignore all
  ;; <em>"Limit 1 console per player"</em> text.
  {:static-abilities [{:type :enable-multiple-consoles
                       :req (req true)
                       :value (req true)}]})

(defcard "Ganksel!"
  ;; When the runner accesses this card, they encounter it as an ice with the
  ;; following text:
  ;;
  ;; Lose [click][click]: Break 2 subroutines on this ice. Only
  ;; the Runner can use this ability.
  ;;
  ;; [subroutine] Trash 1 installed resource.
  ;; [subroutine] You may install 1 asset from HQ or Archives.
  ;; [subroutine] You
  ;; may install 1 ice from HQ or Archives.
  ;; [subroutine] The Runner cannot steal
  ;; or trash Corp cards for the remainder of this run.
  (letfn [(install-sub
            [type line]
            {:prompt (msg "Choose " line " to install from Archives or HQ")
             :waiting-prompt true
             :label (str "install " line " from Archives or HQ")
             :show-discard true
             :choices {:card #(and (corp? %)
                                   (or (and (= type "Ice") (ice? %))
                                       (and (= type "Asset") (asset? %)))
                                   (or (in-hand? %)
                                       (in-discard? %)))}
             :async true
             :msg (msg (corp-install-msg target))
             :effect (req (let [this (zone->name (second (get-zone card)))
                                nice target]
                            (corp-install state side eid target nil)))})]
    {:flags {:rd-reveal (req true)}
     :subroutines [trash-installed-sub
                   (install-sub "Asset" "an asset")
                   (install-sub "Ice" "an ice")
                   cannot-steal-or-trash-sub]
     :runner-abilities [(bioroid-break 2 2)]
     :on-access {:async true
                 :req (req (not (in-discard? card)))
                 :msg "force the Runner to encounter Ganksel!"
                 :effect (req
                           (if (installed? card)
                             (wait-for (rez state side card nil)
                                       (force-ice-encounter state side eid (get-card state card)))
                             (force-ice-encounter state side eid (get-card state card))))}}))

(defcard "A to G breaker"
  ;; 1[credit]: +1strength
  ;; 1[credit]: break 1 subroutine that starts with
  ;; letters from A to E
  (auto-icebreaker {:abilities [(break-sub 1 1 "All" {:req (req (alphabet-breaker-check
                                                                  "ABCDEabcde"
                                                                  current-ice))})
                                (strength-pump 1 1)]}))


(defcard "Unsure Gamble"
  ;; Gain 1d10 credits.
    {:on-play {:async true
             :effect (req (let [roll (roll-dice 10)]
                            (continue-ability
                              state side
                              {:msg (msg "gain " roll " [credits]")
                               :async true
                               :effect (req (gain-credits state side eid roll))}
                              card nil)))}})

(defcard "Mirror Trap"
  ;; When you rez this ice during a run, you may swap it with another rezzed ice. If
  ;; you do, derez this ice, and the runner is now encountering that ice.
  {:on-rez {:req (req run)
            :choices {:req (req (and (ice? target)
                                     (rezzed? target)
                                     (not (same-card? card target))))}
            :prompt "Swap mirror trap with another ice?"
            :msg (msg "swap Mirror Trap with " (card-str state target)
                      ", force the runner to encounter it, and derez Mirror Trap")
            :effect (req (let [this card
                               that (get-card state target)]
                           ;;(when-not (= (second (get-zone this)) (second (get-zone that)))
                           ;;  (redirect-run state side (second (get-zone state this)) :encounter-ice\
))
                           (wait-for (encounter-ends state side)
                                     (swap! state update-in [:run]
                                            #(assoc % :position (inc (:index this)) :server [(second \
(get-zone this))]))
                                     (swap-ice state side this that)
                                     (set-next-phase state :encounter-ice)
                                     (update-all-ice state side)
                                     (update-all-icebreakers state side)
                                     (start-next-phase state side eid)
                                     (effect-completed state side eid))))
            :async true}
   :subroutines [(assoc (end-the-run-unless-runner-pays [:credit 1]) :label " [credit] Ɩ ƨγɒq ɿɘnnυɿ \
ɘʜɈ ƨƨɘlnυ nυɿ ɘʜɈ bnƎ")]})

(defcard "Longsort"
  ;; Choose one:
  ;; * Sort your deck by cost (lowest cost at top)
  ;; * Sort your deck
  ;; by cost (lowest cost at bottom)
  (let [all [{:effect (req (swap! state update-in [:runner :deck]
                                  (fn [x] (sort #(compare (:cost %1) (:cost %2)) x))))
              :msg "sort the stack by cost (lowest at the top)"}
             {:effect (req (swap! state update-in [:runner :deck]
                                  (fn [x] (sort #(compare (:cost %2) (:cost %1)) x))))
              :msg "sort the stack by cost (lowest at the bottom)"}]
        choice (fn choice [abis]
                 {:prompt "Choose an ability to resolve"
                  :choices (map #(capitalize (:msg %)) abis)
                  :async true
                  :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                 (wait-for
                                   (resolve-ability state side chosen card nil)
                                   (effect-completed state side eid))))})]
    {:on-play
     {:async true
      :effect (effect (continue-ability (choice all) card nil))}}))

(defcard "You have three credits"
  ;; You have 3[credits].
  (let [fix-creds {:req (req (not= 3 (:credit corp)))
                   :async true
                   ;;:msg (msg "fix credits")
                   :effect (req (let [diff (- 3 (:credit corp))]
                                  (if (pos? diff)
                                    (gain-credits state :corp eid diff)
                                    (lose-credits state :corp eid (- diff)))))}]
    {:events [{:event :pre-start-game
	       :req (req (= :corp side))
	       :async true
               :effect (req (lose-credits state :corp eid 2))}
              (assoc fix-creds :event :corp-credit-gain)
              (assoc fix-creds :event :corp-spent-credits)
              (assoc fix-creds :event :corp-credit-loss)]}))

(defcard "QuickSort"
  ;; Choose one:
  ;; * The corporation arranges R&D in sorted order by name (A's
  ;; ontop, they may look at R&D)
  ;; * The runner arranges the stack in sorted
  ;; order by name (A's ontop, they may look at the stack)
  (let [all [{:effect (req (swap! state update-in [:runner :deck]
                                  (fn [x] (sort #(compare (:title %1) (:title %2)) x))))
              :msg "sort the stack by name"}
             {:effect (req (swap! state update-in [:corp :deck]
                                  (fn [x] (sort #(compare (:title %1) (:title %2)) x))))

              :msg "force the Corporation to sort R&D by name"}]
        choice (fn choice [abis]
                 {:prompt "Choose an ability to resolve"
                  :choices (map #(capitalize (:msg %)) abis)
                  :async true
                  :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                 (wait-for
                                   (resolve-ability state side chosen card nil)
                                   (effect-completed state side eid))))})]
    {:on-play
     {:async true
      :effect (effect (continue-ability (choice all) card nil))}}))

(defcard "I want that one"
  ;; Play only if you made a successful run on HQ, R&D and Archives this turn.
  ;; Access any visible card.
  {:on-play {:req (req (and (some #{:hq} (:successful-run runner-reg))
                            (some #{:rd} (:successful-run runner-reg))
                            (some #{:archives} (:successful-run runner-reg))))
             :choices {:card #(corp? %)}
             :msg (msg "access " (:title target))
	     :async true
             :effect (effect (access-card eid target))}})

(defcard "Newer Angeles Sol"
  ;; Whenever an agenda is scored or stolen, and whenever a run ends, you may
  ;; play a lockdown from HQ or Archives (paying all costs).
  (let [nasol {:optional
               {:prompt "Play a Lockdown?"
                :player :corp
                :req (req (and (some #(has-subtype? % "Lockdown")
                                     (concat (:hand corp) (:discard corp)))
	                       (not (some #(has-subtype? % "Lockdown")(:play-area corp)))))
                :yes-ability {:prompt "Choose a Lockdown to play from HQ or Archives"
                              :show-discard true
                              :async true
                              :choices {:card #(and (has-subtype? % "Lockdown")
                                                    (corp? %)
                                                    (or (in-hand? %)
                                                        (in-discard? %)))}
                              :msg (msg "play a lockdown from " (name-zone "Corp" (get-zone target)))
                              :effect (effect (play-instant eid target))}}}]
    {:events [(assoc nasol :event :agenda-scored)
              (assoc nasol :event :agenda-stolen)
              (assoc nasol :event :run-ends :req (req (first-event? state side :run-ends)))]}))
