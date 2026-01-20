(ns game.core.actions
  (:require
    [clj-uuid :as uuid]
    [clojure.stacktrace :refer [print-stack-trace]]
    [clojure.string :as string]
    [game.core.agendas :refer [update-advancement-requirement update-all-advancement-requirements update-all-agenda-points]]
    [game.core.board :refer [installable-servers]]
    [game.core.card :refer [get-advancement-requirement get-agenda-points get-card get-counters]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [break-sub-ability-cost card-ability-cost card-ability-cost score-additional-cost-bonus]]
    [game.core.effects :refer [any-effects is-disabled-reg?]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [ability-as-handler checkpoint register-once register-pending-event pay queue-event resolve-ability trigger-event-simult]]
    [game.core.flags :refer [can-advance? can-score?]]
    [game.core.ice :refer [break-subroutine! break-subs-event-context get-current-ice get-pump-strength get-strength pump resolve-subroutine! resolve-unbroken-subs! substitute-x-credit-costs]]
    [game.core.initializing :refer [card-init]]
    [game.core.moving :refer [move trash]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs build-cost-string ->c]]
    [game.core.play-instants :refer [play-instant]]
    [game.core.expend :refer [expend expendable?]]
    [game.core.prompt-state :refer [remove-from-prompt-queue]]
    [game.core.prompts :refer [resolve-select first-prompt-by-eid first-selection-by-eid]]
    [game.core.props :refer [add-counter add-prop set-prop]]
    [game.core.runs :refer [continue get-runnable-zones]]
    [game.core.say :refer [play-sfx system-msg implementation-msg n-last-logs]]
    [game.core.servers :refer [name-zone zones->sorted-names]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [dissoc-in quantify remove-once same-card? same-side? server-cards to-keyword]]
    [taoensso.timbre :as timbre]))

(defn- without-history
  "Returns the given atom without history keys"
  [state]
  (dissoc state :log :history :click-states :turn-state :paid-ability-state))

(defn- update-click-state
  "Update :click-states to hold latest 4 moments before performing actions."
  [state ability]
  (when (:action ability)
    (let [state' (without-history @state)
          click-states (vec (take-last 4 (conj (:click-states @state) state')))]
      (swap! state assoc :click-states click-states))))

(defn- update-paid-ability-state
  "Holds a single state for undo-paid-ability command. Gets cleared on taking an action"
  [state ability]
  (if (:action ability)
    (swap! state dissoc :paid-ability-state)
    (swap! state assoc :paid-ability-state (without-history @state))))

(defn- update-history!
  [state ability]
  (update-paid-ability-state state ability)
  (update-click-state state ability))

(defn- no-blocking-prompt?
  [state side]
  (let [prompt-type (get-in @state [side :prompt-state :prompt-type])]
    (or (= nil prompt-type)
        (= :run prompt-type)
        (= :prevent prompt-type))))

(defn- no-blocking-or-prevent-prompt?
  [state side]
  (let [prompt-type (get-in @state [side :prompt-state :prompt-type])]
    (or (= nil prompt-type)
        (= :run prompt-type))))

;;; Neutral actions
(defn- do-play-ability [state side eid {:keys [card ability ability-idx targets ignore-cost]}]
  (let [source {:source card
                :source-type :ability
                :source-info {:ability-idx ability-idx
                              :ability-targets targets}}
        eid (or eid (make-eid state source))
        cost (when-not ignore-cost
               (seq (card-ability-cost state side ability card targets)))
        ability (assoc ability :cost cost)]
    (when (or (nil? cost)
              (can-pay? state side eid card (:title card) cost))
      (update-history! state ability)
      (if (:action ability)
        (let [stripped-card (select-keys card [:cid :type :title])]
          (wait-for
            (trigger-event-simult state side :action-played nil {:ability-idx ability-idx :card stripped-card})
            (wait-for
              (resolve-ability state side ability card targets)
              (trigger-event-simult state side eid :action-resolved nil {:ability-idx ability-idx :card stripped-card}))))
        (resolve-ability state side eid ability card targets)))))

(defn play-ability
  "Triggers a card's ability using its zero-based index into the card's card-def :abilities vector."
  ([state side args] (play-ability state side nil args))
  ([state side eid {:keys [card] ability-idx :ability :as args}]
   (let [card (get-card state card)
         args (assoc args :card card)
         ability (nth (:abilities card) ability-idx)
         blocking-prompt? (not (no-blocking-prompt? state side))
         cannot-play (or (:disabled card)
                         ;; cannot play actions during runs
                         (and (:action ability) (:run @state))
                         ;; while resolving another ability or promppt
                         blocking-prompt?
                         (not= side (to-keyword (:side card)))
                         ;; prevention/disabling abilities
                         (any-effects state side :prevent-paid-ability true? card [ability ability-idx])
                         (some? (is-disabled-reg? state card)))]
     (when blocking-prompt?
       (toast state side "You cannot play abilities while other abilities are resolving." "warning"))
     (when-not cannot-play
       (do-play-ability state side eid (assoc args :ability-idx ability-idx :ability ability))))))

(defn expend-ability
  "Called when the player clicks a card from hand."
  [state side {:keys [card]}]
  (if (no-blocking-or-prevent-prompt? state side)
    (let [card (get-card state card)
          eid (make-eid state {:source card :source-type :ability})
          expend-ab (expend (:expend (card-def card)))]
      (do-play-ability
        state side eid
        {:card card
         :ability expend-ab
         :ability-idx 0
         :targets nil}))
    (toast state side (str "You cannot play abilities while other abilities are resolving.")
              "warning")))

(defn flashback
  "Called when the player clicks a flashback card from hand."
  [state side {:keys [card] :as context}]
  (when-let [card (get-card state card)]
    (let [flashback-cost (:flashback (card-def card))
          eid (make-eid state {:source card :source-type :ability})
          card (assoc card :rfg-instead-of-trashing true)]
      (do-play-ability
        state side eid
        {:card card
         :ability {:action true
                   :async true
                   :effect (req (play-instant state side eid (assoc card :rfg-instead-of-trashing true) {:base-cost flashback-cost :as-flashback true}))}
         :ability-idx 0
         :targets []}))))

(defn play
  "Called when the player clicks a card from hand."
  [state side {:keys [card] :as context}]
  (when-let [card (get-card state card)]
    (when (and (not (get-in @state [side :prompt-state :prompt-type]))
               (not (and (= side :corp) (:corp-phase-12 @state)))
               (not (and (= side :runner) (:runner-phase-12 @state))))
      (let [context (assoc context :card card)]
        (case (:type card)
          ("Event" "Operation")
          (play-ability state side {:card (get-in @state [side :basic-action-card])
                                    :ability 3
                                    :targets [context]})
          ("Hardware" "Resource" "Program" "ICE" "Upgrade" "Asset" "Agenda")
          (play-ability state side {:card (get-in @state [side :basic-action-card])
                                    :ability 2
                                    :targets [context]}))))))

(defn click-draw
  "Click to draw."
  [state side _]
  (play-ability state side {:card (get-in @state [side :basic-action-card])
                            :ability 1}))

(defn click-credit
  "Click to gain 1 credit."
  [state side _]
  (play-ability state side {:card (get-in @state [side :basic-action-card])
                            :ability 0}))

(defn move-card
  "Called when the user drags a card from one zone to another."
  [state side {:keys [card server]}]
  (let [c (get-card state card)
        last-zone (last (:zone c))
        src (name-zone (:side c) (:zone c))
        from-str (card-str state c)
        s (if (#{"HQ" "R&D" "Archives"} server) :corp :runner)]
    ;; allow moving from play-area always, otherwise only when same side, and to valid zone
    (when (and (not= src server)
               (same-side? s (:side card))
               (not= :select (get-in @state [side :prompt-state :prompt-type]))
               (or (= last-zone :play-area)
                   (same-side? side (:side card))))
      (let [move-card-to (partial move state s c)
            card-prompts (filter #(same-card? :title % c) (get-in @state [side :prompt]))
            log-move (fn [verb & text]
                       (system-msg state side (str verb " " from-str
                                                   (when (seq text)
                                                     (apply str " " text)))))]
        (case server
          ("Heap" "Archives")
          (do (when (pos? (count card-prompts))
                ;; Remove all prompts associated with the trashed card
                (doseq [prompt card-prompts]
                  (remove-from-prompt-queue state side prompt)
                  (effect-completed state side (:eid prompt))))
              (if (= :hand (first (:zone c)))
                ;; Discard from hand, do not trigger trash
                (do (move-card-to :discard {:force true})
                    (log-move "discards"))
                (do (trash state s (make-eid state) c {:unpreventable true})
                    (log-move "trashes"))))
          ("the Grip" "HQ")
          (do (move-card-to :hand {:force true})
              (log-move "moves" "to " server))
          ("Stack" "R&D")
          (do (move-card-to :deck {:front true :force true})
              (log-move "moves" "to the top of " server))
          ;; default
          nil)))))

(defn trash-button
  [state side eid card]
  (system-msg state side (str "trashes " (card-str state card)))
  (trash state side eid card {:unpreventable true}))

(defn- finish-prompt [state side prompt card]
  (when-let [end-effect (:end-effect prompt)]
    (end-effect state side (make-eid state) card nil))
  true)

(defn- prompt-error
  [context prompt prompt-args]
  (timbre/error (Exception. (str "Error " context
                                 "\nPrompt: " prompt
                                 "\nPrompt args: " prompt-args))))

(defn- maybe-pay
  [state side eid card choices choice]
  (if (= choices :credit)
    (pay state side eid card (->c :credit (min choice (get-in @state [side :credit]))))
    (effect-completed state side eid)))

;; TODO - resolve-prompt does some evil things with eids, maybe we can fix it later - nbk, 2025
(defn resolve-prompt
  "Resolves a prompt by invoking its effect function with the selected target of the prompt.
  Triggered by a selection of a prompt choice button in the UI."
  [state side {:keys [choice eid] :as args}]
  (let [prompt (or (first-prompt-by-eid state side eid)
                   (first (get-in @state [side :prompt])))
        prompt-eid eid
        effect (:effect prompt)
        card (get-card state (:card prompt))
        choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (= :trace (:prompt-type prompt))
          (:counter choices)
          (:number choices))
      (if (number? choice)
        (do (remove-from-prompt-queue state side prompt)
            (let [eid (make-eid state (:eid prompt))]
              (wait-for (maybe-pay state side eid card choices choice)
                        (when (:counter choices)
                          ;; :Counter prompts deduct counters from the card
                          (add-counter state side (make-eid state eid) card (:counter choices) (- choice)))
                        ;; trigger the prompt's effect function
                        (when effect
                          (effect (or choice card)))
                        (finish-prompt state side prompt card))))
        (prompt-error "in an integer prompt" prompt args))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (if (string? choice)
        (let [title-fn (:card-title choices)
              found (some #(when (= (string/lower-case choice) (string/lower-case (:title % ""))) %) (server-cards))]
          (if found
            (if (title-fn state side (make-eid state) card [found])
              (do (remove-from-prompt-queue state side prompt)
                  (when effect
                    (effect (or choice card)))
                  (finish-prompt state side prompt card))
              (toast state side (str "You cannot choose " choice " for this effect.") "warning"))
            (toast state side (str "Could not find a card named " choice ".") "warning")))
        (prompt-error "in a card-title prompt" prompt args))

      ;; Otherwise, choices is a sequence of strings and/or cards
      ;; choice is a string and should match one of the strings, or the title of one of the cards
      (:uuid choice)
      (let [uuid (uuid/as-uuid (:uuid choice))
            match (first (filter #(= uuid (:uuid %)) choices))]
        (when match
          (remove-from-prompt-queue state side prompt)
          (if (= (:value match) "Cancel")
            (do (if-let [cancel-effect (:cancel-effect prompt)]
                  ;; trigger the cancel effect
                  (cancel-effect choice)
                  (effect-completed state side (:eid prompt)))
                (finish-prompt state side prompt card))
            (do (effect match)
                (finish-prompt state side prompt card)))))

      :else
      (prompt-error "in an unknown prompt type" prompt args))))

(defn- update-first [selection target eid c]
  "This ensures that updating the selected set of cards doesn't mix up prompts (usually when the user does something silly, or the front-end/back-end are out of sync"
  (mapv (fn [s]
          (if (= (-> s :ability :eid :eid) (:eid eid))
            (update s :cards
                    (if (:selected c)
                      (fn [cards] (conj cards c))
                      (fn [cards] (remove-once #(same-card? % target) cards))))
            s))
        selection))

(defn select
  "Attempt to select the given card to satisfy the current select prompt. Calls resolve-select
  if the max number of cards has been selected."
  [state side {:keys [card shift-key-held eid] :as args}]
  (let [target (get-card state card)
        prompt (or (first-selection-by-eid state side eid)
                   (first (get-in @state [side :selected])))
        ability (:ability prompt)
        card (when (:card ability) (get-card state (:card ability)))
        card-req (:req prompt)
        card-condition (:card prompt)
        cid (:not-self prompt)]
    (swap! state assoc-in [side :shift-key-select] shift-key-held)
    (when (and (not= (:cid target) cid)
               (cond
                 card-condition (card-condition target)
                 card-req (card-req state side (:eid ability) card [target])
                 :else true))
      (let [c (update-in target [:selected] not)]
        (update! state side c)
        (swap! state update-in [side :selected] #(update-first % target eid c))
        (let [selected (or (first-selection-by-eid state side eid)
                           (first (get-in @state [side :selected])))
              prompt (or
                       (first-prompt-by-eid state side eid :select)
                       (first (filter #(= :select (:prompt-type %)) (get-in @state [side :prompt]))))]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side eid card (select-keys prompt [:cancel-effect]) update! resolve-ability)))))))

(defn play-auto-pump
  "Use the 'match strength with ice' function of icebreakers."
  [state side args]
  (let [card (get-card state (:card args))
        eid (make-eid state {:source card :source-type :ability})
        current-ice (get-current-ice state)
        can-pump (fn [ability]
                   (when (:pump ability)
                     ((:req ability) state side eid card nil)))
        [pump-ability pump-cost]
        (some->> (filter (complement :auto-pump-ignore) (:abilities (card-def card)))
                 (keep #(when (can-pump %)
                          [% (card-ability-cost state side % card current-ice)]))
                 (filter (complement :auto-pump-ignore))
                 (seq)
                 (sort-by #(-> % first :auto-pump-sort))
                 (apply min-key #(let [costs (second %)]
                                   (reduce (fnil + 0 0) 0 (keep :cost/amount costs)))))
        pump-strength (get-pump-strength state side pump-ability card)
        strength-diff (when (and current-ice
                                 (get-strength current-ice)
                                 (get-strength card))
                        (max 0 (- (get-strength current-ice)
                                  (get-strength card))))
        times-pump (if (and strength-diff
                              (pos? pump-strength))
                     (int (Math/ceil (/ strength-diff pump-strength)))
                     0)
        total-pump-cost (when (and pump-ability
                                   times-pump)
                          (repeat times-pump pump-cost))]
    (when (can-pay? state side eid card (:title card) total-pump-cost)
      (wait-for (pay state side (make-eid state eid) card total-pump-cost)
                (dotimes [_ times-pump]
                  (resolve-ability state side (dissoc pump-ability :cost :msg) (get-card state card) nil))
                (system-msg state side (str (build-spend-msg (:msg async-result) "increase")
                                            "the strength of " (:title card) " to "
                                            (get-strength (get-card state card))))
                (effect-completed state side eid)))))

(defn- play-heap-breaker-auto-pump-and-break-impl
  [state side sub-groups-to-break current-ice]
  {:async true
   :effect (req
             (let [subs-to-break (first sub-groups-to-break)
                   sub-groups-to-break (rest sub-groups-to-break)]
               (doseq [sub subs-to-break]
                 (break-subroutine! state (get-card state current-ice) sub card))
               (let [ice (get-card state current-ice)
                     on-break-subs (when ice (:on-break-subs (card-def current-ice)))
                     event-args (when on-break-subs
                                  {:card-abilities (ability-as-handler ice on-break-subs)})]
                 (wait-for (trigger-event-simult state side :subroutines-broken event-args (break-subs-event-context state ice subs-to-break card))
                           (if (empty? sub-groups-to-break)
                             (effect-completed state side eid)
                             (continue-ability state side (play-heap-breaker-auto-pump-and-break-impl state side sub-groups-to-break current-ice) card nil))))))})

(defn play-heap-breaker-auto-pump-and-break
  "Play auto-pump-and-break for heap breakers"
  [state side args]
  (let [card (get-card state (:card args))
        eid (make-eid state {:source card :source-type :ability})
        current-ice (get-current-ice state)
        ;; match strength
        can-pump (fn [ability]
                   (when (and (:heap-breaker-pump ability)
                              (not (any-effects state side :prevent-paid-ability true? card [ability])))
                     ((:req ability (req true)) state side eid card nil)))
        breaker-ability (some #(when (can-pump %) %) (:abilities (card-def card)))
        pump-strength-at-once (when breaker-ability
                                (:heap-breaker-pump breaker-ability))
        subs-broken-at-once (when breaker-ability
                              (:heap-breaker-break breaker-ability))
        strength-diff (when (and current-ice
                                 (get-strength current-ice)
                                 (get-strength card))
                        (max 0 (- (get-strength current-ice)
                                  (get-strength card))))
        unbroken-subs (count (remove :broken (:subroutines current-ice)))
        x-number (when (and strength-diff unbroken-subs)
                   (max strength-diff unbroken-subs))
        x-breaker (= :x pump-strength-at-once)
        pumps-needed (when (and strength-diff pump-strength-at-once)
                       (if x-breaker
                         1
                         (int (Math/ceil (/ strength-diff pump-strength-at-once)))))
        breaks-needed (when (and unbroken-subs subs-broken-at-once)
                        (if x-breaker
                          1
                          (int (Math/ceil (/ unbroken-subs subs-broken-at-once)))))
        ability-uses-needed (when (and pumps-needed breaks-needed)
                              (if x-breaker
                                1
                                (+ pumps-needed
                                   breaks-needed
                                   (if (pos? pumps-needed) -1 0)))) ;already broken once with last pump
        total-cost (when (and breaker-ability
                              ability-uses-needed)
                     (if x-breaker
                       [(->c :credit x-number)]
                       (repeat ability-uses-needed (:cost breaker-ability))))]
    (when (and breaker-ability
               (can-pay? state side eid card (:title card) total-cost))
      (wait-for (pay state side (make-eid state eid) card total-cost)
                (if x-breaker
                  (pump state side (get-card state card) x-number)
                  (pump state side (get-card state card) (* pump-strength-at-once ability-uses-needed)))
                (let [payment-str (:msg async-result)
                      sub-groups-to-break (if (and (number? subs-broken-at-once) (pos? subs-broken-at-once))
                                        (partition subs-broken-at-once subs-broken-at-once nil (remove :broken (:subroutines current-ice)))
                                        [(remove :broken (:subroutines current-ice))])]
                  (wait-for (resolve-ability state side (play-heap-breaker-auto-pump-and-break-impl state side sub-groups-to-break current-ice) card nil)
                            (system-msg state side
                                        (str (build-spend-msg payment-str "increase")
                                             "the strength of " (:title card)
                                             " to " (get-strength (get-card state card))
                                             " and break all subroutines on " (:title current-ice)))
                            (continue state side nil)))))))

(defn- play-auto-pump-and-break-impl
  [state side payment-eid sub-groups-to-break current-ice break-ability]
  {:async true
   :effect (req
             (let [subs-to-break (first sub-groups-to-break)
                   sub-groups-to-break (rest sub-groups-to-break)]
               (doseq [sub subs-to-break]
                 (break-subroutine! state (get-card state current-ice) sub card))
               (let [ice (get-card state current-ice)
                     on-break-subs (when ice (:on-break-subs (card-def current-ice)))
                     event-args (when on-break-subs
                                  {:card-abilities (ability-as-handler ice on-break-subs)})]
               (wait-for
                 (resolve-ability state side (make-eid state payment-eid)
                                  (:additional-ability break-ability) (get-card state card) nil)
                 (wait-for (trigger-event-simult state side :subroutines-broken event-args (break-subs-event-context state ice subs-to-break card))
                           (if (empty? sub-groups-to-break)
                             (effect-completed state side eid)
                             (continue-ability state side (play-auto-pump-and-break-impl state side payment-eid sub-groups-to-break current-ice break-ability) card nil)))))))})

(defn play-auto-pump-and-break
  "Use play-auto-pump and then break all available subroutines"
  [state side args]
  (if (some #(:heap-breaker-break %) (:abilities (card-def (get-card state (:card args)))))
    (play-heap-breaker-auto-pump-and-break state side args)
    (let [card (get-card state (:card args))
          eid (make-eid state {:source card :source-type :ability})
          current-ice (get-current-ice state)
          ;; match strength
          can-pump (fn [ability]
                     (when (:pump ability)
                       ((:req ability) state side eid card nil)))
          [pump-ability pump-cost]
          (some->> (filter (complement :auto-pump-ignore) (:abilities (card-def card)))
                   (keep #(when (can-pump %)
                            [% (card-ability-cost state side % card current-ice)]))
                   (seq)
                   (sort-by #(-> % first :auto-pump-sort))
                   (apply min-key #(let [costs (second %)]
                                     (reduce (fnil + 0 0) 0 (mapv :cost/amount costs)))))
          pump-strength (get-pump-strength state side pump-ability card)
          strength-diff (when (and current-ice
                                   (get-strength current-ice)
                                   (get-strength card))
                          (max 0 (- (get-strength current-ice)
                                    (get-strength card))))
          times-pump (if (and strength-diff
                              (pos? pump-strength))
                       (int (Math/ceil (/ strength-diff pump-strength)))
                       0)
          total-pump-cost (when (and pump-ability
                                     times-pump)
                            (repeat times-pump pump-cost))
          ;; break all subs
          can-break (fn [ability]
                      (when (and (:break-req ability)
                                 (not (any-effects state side :prevent-paid-ability true? card [ability])))
                        ((:break-req ability) state side eid card nil)))
          [break-ability break-cost]
          (some->> (:abilities (card-def card))
                   (keep #(when (can-break %)
                            [% (break-sub-ability-cost state side % card current-ice)]))
                   (seq)
                   (sort-by #(-> % first :auto-break-sort))
                   (apply min-key #(let [costs (second %)]
                                     (reduce (fnil + 0 0) 0 (mapv :cost/amount costs)))))
          once-key (:once break-ability)
          subs-broken-at-once (when break-ability
                                (:break break-ability 1))
          unbroken-subs (when (:subroutines current-ice)
                          (count (remove :broken (:subroutines current-ice))))
          some-already-broken (not= unbroken-subs (count (:subroutines current-ice)))
          times-break (when (and unbroken-subs
                                 subs-broken-at-once)
                        (if (pos? subs-broken-at-once)
                          (int (Math/ceil (/ unbroken-subs subs-broken-at-once)))
                          1))
          break-cost (substitute-x-credit-costs break-cost unbroken-subs (:auto-break-creds-per-sub break-ability))
          total-break-cost (when (and break-cost
                                      times-break)
                             (repeat times-break break-cost))
          total-cost (merge-costs (conj total-pump-cost total-break-cost))]
      (when (and break-ability
                 (can-pay? state side eid card (:title card) total-cost))
        (wait-for (pay state side (make-eid state eid) card total-cost)
                  (dotimes [_ times-pump]
                    (resolve-ability state side (dissoc pump-ability :cost :msg) (get-card state card) nil))
                  (let [payment-eid async-result
                        payment-str (:msg payment-eid)
                        sub-groups-to-break (if (pos? subs-broken-at-once)
                                              (partition subs-broken-at-once subs-broken-at-once nil (remove :broken (:subroutines current-ice)))
                                              [(remove :broken (:subroutines current-ice))])]
                    (wait-for (resolve-ability state side (play-auto-pump-and-break-impl state side payment-eid sub-groups-to-break current-ice break-ability) card nil)
                              (system-msg state side
                                          (if (pos? times-pump)
                                            (str (build-spend-msg payment-str "increase")
                                                 "the strength of " (:title card)
                                                 " to " (get-strength (get-card state card))
                                                 " and break all " (when (< 1 unbroken-subs) unbroken-subs)
                                                 " subroutines on " (:title current-ice))
                                            (str (build-spend-msg payment-str "use")
                                                 (:title card)
                                                 " to break "
                                                 (if some-already-broken
                                                   "the remaining "
                                                   "all ")
                                                 unbroken-subs " subroutines on "
                                                 (:title current-ice))))
                              (when once-key (register-once state side {:once once-key} card))
                              (continue state side nil))))))))

(def dynamic-abilities
  {"auto-pump" #'play-auto-pump
   "auto-pump-and-break" #'play-auto-pump-and-break})

(defn play-dynamic-ability
  "Triggers an ability that was dynamically added to a card's data but is not necessarily present in its
  :abilities vector."
  [state side args]
  (if (no-blocking-or-prevent-prompt? state side)
    ((dynamic-abilities (:dynamic args)) state (keyword side) args)
    (toast state side (str "You cannot play abilities while other abilities are resolving.")
           "warning")))


(defn play-corp-ability
  "Triggers a runner card's corp-ability using its zero-based index into the card's card-def :corp-abilities vector."
  ([state side args] (play-corp-ability state side nil args))
  ([state side eid {:keys [card] ability-idx :ability :as args}]
   (let [card (get-card state card)
         cdef (card-def card)
         ability (get-in cdef [:corp-abilities ability-idx])
         cannot-play (or (:disabled card)
                         (any-effects state side :prevent-paid-ability true? card [ability ability-idx]))]
     (when-not cannot-play
       (do-play-ability state side eid {:ability ability
                                        :card card
                                        :ability-idx ability-idx
                                        :targets nil})))))

(defn play-runner-ability
  "Triggers a corp card's runner-ability using its zero-based index into the card's card-def :runner-abilities vector."
  ([state side args] (play-runner-ability state side nil args))
  ([state side eid {:keys [card] ability-idx :ability :as args}]
   (let [card (get-card state card)
         cdef (card-def card)
         ability (get-in cdef [:runner-abilities ability-idx])
         cannot-play (or (:disabled card)
                         (any-effects state side :prevent-paid-ability true? card [ability ability-idx]))]
     (when-not cannot-play
       (do-play-ability state side eid (assoc args :ability-idx ability-idx :ability ability))))))

(defn play-subroutine
  "Triggers a card's subroutine using its zero-based index into the card's :subroutines vector."
  [state side {:keys [card subroutine]}]
  (if (no-blocking-or-prevent-prompt? state side)
    (let [card (get-card state card)
          sub (nth (:subroutines card) subroutine nil)]
      (when card
        (resolve-subroutine! state side card sub)))
    (toast state side (str "You cannot fire subroutines while abilities are being resolved.")
           "warning")))

(defn play-unbroken-subroutines
  "Triggers each unbroken subroutine on a card in order, waiting for each to complete"
  [state side {:keys [card]}]
  (if (no-blocking-or-prevent-prompt? state side)
    (when-let [card (get-card state card)]
      (resolve-unbroken-subs! state side card))
    (toast state side (str "You cannot fire subroutines while abilities are being resolved.")
           "warning")))

;;; Corp actions
(defn trash-resource
  "Click to trash a resource."
  [state side _]
  (play-ability state side {:card (get-in @state [:corp :basic-action-card])
                            :ability 5}))

(defn do-purge
  "Purge viruses."
  [state side _]
  (play-ability state side {:card (get-in @state [:corp :basic-action-card])
                            :ability 6}))

(defn click-advance
  "Click to advance installed card."
  [state side {:keys [card] :as context}]
  (when-let [card (get-card state card)]
    (let [context (assoc context :card card)]
      ;; note that can-advance potentially generates toasts (effcom),
      ;; so it cannot go in the req of basic, since that can generate infinite toast loops
      ;; when update-and-send-diffs causes more updates that need to be updated and sent...
      (if (can-advance? state side card)
        (play-ability state side {:card (get-in @state [:corp :basic-action-card])
                                  :ability 4
                                  :targets [context]})
        (toast state :corp "Cannot advance cards this turn." "warning")))))

;;; Runner actions
(defn click-run
  "Click to start a run."
  [state side context]
  (play-ability state side {:card (get-in @state [:runner :basic-action-card])
                            :ability 4
                            :targets [context]}))

(defn remove-tag
  "Click to remove a tag."
  [state side _]
  (play-ability state side {:card (get-in @state [:runner :basic-action-card])
                            :ability 5}))

(defn view-deck
  "Allows the player to view their deck by making the cards in the deck public."
  [state side _]
  (system-msg state side "looks at [their] deck")
  (swap! state assoc-in [side :view-deck] true))

(defn close-deck
  "Closes the deck view and makes cards in deck private again."
  [state side _]
  (system-msg state side "stops looking at [their] deck")
  (swap! state update-in [side] dissoc :view-deck))

(defn generate-install-list
  [state _ {:keys [card]}]
  (if-let [card (get-card state card)]
    (if (expendable? state card)
      (swap! state assoc-in [:corp :install-list] (conj (installable-servers state card) "Expend"))
      (swap! state assoc-in [:corp :install-list] (installable-servers state card)))
    (swap! state dissoc-in [:corp :install-list])))

(defn generate-runnable-zones
  [state _ _]
  (swap! state assoc-in [:runner :runnable-list] (zones->sorted-names (get-runnable-zones state))))

(defn advance
  "Advance a corp card that can be advanced.
   If you pass in a truthy value as the no-cost parameter, it will advance at no cost (for the card Success)."
  ([state side {:keys [card]}] (advance state side (make-eid state) card nil))
  ([state side card no-cost] (advance state side (make-eid state) card no-cost))
  ([state side eid card no-cost]
   (let [card (get-card state card)
         eid (assoc eid :source-type :advance)]
     (if (can-advance? state side card)
       (wait-for (pay state side
                      (make-eid state (assoc eid :action :corp-advance))
                      card
                      (->c :click (if-not no-cost 1 0))
                      (->c :credit (if-not no-cost 1 0)))
                 (if-let [payment-str (:msg async-result)]
                   (do (system-msg state side (str (build-spend-msg payment-str "advance") (card-str state card)))
                       (update-advancement-requirement state card)
                       (wait-for
                         (add-prop state side (get-card state card) :advance-counter 1)
                         (play-sfx state side "click-advance")
                         (effect-completed state side eid)))
                   (effect-completed state side eid)))
       (effect-completed state side eid)))))

(defn resolve-score
  "resolves the actual 'scoring' of an agenda (after costs/can-steal has been worked out)"
  [state side eid card {:keys [advancement-tokens advancement-requirement]}]
  (let [moved-card (move state :corp card :scored)
        c (card-init state :corp moved-card {:resolve-effect false
                                             :init-data true})
        _ (update-all-advancement-requirements state)
        _ (update-all-agenda-points state)
        c (get-card state c)
        points (get-agenda-points c)]
    (system-msg state :corp (str "scores " (:title c)
                                 " and gains " (quantify points "agenda point")))
    (implementation-msg state card)
    (set-prop state :corp (get-card state c) :advance-counter 0)
    (swap! state update-in [:corp :register :scored-agenda] #(+ (or % 0) points))
    (play-sfx state side "agenda-score")
    (when-let [on-score (:on-score (card-def c))]
      (register-pending-event state :agenda-scored c on-score))
    (queue-event state :agenda-scored {:card c
                                       :advancement-requirement advancement-requirement
                                       :advancement-tokens advancement-tokens
                                       :points points})
    (checkpoint state nil eid {:duration :agenda-scored})))

(defn score
  "Score an agenda."
  ([state side eid card] (score state side eid card nil))
  ([state side eid card {:keys [no-req ignore-turn ignore-adv]}]
   (if-not (can-score? state side card {:no-req no-req :ignore-turn ignore-turn :ignore-adv ignore-adv})
     (effect-completed state side eid)
     (let [cost (score-additional-cost-bonus state side card)
           adv-cost (if (or no-req ignore-adv)
                      0
                      (get-advancement-requirement card))
           adv-tokens (get-counters card :advancement)
           cost-strs (build-cost-string cost)
           can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs cost)) card (:title card) cost)]
       (cond
         (string/blank? cost-strs) (resolve-score state side eid card {:advancement-requirement adv-cost :advancement-tokens adv-tokens})
         (not can-pay) (effect-completed state side eid)
         :else (wait-for (pay state side (make-eid state
                                                   (assoc eid
                                                          :additional-costs cost
                                                          :source card
                                                          :source-type :corp-score))
                              card cost)
                         (let [payment-result async-result]
                           (if (string/blank? (:msg payment-result))
                             (effect-completed state side eid)
                             (do
                               (system-msg state side (str (:msg payment-result) " to score " (:title card)))
                               (resolve-score state side eid card {:advancement-requirement adv-cost :advancement-tokens adv-tokens}))))))))))
