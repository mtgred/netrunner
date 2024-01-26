(ns game.core.actions
  (:require
    [clj-uuid :as uuid]
    [clojure.stacktrace :refer [print-stack-trace]]
    [clojure.string :as string]
    [game.core.agendas :refer [update-advancement-requirement update-all-advancement-requirements update-all-agenda-points]]
    [game.core.board :refer [get-zones installable-servers]]
    [game.core.card :refer [get-agenda-points get-card]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [break-sub-ability-cost card-ability-cost score-additional-cost-bonus]]
    [game.core.effects :refer [any-effects]]
    [game.core.eid :refer [effect-completed eid-set-defaults make-eid]]
    [game.core.engine :refer [ability-as-handler checkpoint register-pending-event pay queue-event resolve-ability trigger-event-simult]]
    [game.core.flags :refer [can-advance? can-score?]]
    [game.core.ice :refer [break-subroutine! get-current-ice get-pump-strength get-strength pump resolve-subroutine! resolve-unbroken-subs!]]
    [game.core.initializing :refer [card-init]]
    [game.core.moving :refer [move trash]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs build-cost-string]]
    [game.core.expend :refer [expend]]
    [game.core.prompt-state :refer [remove-from-prompt-queue]]
    [game.core.prompts :refer [resolve-select]]
    [game.core.props :refer [add-counter add-prop set-prop]]
    [game.core.runs :refer [can-run-server? continue get-runnable-zones total-run-cost]]
    [game.core.say :refer [play-sfx system-msg implementation-msg]]
    [game.core.servers :refer [name-zone unknown->kw zones->sorted-names]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [dissoc-in quantify remove-once same-card? same-side? server-cards]]))

;;; Neutral actions
(defn- do-play-ability [state side card ability ability-idx targets]
  (let [cost (seq (card-ability-cost state side ability card targets))]
    (when (or (nil? cost)
              (can-pay? state side (make-eid state {:source card :source-type :ability :source-info {:ability-idx ability-idx :ability-targets targets}}) card (:title card) cost))
      (let [eid (make-eid state {:source card :source-type :ability :source-info {:ability-idx ability-idx :ability-targets targets}})]
        (resolve-ability state side eid (assoc ability :cost cost) card targets)))))

(defn play-ability
  "Triggers a card's ability using its zero-based index into the card's card-def :abilities vector."
  [state side {:keys [card ability targets]}]
  (let [card (get-card state card)
        abilities (:abilities card)
        ab (nth abilities ability)
        cannot-play (or (:disabled card)
                        (any-effects state side :prevent-paid-ability true? card [ab ability]))]
    (when-not cannot-play
      (do-play-ability state side card ab ability targets))))

(defn expend-ability
  "Called when the player clicks a card from hand."
  [state side {:keys [card]}]
  (let [card (get-card state card)
        eid (make-eid state {:source card :source-type :ability})
        expend-ab (expend (:expend card))]
    (resolve-ability state side eid expend-ab card nil)))

(defn play
  "Called when the player clicks a card from hand."
  [state side {:keys [card server]}]
  (when-let [card (get-card state card)]
    (case (:type card)
      ("Event" "Operation")
      (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 3 :targets [card]})
      ("Hardware" "Resource" "Program" "ICE" "Upgrade" "Asset" "Agenda")
      (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 2 :targets [card server]}))))

(defn click-draw
  "Click to draw."
  [state side _]
  (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 1}))

(defn click-credit
  "Click to gain 1 credit."
  [state side _]
  (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 0}))

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
          ("Grip" "HQ")
          (do (move-card-to :hand {:force true})
              (log-move "moves" "to " server))
          ("Stack" "R&D")
          (do (move-card-to :deck {:front true :force true})
              (log-move "moves" "to the top of " server))
          ;; default
          nil)))))

(defn- finish-prompt [state side prompt card]
  (when-let [end-effect (:end-effect prompt)]
    (end-effect state side (make-eid state) card nil))
  true)

(defn- prompt-error
  [context prompt prompt-args]
  (.println *err* (with-out-str (print-stack-trace (Exception. (str "Error " context)))))
  (.println *err* (str "Prompt: " prompt))
  (.println *err* (str "Prompt args: " prompt-args)))

(defn- maybe-pay
  [state side eid card choices choice]
  (if (= choices :credit)
    (pay state side eid card :credit (min choice (get-in @state [side :credit])))
    (effect-completed state side eid)))

(defn resolve-prompt
  "Resolves a prompt by invoking its effect function with the selected target of the prompt.
  Triggered by a selection of a prompt choice button in the UI."
  [state side {:keys [choice] :as args}]
  (let [prompt (first (get-in @state [side :prompt]))
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
                          (add-counter state side card (:counter choices) (- choice)))
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

(defn select
  "Attempt to select the given card to satisfy the current select prompt. Calls resolve-select
  if the max number of cards has been selected."
  [state side {:keys [card]}]
  (let [target (get-card state card)
        prompt (first (get-in @state [side :selected]))
        ability (:ability prompt)
        card-req (:req prompt)
        card-condition (:card prompt)
        cid (:not-self prompt)]
    (when (and (not= (:cid target) cid)
               (cond
                 card-condition (card-condition target)
                 card-req (card-req state side (:eid ability) (get-card state (:card ability)) [target])
                 :else true))
      (let [c (update-in target [:selected] not)]
        (update! state side c)
        (if (:selected c)
          (swap! state update-in [side :selected 0 :cards] #(conj % c))
          (swap! state update-in [side :selected 0 :cards]
                 (fn [coll] (remove-once #(same-card? % target) coll))))
        (let [selected (get-in @state [side :selected 0])
              prompt (first (get-in @state [side :prompt]))
              card (:card prompt)]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side card (select-keys prompt [:cancel-effect]) update! resolve-ability)))))))

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
        (some->> (:abilities (card-def card))
                 (keep #(when (can-pump %)
                          [% (:cost %)]))
                 (seq)
                 (sort-by #(-> % first :auto-pump-sort))
                 (apply min-key #(let [costs (second %)]
                                   (reduce (fnil + 0 0) 0 (mapv second costs)))))
        cost-req (or (:cost-req pump-ability) identity)
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
                          (repeat times-pump (cost-req [pump-cost])))]
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
                 (wait-for (trigger-event-simult state side :subroutines-broken event-args ice subs-to-break)
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
                       [:credit x-number]
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
  [state side sub-groups-to-break current-ice break-ability]
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
                 (resolve-ability state side (make-eid state {:source card :source-type :ability})
                                  (:additional-ability break-ability) (get-card state card) nil)
                 (wait-for (trigger-event-simult state side :subroutines-broken event-args ice subs-to-break)
                           (if (empty? sub-groups-to-break)
                             (effect-completed state side eid)
                             (continue-ability state side (play-auto-pump-and-break-impl state side sub-groups-to-break current-ice break-ability) card nil)))))))})

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
          (some->> (:abilities (card-def card))
                   (keep #(when (can-pump %)
                            [% (:cost %)]))
                   (seq)
                   (sort-by #(-> % first :auto-pump-sort))
                   (apply min-key #(let [costs (second %)]
                                     (reduce (fnil + 0 0) 0 (mapv second costs)))))
          pump-cost-req (or (:cost-req pump-ability) identity)
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
                            (repeat times-pump (pump-cost-req [pump-cost])))
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
                                     (reduce (fnil + 0 0) 0 (mapv second costs)))))
          break-cost-req (or (:cost-req break-ability) identity)
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
          total-break-cost (when (and break-cost
                                      times-break)
                             (repeat times-break (break-cost-req [break-cost])))
          total-cost (merge-costs (conj total-pump-cost total-break-cost))]
      (when (and break-ability
                 (can-pay? state side eid card (:title card) total-cost))
        (wait-for (pay state side (make-eid state eid) card total-cost)
                  (dotimes [_ times-pump]
                    (resolve-ability state side (dissoc pump-ability :cost :msg) (get-card state card) nil))
                  (let [payment-str (:msg async-result)
                        sub-groups-to-break (if (pos? subs-broken-at-once)
                                              (partition subs-broken-at-once subs-broken-at-once nil (remove :broken (:subroutines current-ice)))
                                              [(remove :broken (:subroutines current-ice))])]
                    (wait-for (resolve-ability state side (play-auto-pump-and-break-impl state side sub-groups-to-break current-ice break-ability) card nil)
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
                              (continue state side nil))))))))

(def dynamic-abilities
  {"auto-pump" #'play-auto-pump
   "auto-pump-and-break" #'play-auto-pump-and-break})

(defn play-dynamic-ability
  "Triggers an ability that was dynamically added to a card's data but is not necessarily present in its
  :abilities vector."
  [state side args]
  ((dynamic-abilities (:dynamic args)) state (keyword side) args))

(defn play-corp-ability
  "Triggers a runner card's corp-ability using its zero-based index into the card's card-def :corp-abilities vector."
  [state side {:keys [card ability targets]}]
  (let [card (get-card state card)
        cdef (card-def card)
        ab (get-in cdef [:corp-abilities ability])
        cannot-play (or (:disabled card)
                        (any-effects state side :prevent-paid-ability true? card [ab ability]))]
    (when-not cannot-play
      (do-play-ability state side card ab ability targets))))

(defn play-runner-ability
  "Triggers a corp card's runner-ability using its zero-based index into the card's card-def :runner-abilities vector."
  [state side {:keys [card ability targets]}]
  (let [card (get-card state card)
        cdef (card-def card)
        ab (get-in cdef [:runner-abilities ability])
        cannot-play (or (:disabled card)
                        (any-effects state side :prevent-paid-ability true? card [ab ability]))]
    (when-not cannot-play
      (do-play-ability state side card ab ability targets))))

(defn play-subroutine
  "Triggers a card's subroutine using its zero-based index into the card's :subroutines vector."
  [state side {:keys [card subroutine]}]
  (let [card (get-card state card)
        sub (nth (:subroutines card) subroutine nil)]
    (when card
      (resolve-subroutine! state side card sub))))

(defn play-unbroken-subroutines
  "Triggers each unbroken subroutine on a card in order, waiting for each to complete"
  [state side {:keys [card]}]
  (let [card (get-card state card)]
    (when card
      (resolve-unbroken-subs! state side card))))

;;; Corp actions
(defn trash-resource
  "Click to trash a resource."
  [state side _]
  (play-ability state side {:card (get-in @state [:corp :basic-action-card]) :ability 5}))

(defn do-purge
  "Purge viruses."
  [state side _]
  (play-ability state side {:card (get-in @state [:corp :basic-action-card]) :ability 6}))

(defn click-advance
  "Click to advance installed card."
  [state side {:keys [card]}]
  (when-let [card (get-card state card)]
    (play-ability state side {:card (get-in @state [:corp :basic-action-card]) :ability 4 :targets [card]})))

;;; Runner actions
(defn click-run
  "Click to start a run."
  [state side {:keys [server]}]
  (play-ability state side {:card (get-in @state [:runner :basic-action-card]) :ability 4 :targets [server]}))

(defn remove-tag
  "Click to remove a tag."
  [state side _]
  (play-ability state side {:card (get-in @state [:runner :basic-action-card]) :ability 5}))

(defn view-deck
  "Allows the player to view their deck by making the cards in the deck public."
  [state side _]
  (system-msg state side "looks at their deck")
  (swap! state assoc-in [side :view-deck] true))

(defn close-deck
  "Closes the deck view and makes cards in deck private again."
  [state side _]
  (system-msg state side "stops looking at their deck")
  (swap! state update-in [side] dissoc :view-deck))

(defn generate-install-list
  [state _ {:keys [card]}]
  (let [card (get-card state card)]
    (if card
      (if (:expend card)
        (swap! state assoc-in [:corp :install-list] (conj (installable-servers state card) "Expend")) ;;april fools we can make this "cast as a sorcery"
        (swap! state assoc-in [:corp :install-list] (installable-servers state card)))
      (swap! state dissoc-in [:corp :install-list]))))

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
         eid (eid-set-defaults eid :source nil :source-type :advance)]
     (if (can-advance? state side card)
       (wait-for (pay state side (make-eid state eid) card :click (if-not no-cost 1 0) :credit (if-not no-cost 1 0) {:action :corp-advance})
                 (if-let [payment-str (:msg async-result)]
                   (do (system-msg state side (str (build-spend-msg payment-str "advance") (card-str state card)))
                       (update-advancement-requirement state card)
                       (add-prop state side (get-card state card) :advance-counter 1)
                       (play-sfx state side "click-advance")
                       (effect-completed state side eid))
                   (effect-completed state side eid)))
       (effect-completed state side eid)))))

(defn resolve-score
  "resolves the actual 'scoring' of an agenda (after costs/can-steal has been worked out)"
  [state side eid card]
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
                                       :points points})
    (checkpoint state nil eid {:duration :agenda-scored})))

(defn score
  "Score an agenda."
  ([state side eid card] (score state side eid card nil))
  ([state side eid card {:keys [no-req]}]
   (if-not (can-score? state side card {:no-req no-req})
     (effect-completed state side eid)
     (let [additional-costs (score-additional-cost-bonus state side card)
           cost (merge-costs (mapv first additional-costs))
           cost-strs (build-cost-string cost)
           can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs additional-costs)) card (:title card) cost)]
       (cond
         (string/blank? cost-strs) (resolve-score state side eid card)
         (not can-pay) (effect-completed state side eid)
         :else (wait-for (pay state side (make-eid state
                                                   (assoc eid :additional-costs additional-costs :source card :source-type :corp-score))
                              nil cost 0)
                         (let [payment-result async-result]
                           (if (string/blank? (:msg payment-result))
                             (effect-completed state side eid)
                             (do
                               (system-msg state side (str (:msg payment-result) " to score " (:title card)))
                               (resolve-score state side eid card))))))))))
