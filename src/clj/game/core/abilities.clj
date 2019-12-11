(in-ns 'game.core)

(declare any-flag-fn? init-trace optional-ability
         do-choices do-ability psi-game resolve-ability-eid resolve-psi resolve-trace
         check-optional check-psi check-trace check-prompt check-ability)

;;;; Functions for implementing card abilities and prompts

;;; Ability related function
(defn should-trigger?
  "Checks if the specified ability definition should trigger.
  Checks for a :req, either in the top level map, or in an :optional or :psi sub-map
  Returns true if no :req found, returns nil if the supplied ability is nil"
  ([state side card targets ability]
    (should-trigger? state side (make-eid state) card targets ability))
  ([state side eid card targets {:keys [req optional psi trace] :as ability}]
   (when ability
     (let [partial-should-trigger? (partial should-trigger? state side eid card targets)]
       (cond
         req (req state side eid card targets)
         optional (partial-should-trigger? optional)
         psi (partial-should-trigger? psi)
         trace (partial-should-trigger? trace)
         :else true)))))

(defn not-used-once?
  [state {:keys [once once-key] :as ability} {:keys [cid] :as card}]
  (not (get-in @state [once (or once-key cid)])))

(defn can-trigger?
  "Checks if ability can trigger. Checks that once-per-turn is not violated."
  [state side ability card targets]
  (and (not-used-once? state ability card)
       (should-trigger? state side card targets ability)))

(defn is-ability?
  "Checks to see if a given map represents a card ability. Looks for :effect, :optional, :trace, or :psi."
  [{:keys [effect optional trace psi]}]
  (or effect optional trace psi))

(defn resolve-ability
  "Resolves an ability defined by the given ability map. Checks :req functions; shows prompts, psi games,
  traces, etc. as needed; charges costs; invokes :effect functions. All card effects and most engine effects
  should run through this method.

  All keys that can be used in an ability map:
  COMMON KEYS
  :req -- a 4-argument function that must return true or false. If false, the ability will not be resolved.
  :cost -- a vector of costs to charge, for example [:credit 1 :click 1]. If the costs cannot be paid, the ability
           will not be resolved.
  :msg -- either a string or a 4-argument function that returns a string. Prints to the log when the ability is finished.
  :effect -- a 4-argument function that will be called if the ability will be resolved (if :req is true or not present,
             costs can be paid, and any prompts are resolved.)
  :player -- manually specifies which player the ability should affect, rather than leave it implicit.

  PROMPT KEYS
  :choices -- this key signals a prompt of some kind. Can be:
      * a 4-argument function returning a vector of cards or strings -- user chooses one option.
        Called a 'choice' prompt.
      * the keyword :credit -- user chooses an integer up to their current credit amount.
      * the keyword :counter -- user chooses an integer up to the :counter value of the given card.
      * a map containing the keyword :number with a value of a 4-argument function returning an integer -- user
        chooses an integer up to the value of the map.
      * a map containing either: 1) the keyword :card with a value of a 1-argument function returning true or false,
        or 2) the keyword :req with a value of the req 5-fn returning true or false. Triggers a 'select' prompt
        with targeting cursor; only cards that cause the 1-argument function to return true will be allowed.
  :prompt -- a string or 4-argument function returning a string to display in the prompt menu.
  :priority -- a numeric value, or true (equivalent to 1). Prompts are inserted into the prompt queue and sorted base
               on priority, with higher priorities coming first. The sort is stable, so if two prompts have the same
               priority, the prompt that was inserted first will remain first after the sort. You should rarely need
               to use a priority larger than 1.
  :not-distinct -- true if the prompt should not collapse :choices entries of the same string to one button.
                   Defaults to false.
  :cancel-effect -- if the prompt uses a Cancel button, this 4-argument function will be called if the user
                    chooses Cancel.

  OTHER PROMPTS
  :psi -- a map that starts a psi game. Has keys :equal and :not-equal, whose values are 4-argument functions
          which are triggered when the game resolves.
  :trace -- a map that starts a trace. Has a :base key that is either an integer or a 4-argument function
            returning an integer for the base strength. The map is otherwise a normal ability map that can
            contain :req, :effect, or any other key in an ability; the :effect is only triggered if the trace
            succeeds. Can also have a key :kicker that is an ability map with key :min, whose effect triggers
            if the trace strength matches or exceeds the :min value. (Constellation ice.)

  SIMULTANEOUS EFFECT RESOLUTION KEYS
  :interactive -- when simultaneous effect resolution has been enabled for a specific event, the user receives
                  a menu of cards that handle the effect and get to choose the order of their resolution. This menu is
                  only shown if at least one ability handling the event has an :interactive function that returns true.
                  If none are interactive, then all handlers will be resolved automatically, one at a time in an
                  arbitrary order. In general, handlers should be marked ':interactive (req true)' if they have
                  important order-of-effect interactions with other cards. The :interactive function can be coded to
                  have smarter logic if necessary -- see Replicator, which is only interactive if there is another
                  copy of the installed card remaining in the Stack.
  :silent -- any handler that does not require user interaction under any circumstances can be marked :silent. If a
             handler's :silent function returns true, then no menu entry will be shown for the handler. In that case,
             the ability will only be resolved once all non-silent abilities are resolved. Example: AstroScript has no
             important interactions with other 'agenda scored' effects, and doesn't care when an agenda token is placed.
             Example: Hayley Kaplan will not show a prompt if there are no valid targets in the grip.

  OTHER KEYS
  :once -- its only value is :per-turn; signifies an effect that can only be triggered once per turn.
  :once-key -- by default, each :once is distinct per card. If multiple copies of a card can only resolve
               some ability once between all of them, then the card should specify a manual :once-key that can
               be any value, preferrably a unique keyword.
  :optional -- shows a 'Yes/No' prompt to let the user decide whether to resolve the ability.
  :makes-run -- indicates if the ability makes a run.
  :install-req -- a function which returns a list of servers a card may be installed into"
  ;; perhaps the most important function in the game logic
  ([state side {:keys [eid] :as ability} card targets]
   (resolve-ability state side (or eid (make-eid state {:source card :source-type :ability})) ability card targets))
  ([state side eid ability card targets]
   (resolve-ability-eid state side (assoc ability :eid eid :source card :source-type :ability) card targets)))

(defn- resolve-ability-eid
  [state side {:keys [eid optional psi trace choices] :as ability} card targets]
  (cond
    ;; Only has the eid, in effect a nil ability
    (and eid (= 1 (count ability)))
    (effect-completed state side eid)
    ;; This was called directly without an eid present
    (and ability (not eid))
    (resolve-ability-eid state side (assoc ability :eid (make-eid state eid)) card targets)
    ;; Both ability and eid are present, so we're good to go
    (and ability eid)
    (cond
      ;; Is this an optional ability?
      optional (check-optional state side ability card targets)
      ;; Is this a psi game?
      psi (check-psi state side ability card targets)
      ;; Is this a trace?
      trace (check-trace state side ability card targets)
      ;; Is this a prompt?
      choices (check-prompt state side ability card targets)
      ;; Just a normal ability
      :else (check-ability state side ability card targets))
    ;; Something has gone terribly wrong, error out
    :else
    (.println *err* (with-out-str
                      (print-stack-trace
                        (Exception. (str "Ability is nil????" ability card targets))
                        2500)))))

;;; Checking functions for resolve-ability
(defn- check-optional
  "Checks if there is an optional ability to resolve"
  [state side {:keys [eid optional] :as ability} card targets]
  (if (can-trigger? state side optional card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :optional)
          (assoc :async true
                 :effect (req (optional-ability state (or (:player optional) side) eid card (:prompt optional) optional targets))))
      card targets)
    (effect-completed state side eid)))

(defn- check-psi
  "Checks if a psi-game is to be resolved"
  [state side {:keys [eid psi] :as ability} card targets]
  (if (can-trigger? state side psi card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :psi)
          (assoc :async true
                 :effect (effect (psi-game eid card psi))))
      card targets)
    (effect-completed state side eid)))

(defn- check-trace
  "Checks if there is a trace to resolve"
  [state side {:keys [eid trace] :as ability} card targets]
  (if (can-trigger? state side ability card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :trace)
          (assoc :async true
                 :effect (effect (init-trace eid card trace))))
      card targets)
    (effect-completed state side eid)))

(defn- check-prompt
  [state side {:keys [eid] :as ability} card targets]
  (if (can-trigger? state side ability card targets)
    (do-choices state side ability card targets)
    (effect-completed state side eid)))

(defn- check-ability
  [state side {:keys [eid choices optional psi trace async] :as ability} card targets]
  (cond
    ;; Can't trigger, so complete the eid and exit
    (not (can-trigger? state side ability card targets))
    (effect-completed state side eid)
    ;; Ability is async, so let it complete itself
    (or async choices optional psi trace)
    (do-ability state side ability card targets)
    ;; Ability isn't async, so we have to complete it outselves
    :else
    (do (do-ability state side ability card targets)
        (effect-completed state side eid))))

(defn- do-choices
  "Handle a choices ability"
  [state side {:keys [cancel-effect choices eid end-effect not-distinct player priority
                      prompt prompt-type show-discard] :as ability}
   card targets]
  (let [s (or player side)
        ab (dissoc ability :choices)
        args {:priority priority :cancel-effect cancel-effect :prompt-type prompt-type
              :show-discard show-discard :end-effect end-effect}]
   (if (map? choices)
     ;; Two types of choices use maps: select prompts, and :number prompts.
     (cond
       ;; a counter prompt
       (:counter choices)
       (prompt! state s card prompt choices ab args)
       ;; a select prompt
       (or (:req choices)
           (:card choices))
       (show-select state s card ability update! resolve-ability args)
       ;; a :number prompt
       (:number choices)
       (let [n ((:number choices) state side eid card targets)
             d (if-let [dfunc (:default choices)]
                 (dfunc state side (make-eid state eid) card targets)
                 0)]
         (prompt! state s card prompt {:number n :default d} ab args))
       (:card-title choices)
       (let [card-titles (sort (map :title (filter #((:card-title choices) state side (make-eid state eid) nil [%])
                                                   (server-cards))))
             choices (assoc choices :autocomplete card-titles)
             args (assoc args :prompt-type :card-title)]
         (prompt! state s card prompt choices ab args))
       ;; unknown choice
       :else nil)
     ;; Not a map; either :credit, :counter, or a vector of cards or strings.
     (let [cs (if-not (fn? choices)
                choices ; :credit or :counter
                (let [cards (choices state side eid card targets)] ; a vector of cards or strings
                  (if not-distinct cards (distinct-by :title cards))))]
       (prompt! state s card prompt cs ab args)))))

(defn- print-msg
  "Prints the ability message"
  [state side {:keys [eid] :as ability} card targets cost-str]
  (when-let [message (:msg ability)]
    (let [desc (if (string? message) message (message state side eid card targets))
          cost-spend-msg (build-spend-msg cost-str "use")]
      (system-msg state (to-keyword (:side card))
                  (str cost-spend-msg (:title card) (str " to " desc))))))

(defn register-once
  "Register ability as having happened if :once specified"
  [state {:keys [once once-key] :as ability} {:keys [cid] :as card}]
  (when once (swap! state assoc-in [once (or once-key cid)] true)))

(defn- do-effect
  "Trigger the effect"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [ability-effect (:effect ability)]
    (ability-effect state side eid card targets)))

(defn- ugly-counter-hack
  "This is brought over from the old do-ability because using `get-card` or `find-latest`
  currently doesn't work properly with `pay-counters`"
  [card cost]
  ;; TODO: Remove me some day
  (let [[counter-type counter-amount] (first (filter #(some #{:advancement :agenda :power :virus} %) (partition 2 cost)))]
    (if counter-type
      (let [counter (if (= :advancement counter-type)
                      [:advance-counter]
                      [:counter counter-type])]
        (update-in card counter - counter-amount))
      card)))

(defn- do-ability
  "Perform the ability, checking all costs can be paid etc."
  [state side {:keys [eid cost] :as ability} card targets]
  ;; Ensure that any costs can be paid
  (wait-for (pay-sync state side (make-eid state eid) card cost {:action (:cid card)})
            (when-let [cost-str async-result]
              ;; Print the message
              (print-msg state side ability card targets cost-str)
              ;; Trigger the effect
              (register-once state ability card)
              (do-effect state side ability (ugly-counter-hack card cost) targets))))

(defn active-prompt?
  "Checks if this card has an active prompt"
  [state side card]
  (some #(when (same-card? card (:card %)) %)
        (flatten (map #(-> @state % :prompt) [side (other-side side)]))))

;;; Optional Ability
(defn optional-ability
  "Shows a 'Yes/No' prompt and resolves the given ability's :yes-ability if Yes is chosen, and :no-ability otherwise.
  If ability has an :autoresolve entry, first call it as a 5-function, and if it returns 'Yes' or 'No'
  resolve the ability as if prompt was displayed and Yes/No was chosen."
  ([state side card message ability targets] (optional-ability state side (make-eid state) card message ability targets))
  ([state side eid card message ability targets]
   (letfn [(prompt-fn [prompt-choice]
             (let [yes-ability (:yes-ability ability)
                   no-ability (:no-ability ability)
                   end-effect (:end-effect ability)]
               (if (and (= prompt-choice "Yes")
                        yes-ability
                        (can-pay? state side eid card (:title card) (:cost yes-ability)))
                 (resolve-ability state side (assoc yes-ability :eid eid) card targets)
                 (if no-ability
                   (resolve-ability state side (assoc no-ability :eid eid) card targets)
                   (effect-completed state side eid)))
               (if end-effect
                 (end-effect state side eid card nil))))]
     (let [autoresolve-fn     (:autoresolve ability)
           autoresolve-answer (when autoresolve-fn
                                (autoresolve-fn state side eid card targets))]
       (case autoresolve-answer
         "Yes" (prompt-fn "Yes")
         "No" (prompt-fn "No")
         (do (when autoresolve-fn
               (toast state side (str "This prompt can be skipped by clicking "
                                      (:title card) " and toggling autoresolve")))
             (show-prompt state side eid card message ["Yes" "No"]
                          prompt-fn ability)))))))

;;; Prompts
(defn prompt!
  "Shows a prompt with the given message and choices. The given ability will be resolved
  when the user resolves the prompt. Cards should generally not call this function directly; they
  should use resolve-ability to resolve a map containing prompt data.

  choices can be:
  1. a vector of strings -- shows one prompt button for each string; the selected option is passed to the
        ability as target.
  2. a vector of cards -- shows one button for each card's title; the selected card is passed as target.
  3. the keyword :credit -- shows a numeric selection box with a max value equal to the player's credits.
  4. the keyword :counter -- shows a numeric selection box with a max value equal to the :counter of the card.
  5. a map with keyword :number -- shows a numeric selection box with max value equal to the :number of the map."
  ([state side card message choices ability] (prompt! state side card message choices ability nil))
  ([state side card message choices ability args]
   (letfn [(wrap-function [args kw]
            (let [f (kw args)] (if f (assoc args kw #(f state side (:eid ability) card [%])) args)))]
     (show-prompt state side (:eid ability) card message choices #(resolve-ability state side ability card [%])
       (wrap-function args :cancel-effect)))))

;;; Psi games
(defn psi-game
  "Starts a psi game by showing the psi prompt to both players. psi is a map containing
  :equal and :not-equal abilities which will be triggered in resolve-psi accordingly."
  ([state side card psi] (psi-game state side (make-eid state {:source-type :psi}) card psi))
  ([state side eid card psi]
   (swap! state assoc :psi {})
   (register-once state psi card)
   (let [eid (assoc eid :source-type :psi)]
     (doseq [s [:corp :runner]]
       (let [all-amounts (range (min 3 (inc (total-available-credits state s eid card))))
             valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                        (any-flag-fn? state :runner :prevent-secretly-spend %))
                                   all-amounts)]
         (show-prompt-with-dice state s card (str "Choose an amount to spend for " (:title card))
                                (map #(str % " [Credits]") valid-amounts)
                                #(resolve-psi state s eid card psi (str->int (first (split % #" "))))
                                {:priority 2
                                 :prompt-type :psi}))))))

(defn resolve-psi
  "Resolves a psi game by charging credits to both sides and invoking the appropriate
  resolution ability."
  [state side eid card psi bet]
  (swap! state assoc-in [:psi side] bet)
  (let [opponent (if (= side :corp) :runner :corp)]
    (if-let [opponent-bet (get-in @state [:psi opponent])]
      (wait-for
        (pay-sync state opponent (make-eid state eid) card [:credit opponent-bet])
        (system-msg state opponent async-result)
        (wait-for
          (pay-sync state side (make-eid state eid) card [:credit bet])
          (system-msg state side async-result)
          (clear-wait-prompt state opponent)
          (wait-for (trigger-event-simult state side (make-eid state eid) :reveal-spent-credits nil (get-in @state [:psi :corp]) (get-in @state [:psi :runner]))
                    (if-let [ability (if (= bet opponent-bet) (:equal psi) (:not-equal psi))]
                      (let [card-side (if (corp? card) :corp :runner)]
                        (continue-ability state card-side (assoc ability :async true) card nil))
                      (effect-completed state side eid)))))
      (show-wait-prompt
        state side (str (string/capitalize (name opponent)) " to choose psi game credits")))))


;;; Traces
(defn init-trace-bonus
  "Applies a bonus base strength of n to the next trace attempt."
  [state side n]
  (swap! state update-in [:bonus :trace] (fnil #(+ % n) 0)))

(defn determine-initiator
  [state {:keys [player] :as trace}]
  (let [constant-effect (get-in @state [:trace :player])]
    (cond
      (some? constant-effect) constant-effect
      (some? player) player
      :else :corp)))

(defn corp-start?
  [trace]
  (= :corp (:player trace)))

(defn resolve-trace
  "Compares trace strength and link strength and triggers the appropriate effects."
  [state side eid card {:keys [player other base bonus link priority ability strength] :as trace} boost]
  (let [corp-strength (if (corp-start? trace)
                        strength
                        ((fnil + 0 0 0) base bonus boost))
        runner-strength (if (corp-start? trace)
                          ((fnil + 0 0) link boost)
                          strength)
        trigger-trace (select-keys trace [:player :other :base :bonus :link :priority :ability :strength])]
    (wait-for (pay-sync state other (make-eid state eid) card [:credit boost])
              (system-msg state other (str async-result
                                           " to increase " (if (corp-start? trace) "link" "trace")
                                           " strength to " (if (corp-start? trace)
                                                             runner-strength
                                                             corp-strength)))
              (clear-wait-prompt state player)
              (let [successful (> corp-strength runner-strength)
                    which-ability (assoc (if successful
                                           (:successful trace)
                                           (:unsuccessful trace))
                                         :eid (make-eid state))]
                (system-say state player (str "The trace was " (when-not successful "un") "successful."))
                (wait-for (trigger-event-simult state :corp (if successful :successful-trace :unsuccessful-trace)
                                                nil ;; No special functions
                                                (assoc trigger-trace
                                                       :corp-strength corp-strength
                                                       :runner-strength runner-strength
                                                       :successful successful
                                                       :corp-spent (if (corp-start? trace)
                                                                     (- strength base bonus)
                                                                     boost)
                                                       :runner-spent (if (corp-start? trace)
                                                                       boost
                                                                       (- strength link))))
                          (wait-for (resolve-ability state :corp (:eid which-ability) which-ability
                                                     card [corp-strength runner-strength])
                                    (if-let [kicker (:kicker trace)]
                                      (if (>= corp-strength (:min kicker))
                                        (continue-ability state :corp kicker card [corp-strength runner-strength])
                                        (effect-completed state side eid))
                                      (effect-completed state side eid))))))))

(defn trace-reply
  "Shows a trace prompt to the second player, after the first has already spent credits to boost."
  [state side eid card {:keys [player other base bonus link priority] :as trace} boost]
  (let [other-type (if (corp-start? trace) "link" "trace")
        strength (if (corp-start? trace)
                   ((fnil + 0 0 0) base bonus boost)
                   ((fnil + 0 0) link boost))
        trace (assoc trace :strength strength)]
    (wait-for (pay-sync state player (make-eid state eid) card [:credit boost])
              (system-msg state player (str async-result
                                            " to increase " (if (corp-start? trace) "trace" "link")
                                            " strength to " strength))
              (clear-wait-prompt state other)
              (show-wait-prompt state player
                                (str (if (corp-start? trace) "Runner" "Corp")
                                     " to boost " other-type " strength")
                                {:priority priority})
              (show-trace-prompt state other (make-eid state eid) card
                                 (str "Boost " other-type " strength?")
                                 #(resolve-trace state side eid card trace %)
                                 trace))))

(defn trace-start
  "Starts the trace process by showing the boost prompt to the first player (normally corp)."
  [state side eid card {:keys [player other base bonus priority label] :as trace}]
  (let [this-type (if (corp-start? trace) "trace" "link")]
    (system-msg state player (str "uses " (:title card)
                                  " to initiate a trace with strength " ((fnil + 0 0) base bonus)
                                  (when (pos? bonus)
                                    (str " (" base " + " bonus ")"))
                                  (when label
                                    (str " (" label ")"))))
    (show-wait-prompt state other
                      (str (if (corp-start? trace) "Corp" "Runner")
                           " to boost " this-type " strength")
                      {:priority priority})
    (show-trace-prompt state player (make-eid state eid) card
                       (str "Boost " this-type " strength?")
                       #(trace-reply state side eid card trace %)
                       trace)))

(defn reset-trace-modifications
  [state]
  (swap! state assoc :trace nil)
  (swap! state dissoc-in [:bonus :trace]))

(defn init-trace
  ([state side card] (init-trace state side (make-eid state {:source-type :trace}) card {:base 0}))
  ([state side card trace] (init-trace state side (make-eid state {:source-type :trace}) card trace))
  ([state side eid card {:keys [base priority] :as trace}]
   (reset-trace-modifications state)
   (wait-for (trigger-event-sync state :corp :pre-init-trace card eid)
             (let [force-base (get-in @state [:trace :force-base])
                   force-link (get-in @state [:trace :force-link])
                   base (cond force-base force-base
                              (fn? base) (base state :corp (make-eid state) card nil)
                              :else base)
                   link (or force-link
                            (get-in @state [:runner :link] 0))
                   bonus (get-in @state [:bonus :trace] 0)
                   initiator (determine-initiator state trace)
                   eid (assoc eid :source-type :trace)
                   corp-credits #(total-available-credits state :corp % card)
                   runner-credits #(total-available-credits state :runner % card)
                   trace (merge trace {:player initiator
                                       :other (if (= :corp initiator) :runner :corp)
                                       :base base
                                       :bonus bonus
                                       :link link
                                       :priority (or priority 2)
                                       :corp-credits corp-credits
                                       :runner-credits runner-credits})]
               (trace-start state side eid card trace)))))

(defn shuffle-into-rd-effect
  ([state side card n] (shuffle-into-rd-effect state side (make-eid state) card n false))
  ([state side card n all?] (shuffle-into-rd-effect state side (make-eid state) card n all?))
  ([state side eid card n all?]
   (continue-ability state side
                    {:show-discard  true
                     :choices {:max (min (-> @state :corp :discard count) n)
                               :card #(and (corp? %)
                                           (in-discard? %))
                               :all all?}
                     :msg (msg "shuffle "
                               (let [seen (filter :seen targets)
                                     m (count (filter #(not (:seen %)) targets))]
                                 (str (join ", " (map :title seen))
                                      (when (pos? m)
                                        (str (when-not (empty? seen) " and ")
                                             (quantify m "unseen card")))))
                               " into R&D")
                     :effect (req (doseq [c targets] (move state side c :deck))
                                  (shuffle! state side :deck))
                     :cancel-effect (req (shuffle! state side :deck))}
                    card nil)))
