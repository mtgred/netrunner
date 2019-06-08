(in-ns 'game.core)

(declare any-flag-fn? init-trace optional-ability check-optional check-psi check-trace
         complete-ability do-choices do-ability psi-game resolve-ability-eid
         resolve-psi resolve-trace show-select)

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

(defn can-trigger?
  "Checks if ability can trigger. Checks that once-per-turn is not violated."
  [state side {:keys [once once-key] :as ability} card targets]
  (let [cid (:cid card)]
    (and (not (get-in @state [once (or once-key cid)]))
         (should-trigger? state side card targets ability))))

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
      * a map containing the keyword :req with a value of a 1-argument function returning true or false. Triggers a
        'select' prompt with targeting cursor; only cards that cause the 1-argument function to return true will
        be allowed.
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
  :counter-cost / :advance-counter-cost -- number of counters to remove to resolve the ability
  :once -- its only value is :per-turn; signifies an effect that can only be triggered once per turn.
  :once-key -- by default, each :once is distinct per card. If multiple copies of a card can only resolve
               some ability once between all of them, then the card should specify a manual :once-key that can
               be any value, preferrably a unique keyword.
  :optional -- shows a 'Yes/No' prompt to let the user decide whether to resolve the ability.
  :end-turn -- if the ability is resolved, then this ability map will be resolved at the end of the turn.
  :makes-run -- indicates if the ability makes a run."
  ;; perhaps the most important function in the game logic
  ([state side {:keys [eid] :as ability} card targets]
   (resolve-ability state side (or eid (make-eid state {:source card :source-type :ability})) ability card targets))
  ([state side eid ability card targets]
   (resolve-ability-eid state side (assoc ability :eid eid :source card :source-type :ability) card targets)))

(defn- resolve-ability-eid
  ([state side {:keys [eid] :as ability} card targets]
   (if (= 1 (count ability)) ;; only has the eid, in effect a nil ability
     (effect-completed state side eid)
     (if (and ability (not eid))
       (resolve-ability-eid state side (assoc ability :eid (make-eid state eid)) card targets)
       (when ability
         ;; Is this an optional ability?
         (check-optional state side ability card targets)
         ;; Is this a psi game?
         (check-psi state side ability card targets)
         ;; Is this a trace?
         (check-trace state side ability card targets)
         ;; Ensure this ability can be triggered more than once per turn,
         ;; or has not been yet been triggered this turn.
         (if (can-trigger? state side ability card targets)
           (if (:choices ability)
             ;; It's a prompt!
             (do-choices state side ability card targets)
             ;; Not a prompt. Trigger the ability.
             (do-ability state side ability card targets))
           (effect-completed state side eid))
         (complete-ability state side ability))))))

;;; Checking functions for resolve-ability
(defn- complete-ability
  [state side {:keys [eid choices optional async psi trace]}]
  ;; If it doesn't have choices and it doesn't have a true async or
  ;; if it does have choices and has false async
  (when (or (not (or choices optional psi trace async))
            (and (or choices optional psi trace)
                 (false? async)))
    (effect-completed state side eid)))

(defn- check-optional
  "Checks if there is an optional ability to resolve"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [optional (:optional ability)]
    (if (can-trigger? state side optional card targets)
      (optional-ability state (or (:player optional) side) eid card (:prompt optional) optional targets)
      (effect-completed state side eid))))

(defn- check-psi
  "Checks if a psi-game is to be resolved"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [psi (:psi ability)]
    (if (can-trigger? state side psi card targets)
      (psi-game state side eid card psi)
      (effect-completed state side eid))))

(defn- check-trace
  "Checks if there is a trace to resolve"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [trace (:trace ability)]
    (if (can-trigger? state side ability card targets)
      (init-trace state side eid card trace)
      (effect-completed state side eid))))

(defn- do-choices
  "Handle a choices ability"
  [state side {:keys [choices player priority cancel-effect not-distinct prompt eid prompt-type] :as ability}
   card targets]
  (let [s (or player side)
        ab (dissoc ability :choices)
        args {:priority priority :cancel-effect cancel-effect :prompt-type prompt-type}]
   (if (map? choices)
     ;; Two types of choices use maps: select prompts, and :number prompts.
     (cond
       ;; a counter prompt
       (:counter choices)
       (prompt! state s card prompt choices ab args)
       ;; a select prompt
       (:req choices)
       (show-select state s card ability args)
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

(declare print-msg do-effect register-end-turn register-once)

(defn- do-ability
  "Perform the ability, checking all costs can be paid etc."
  [state side {:keys [eid cost counter-cost advance-counter-cost] :as ability} {:keys [advance-counter] :as card} targets]
  ;; Ensure counter costs can be paid
  (let [[counter-type counter-amount] counter-cost]
    (when (and (or (not counter-cost)
                   (<= (or counter-amount 0)
                       (get-in card [:counter counter-type] 0)))
               (or (not advance-counter-cost)
                   (<= advance-counter-cost (or advance-counter 0))))
      ;; Ensure that any costs can be paid
      (wait-for
        (pay-sync state side (make-eid state eid) card cost {:action (:cid card)})
        (if-let [cost-str async-result]
          (let [c (if counter-cost
                    (update-in card [:counter counter-type] #(- (or % 0) (or counter-amount 0)))
                    card)
                c (if advance-counter-cost
                    (update-in c [:advance-counter] #(- (or % 0) (or advance-counter-cost 0)))
                    c)]
            ;; Remove any counters
            (when (or counter-cost advance-counter-cost)
              (update! state side c)
              (when (is-type? card "Agenda")
                (trigger-event state side :agenda-counter-spent card)))
            ;; Print the message
            (print-msg state side ability card targets cost-str)
            ;; Trigger the effect
            (register-end-turn state side ability card targets)
            (register-once state ability card)
            (do-effect state side ability c targets)))))))

(defn- print-msg
  "Prints the ability message"
  [state side {:keys [eid] :as ability} card targets cost-str]
  (when-let [message (:msg ability)]
    (when-let [desc (if (string? message) message (message state side eid card targets))]
      (system-msg state (to-keyword (:side card))
                  (str (build-spend-msg cost-str "use")
                       (:title card) (when desc (str " to " desc)))))))

(defn- do-effect
  "Trigger the effect"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [ability-effect (:effect ability)]
    (ability-effect state side eid card targets)))

(defn- register-end-turn
  "Register :end-turn effect if present"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [end-turn (:end-turn ability)]
    (swap! state update-in [side :register :end-turn]
           #(conj % {:ability end-turn :card card :targets targets :eid eid}))))

(defn register-once
  "Register ability as having happened if :once specified"
  [state {:keys [once once-key] :as ability} {:keys [cid] :as card}]
  (when once (swap! state assoc-in [once (or once-key cid)] true)))

(defn active-prompt?
  "Checks if this card has an active prompt"
  [state side card]
  (some #(when (= (:cid card) (-> % :card :cid)) %)
        (flatten (map #(-> @state % :prompt) [side (other-side side)]))))

;;; Optional Ability
(defn optional-ability
  "Shows a 'Yes/No' prompt and resolves the given ability's :yes-ability if Yes is chosen, and :no-ability otherwise.
  If ability has an :autoresolve entry, first call it as a 5-function, and if it returns 'Yes' or 'No'
  resolve the ability as if prompt was displayed and Yes/No was chosen."
  ([state side card message ability targets] (optional-ability state side (make-eid state) card message ability targets))
  ([state side eid card message ability targets]
   (letfn [(prompt-fn [prompt-choice]
             (let [yes-ability (:yes-ability ability)]
               (if (and (= prompt-choice "Yes")
                        yes-ability
                        (can-pay? state side eid card (:title card) (:cost yes-ability)))
                 (resolve-ability state side (assoc yes-ability :eid eid) card targets)
                 (if-let [no-ability (:no-ability ability)]
                   (resolve-ability state side (assoc no-ability :eid eid) card targets)
                   (effect-completed state side eid)))))]
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
                  (-> args
                      (wrap-function :cancel-effect)
                      (wrap-function :end-effect))))))

(defn- add-to-prompt-queue
  "Adds a newly created prompt to the current prompt queue"
  [state side {:keys [priority] :as prompt}]
  (let [priority-comp #(case % true 1 nil 0 %)
        split-fn #(>= (priority-comp (:priority %)) (priority-comp priority))
        ;; insert the new prompt into the already-sorted queue based on its priority.
        update-fn #(let [[head tail] (split-with split-fn %)] (concat head (cons prompt tail)))]
    (swap! state update-in [side :prompt] update-fn)))

(defn show-prompt
  "Engine-private method for displaying a prompt where a *function*, not a card ability, is invoked
  when the prompt is resolved. All prompts flow through this method."
  ([state side card message choices f] (show-prompt state side (make-eid state) card message choices f nil))
  ([state side card message choices f args] (show-prompt state side (make-eid state) card message choices f args))
  ([state side eid card message choices f
    {:keys [priority prompt-type show-discard cancel-effect end-effect] :as args}]
   (let [prompt (if (string? message) message (message state side nil card nil))
         newitem {:eid eid
                  :msg prompt
                  :choices choices
                  :effect f
                  :card card
                  :prompt-type prompt-type
                  :show-discard show-discard
                  :priority priority
                  :cancel-effect cancel-effect
                  :end-effect end-effect}]
     (when (or (= prompt-type :waiting)
               (:number choices)
               (:card-title choices)
               (#{:credit :counter} choices)
               (pos? (count choices)))
       (add-to-prompt-queue state side newitem)))))

(defn- show-trace-prompt
  "Specific function for displaying a trace prompt. Works like `show-prompt` with some extensions.
   Always uses `:credit` as the `choices` variable, and passes on some extra properties, such as base and bonus."
  ([state side card message f args] (show-trace-prompt state side (make-eid state) card message f args))
  ([state side eid card message f {:keys [priority player other base bonus strength link] :as args}]
   (let [prompt (if (string? message) message (message state side nil card nil))
         corp-credits (total-available-credits state :corp eid card)
         runner-credits (total-available-credits state :runner eid card)
         newitem {:eid eid
                  :msg prompt
                  :choices (if (= :corp side) corp-credits runner-credits)
                  :corp-credits corp-credits
                  :runner-credits runner-credits
                  :prompt-type :trace
                  :effect f
                  :card card
                  :priority priority
                  :player player
                  :other other
                  :base base
                  :bonus bonus
                  :strength strength
                  :link link}]
     (add-to-prompt-queue state side newitem))))

(defn show-select
  "A select prompt uses a targeting cursor so the user can click their desired target of the ability.
  As with prompt!, the preferred method for showing a select prompt is through resolve-ability."
  ([state side card ability] (show-select state side card ability nil))
  ([state side card ability args]
   ;; if :max is a function, call it and assoc its return value as the new :max number of cards
   ;; that can be selected.
   (letfn [(wrap-function [args kw]
             (let [f (kw args)] (if f (assoc args kw #(f state side (:eid ability) card [%])) args)))]
     (let [ability (update-in ability [:choices :max] #(if (fn? %) (% state side (make-eid state) card nil) %))
           all (get-in ability [:choices :all])
           m (get-in ability [:choices :max])]
       (swap! state update-in [side :selected]
              #(conj (vec %) {:ability (dissoc ability :choices)
                              :req (get-in ability [:choices :req])
                              :not-self (when (get-in ability [:choices :not-self]) (:cid card))
                              :max m
                              :all all}))
       (show-prompt state side card
                    (if-let [message (:prompt ability)]
                      message
                      (if m
                        (str "Select " (if all "" "up to ") m " targets for " (:title card))
                        (str "Select a target for " (:title card))))
                    (if all ["Hide"] ["Done"])
                    (if all
                      (fn [choice]
                        (toast state side (str "You must choose " m))
                        (show-select state side card ability args))
                      (fn [choice] (resolve-select state side)))
                    (-> args
                        (assoc :prompt-type :select
                               :show-discard (:show-discard ability))
                        (wrap-function :cancel-effect)))))))

(defn resolve-select
  "Resolves a selection prompt by invoking the prompt's ability with the targeted cards.
  Called when the user clicks 'Done' or selects the :max number of cards."
  [state side]
  (let [selected (get-in @state [side :selected 0])
        cards (map #(dissoc % :selected) (:cards selected))
        curprompt (first (get-in @state [side :prompt]))]
    (swap! state update-in [side :selected] #(vec (rest %)))
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % curprompt) pr)))
    (if-not (empty? cards)
      (do (doseq [card cards]
            (update! state side card))
          (resolve-ability state side (:ability selected) (:card curprompt) cards))
      (if-let [cancel-effect (:cancel-effect curprompt)]
        (cancel-effect nil)
        (effect-completed state side (:eid (:ability selected)))))))

(defn show-wait-prompt
  "Shows a 'Waiting for ...' prompt to the given side with the given message.
  The prompt cannot be closed except by a later call to clear-wait-prompt.
  The prompt has default priority 1, but can be overridden."
  ([state side message] (show-wait-prompt state side message {:priority 1}))
  ([state side message {:keys [priority card] :as args}]
   (show-prompt state side card (str "Waiting for " message) nil
                (fn [c] (system-msg state side (str "is waiting for " message))) ; this function is never called, because the prompt has no button.
                {:priority priority :prompt-type :waiting})))

(defn clear-wait-prompt
  "Removes the first 'Waiting for...' prompt from the given side's prompt queue."
  [state side]
  (when-let [wait (some #(when (= :waiting (:prompt-type %)) %) (-> @state side :prompt))]
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % wait) pr)))))

;;; Psi games
(defn show-prompt-with-dice
  "Calls show-prompt normally, but appends a 'roll d6' button to choices.
  If user chooses to roll d6, reveal the result to user and re-display
  the prompt without the 'roll d6 button'."
  ([state side card message other-choices f]
   (show-prompt state side card message other-choices f nil))
  ([state side card message other-choices f args]
   (let [dice-msg "Roll a d6",
         choices (conj other-choices dice-msg)]
     (show-prompt state side card message choices
                  #(if (not= % dice-msg)
                     (f %)
                     (show-prompt state side card
                                  (str message " (Dice result: " (inc (rand-int 6)) ")")
                                  other-choices f args))
                  args))))

(defn psi-game
  "Starts a psi game by showing the psi prompt to both players. psi is a map containing
  :equal and :not-equal abilities which will be triggered in resolve-psi accordingly."
  ([state side card psi] (psi-game state side (make-eid state) card psi))
  ([state side eid card psi]
   (swap! state assoc :psi {})
   (register-once state psi card)
   (system-msg state (:player psi :corp) (str "uses " (:title card) " to start a psi game"))
   (doseq [s [:corp :runner]]
     (let [all-amounts (range (min 3 (inc (get-in @state [s :credit]))))
           valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                      (any-flag-fn? state :runner :prevent-secretly-spend %))
                                 all-amounts)]

       (show-prompt-with-dice state s card (str "Choose an amount to spend for " (:title card))
                              (map #(str % " [Credits]") valid-amounts)
                              #(resolve-psi state s eid card psi (str->int (first (split % #" "))))
                    {:priority 2
                     :prompt-type :psi})))))

(defn resolve-psi
  "Resolves a psi game by charging credits to both sides and invoking the appropriate
  resolution ability."
  [state side eid card psi bet]
  (swap! state assoc-in [:psi side] bet)
  (let [opponent (if (= side :corp) :runner :corp)]
    (if-let [opponent-bet (get-in @state [:psi opponent])]
      (do (clear-wait-prompt state opponent)
          (deduct state opponent [:credit opponent-bet])
          (system-msg state opponent (str "spends " opponent-bet " [Credits]"))
          (deduct state side [:credit bet])
          (system-msg state side (str "spends " bet " [Credits]"))
          (wait-for (trigger-event-simult state side :reveal-spent-credits nil (get-in @state [:psi :corp]) (get-in @state [:psi :runner]))
                    (if-let [ability (if (= bet opponent-bet) (:equal psi) (:not-equal psi))]
                      (let [card-side (if (= "Corp" (:side card)) :corp :runner)]
                        (continue-ability state card-side (assoc ability :async true) card nil))
                      (effect-completed state side eid))))
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
              (system-msg state other (str " spends " boost
                                           " [Credits] to increase " (if (corp-start? trace) "link" "trace")
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
              (system-msg state player (str " spends " boost
                                            "[Credits] to increase " (if (corp-start? trace) "trace" "link")
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
  (swap! state dissoc :trace)
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
                   trace (merge trace {:player initiator
                                       :other (if (= :corp initiator) :runner :corp)
                                       :base base
                                       :bonus bonus
                                       :link link
                                       :priority (or priority 2)})]
               (trace-start state side eid card trace)))))

(defn rfg-and-shuffle-rd-effect
  ([state side card n] (rfg-and-shuffle-rd-effect state side (make-eid state) card n false))
  ([state side card n all?] (rfg-and-shuffle-rd-effect state side (make-eid state) card n all?))
  ([state side eid card n all?]
   (move state side card :rfg)
   (continue-ability state side
                    {:show-discard  true
                     :choices {:max n
                               :req #(and (= (:side %) "Corp")
                                          (= (:zone %) [:discard]))
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
