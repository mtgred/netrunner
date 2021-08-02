(ns game.core.engine
  (:require
    [clojure.set :as clj-set]
    [clojure.stacktrace :refer [print-stack-trace]]
    [clojure.string :as string]
    [clj-uuid :as uuid]
    [game.core.board :refer [clear-empty-remotes]]
    [game.core.card :refer [active? facedown? get-card get-cid has-subtype? installed? rezzed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [any-effects effect-pred get-effect-maps unregister-floating-effects]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs handler]]
    [game.core.prompt-state :refer [add-to-prompt-queue]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-select show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [check-win-by-agenda]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [dissoc-in distinct-by in-coll? server-cards remove-once same-card? side-str to-keyword]]
    [jinteki.utils :refer [other-side]]))

;; resolve-ability docs

; Perhaps the most important function in the game logic.

; Resolves an ability defined by the given ability map. Checks :req functions; shows prompts, psi games,
; traces, etc. as needed; charges costs; invokes :effect functions. All card effects and most engine effects
; should run through this method. Unless noted, every key listed is optional.

; How to read this:
; :key -- type
;   Description, reasoning, expected return value, etc.

; Types:
; integer, string, keyword: clojure literals
; boolean: clojure literal, but should generally only be true (as leaving it off counts as false)
; vector: can be a literal [] or '(), can also be the return value of a function, but
;   must not be a function definition that returns a vector or list
; map of X: clojure literal. X is a comma-separated list of allowed keys.
;   The description will be laid out recursively, and keys will be marked as required or optional.
; ability map: an ability map as defined here.
; enum: a comma-separated list of values, one of which can be used.
; 5-fn: typically (req), but any function definition that takes state, side, eid, card, targets.

; Something that accepts different types will have the types separated by "or".
; For example, "string or 5-fn" means it accepts a string or a 5-fn. The description will
; say what the 5-fn should return, if anything.

; COMMON KEYS
; :req -- 5-fn
;   Must return true or false. If false, the ability will not be resolved.
; :cost -- vector
;   A vector of cost pairs to charge, for example [:credit 1 :click 1].
;   If the costs cannot be paid, the ability will not be resolved.
; :msg -- string or 5-fn.
;   Must return a string. (`msg` is expressly built for this.)
;   Prints to the log when the ability is finished. Will be used as the :label if no :label is provided.
;   The output is written as:
;     (with cost) "{Player} pays {X} to use {card title} to {msg result}."
;     (with no cost) "{Player} uses {card title} to {msg result}."
;   so the returned string should always be written imperatively.
; :label -- string
;   If this ability is in an :abilities map on a card, the label will be prepended with a string
;   of the costs, and both will be displayed in the ability button. If this is not defined
;   and a label is needed, :msg will be used if it is a string.
; :effect -- 5-fn
;   Will be called if the ability will be resolved (if :req is true or not present,
;   costs can be paid, and any prompts are resolved).
; :player -- enum: :corp, :runner
;   Manually specifies which player the ability should affect, rather than leave it implicit.
; :async -- boolean
;   Mark the ability as "async", meaning the :effect function must call effect-completed itself.
;   Without this being set to true, resolve-ability will call effect-completed once it's done.
;   This part of the engine is really dumb and complicated, so ask someone on slack about it.
; :cost-req -- 1-fn
;   A function which will be applied to the cost of an ability immediatly prior to being paid. See all-stealth or min-stealth for examples.

; PROMPT KEYS
; :prompt -- string or 5-fn
;   Must return a string to display in the prompt menu.
; :choices -- 5-fn or :credit or :counter or map
;   This key signals a prompt of some kind. (It should be split into multiple keys, but that's a lot of work lol)
;   * 5-fn
;       Must return a vector of card objects or strings.
;       User chooses one. This is called a 'choice' prompt.
;   * :credit
;       User chooses an integer up to their current credit amount.
;   * :counter
;       User chooses an integer up to the :counter value of the current card.
;   * map of :number, :default
;       User chooses an integer between 0 and some number
;       :number -- 5-fn
;         Required. Must return a number, which is the maximum allowed.
;       :default -- 5-fn
;         Optional. Must return a number, which is the number the dropdown will display initially.
;   * map of :card or :req, :all, :max, :not-self
;       Triggers a 'select' prompt with targeting cursor for "selecting" one or more cards.
;       :card -- 1-argument function
;         Either this or :req is required, but not both.
;         Accepts a single card object, must return true or false to indicate if selection is successful.
;       :req -- 5-fn
;         Either this or :card is required, but not both.
;         "target" is the clicked card. Must return true or false to indicate if selection is successful.
;       :all -- boolean
;         Optional. Changes the select prompt from optional (can click "Done" to not select anything) to
;         mandatory. When :max is also set to true, enforces selecting the specified number of cards.
;       :max -- integer
;         Optional. Changes the select from to resolving after selecting a single card to
;         resolving when either a number of cards (as set by :max) have been selected or a number
;         of cards less than that set by :max are selected and the "Done" button has been clicked.
;         When :all is also set to true, enforces selecting the specified number of cards.
;       :not-self -- boolean
;         Certain abilities can target any card on the table, others can only target other cards.
;         For example, Yusuf vs Aesop's. Setting this is the same as adding
;         `(not (same-card? % card))` to your :card function or `(not (same-card? target card))`
;         to your :req function.
; :not-distinct -- boolean
;   By default, duplicate entries of the same string will be combined into a single button.
;   If set to true, duplicate entries of the same string will be shown as multiple buttons.
; :cancel-effect -- 5-fn
;   If a prompt with the choice "Cancel" is clicked, the prompt exits without doing anything else
;   and this function will be called.

; SIMULTANEOUS EFFECT RESOLUTION KEYS
; :interactive -- 5-fn. when simultaneous effect resolution has been enabled for a specific event, the user receives
;                 a menu of cards that handle the effect and get to choose the order of their resolution. This menu is
;                 only shown if at least one ability handling the event has an :interactive function that returns true.
;                 If none are interactive, then all handlers will be resolved automatically, one at a time in an
;                 arbitrary order. In general, handlers should be marked ':interactive (req true)' if they have
;                 important order-of-effect interactions with other cards. The :interactive function can be coded to
;                 have smarter logic if necessary -- see Replicator, which is only interactive if there is another
;                 copy of the installed card remaining in the Stack.
; :silent -- any handler that does not require user interaction under any circumstances can be marked :silent. If a
;            handler's :silent function returns true, then no menu entry will be shown for the handler. In that case,
;            the ability will only be resolved once all non-silent abilities are resolved. Example: AstroScript has no
;            important interactions with other 'agenda scored' effects, and doesn't care when an agenda token is placed.
;            Example: Hayley Kaplan will not show a prompt if there are no valid targets in the grip.

; OTHER KEYS
; :once -- either :per-turn or :per-run. signifies an effect that can only be triggered once per turn.
; :once-key -- keyword. by default, each :once is distinct per card. If multiple copies of a card can only resolve
;              some ability once between all of them, then the card should specify a manual :once-key that can
;              be any value, preferrably a unique keyword.
; :install-req -- a function which returns a list of servers a card may be installed into
; :makes-run -- boolean. indicates if the ability makes a run.

; COMPLEX ABILITY WRAPPERS
; These are wrappers around more complex/cumbersome functionality. They all call into a flow that would
; be cumbersome to write out every time in a "normal" ability map.
; :psi -- map of :req, :equal, :not-equal
;   Handles psi games.
;   :req -- 5-fn
;     Optional. Works same as :req in an ability map.
;   :equal -- ability map
;     At least one of :equal and :not-equal are required.
;     Async ability that is executed if the players reveal the same number of credits.
;   :not-equal -- ability map
;     At least one of :equal and :not-equal are required.
;     Async ability that is executed if the players reveal the a differen number of credits.
; :trace -- map of :req, :label, :base, :successful, :unsuccessful, :kicker, :kicker-min
;   Handles traces.
;   :req -- 5-fn
;     Optional. Works same as :req in an ability map.
;   :label -- 5-fn
;     Optional. Works same as :label in an ability map.
;   :base -- integer or 5-fn
;     Required. Must return an integer, which sets the "base" initial value for the trace.
;   :successful -- ability map
;     At least one of :successful and :unsuccessful are required.
;     Async ability that is executed if the trace is successful.
;   :unsuccessful -- ability map
;     At least one of :successful and :unsuccessful are required.
;     Async ability that is executed if the trace is unsuccessful.
;   :kicker -- ability map
;     Optional. Async ability that is executed if the corp's trace strength is equal to or greater
;     than :kicker-min.
;   :kicker-min -- integer
;     Required if :kicker is defined. The number the corp's strength must be equal to or greater
;     to execute the :kicker ability.
; :optional -- map of :req, :prompt, :player, :yes-ability, :no-ability, :end-effect, :autoresolve
;   Shows a "Yes/No" prompt to handle the user deciding whether to resolve the ability.
;   :req -- 5-fn
;     Optional. Works same as :req in an ability map.
;   :prompt -- string or 5-fn
;     Required. Must return a string to display in the prompt menu.
;   :player -- enum: :corp, :runner
;     Optional. Hardcodes which player will resolve this prompt.
;   :yes-ability -- ability-map
;     At least one of :yes-ability and :no-ability are required. (Normally, this should always
;     be defined, but sometimes it's easier to word things in the negative, such that only
;     defining :no-ability is the most natural way to write the ability.)
;     Async ability that is executed if "Yes" is chosen.
;   :no-ability -- ability-map
;     At least one of :yes-ability and :no-ability are required. (See note above.)
;     Async ability that is executed if "No" is chosen, or if "Yes" is chosen but there's
;     no :yes-ability defined, or :yes-ability has a :cost that the player can't afford.
;   :end-effect -- 5-fn
;     Called after :yes- or :no-ability has been resolved.
;     Must not be async as it is not awaited.
;   :autoresolve -- 5-fn
;     Optional. Used to set an optional prompt to either ask, always choose "Yes", or always choose "No".
;     Must use the (get-autoresolve kw) helper function, where kw is an ability-specific
;     keyword that will be stored in the card's [:special] map.
;     Must use the (set-autoresolve kw) helper function in the card's :abilities vector,
;     to allow for changing the autoresolve settings, where kw is the same keyword as used in
;     the (get-autoresolve kw) call.

(declare do-ability resolve-ability-eid check-ability pay prompt! check-choices do-choices)

(def ability-types (atom {}))

(defn register-ability-type
  [kw ability-fn]
  (swap! ability-types assoc kw ability-fn))

(defn select-ability-kw
  [ability]
  (first (keys (select-keys ability (keys @ability-types)))))

(defn dissoc-req
  [ability]
  (if-let [ab (select-ability-kw ability)]
    (dissoc-in ability [ab :req])
    (dissoc ability :req)))

(defn should-trigger?
  "Checks if the specified ability definition should trigger.
  Checks for a :req, either in the top level map, or in an :optional or :psi sub-map
  Returns true if no :req found, returns nil if the supplied ability is nil"
  ([state side eid card targets {:keys [req] :as ability}]
   (when ability
     (let [ab (select-ability-kw ability)]
       (cond
         ab (should-trigger? state side eid card targets (get ability ab))
         req (req state side eid card targets)
         :else true)))))

(defn not-used-once?
  [state {:keys [once once-key]} {:keys [cid]}]
  (if once
    (not (get-in @state [once (or once-key cid)]))
    true))

(defn can-trigger?
  "Checks if ability can trigger. Checks that once-per-turn is not violated."
  ([state side eid ability card] (can-trigger? state side eid ability card nil))
  ([state side eid ability card targets]
   (and (not-used-once? state ability card)
        (should-trigger? state side eid card targets ability))))

(defn is-ability?
  "Checks to see if a given map represents a card ability."
  [{:keys [effect msg]}]
  (or effect msg (seq (keys @ability-types))))

(defn resolve-ability
  ([state side {:keys [eid action] :as ability} card targets]
   (resolve-ability state side (or eid (make-eid state {:source card :source-type :ability :action action})) ability card targets))
  ([state side eid ability card targets]
   (resolve-ability-eid state side (assoc ability :eid eid) card targets)))

(defn- resolve-ability-eid
  [state side {:keys [eid choices] :as ability} card targets]
  (cond
    ;; Only has the eid, in effect a nil ability
    (and eid (= 1 (count ability)))
    (effect-completed state side eid)
    ;; Both ability and eid are present, so we're good to go
    (and ability eid)
    (let [ab (select-ability-kw ability)
          ability-fn (get @ability-types ab)]
      (cond
        ab (ability-fn state side ability card targets)
        choices (check-choices state side ability card targets)
        :else (check-ability state side ability card targets)))
    ;; Something has gone terribly wrong, error out
    :else
    (.println *err* (with-out-str
                      (print-stack-trace
                        (Exception. (str "Ability is nil????" ability card targets))
                        2500)))))

;;; Checking functions for resolve-ability
(defn- check-choices
  [state side {:keys [eid] :as ability} card targets]
  (if (can-trigger? state side eid ability card targets)
    (do-choices state side ability card targets)
    (effect-completed state side eid)))

(defn- check-ability
  [state side {:keys [eid] :as ability} card targets]
  (if (can-trigger? state side eid ability card targets)
    (do-ability state side ability card targets)
    (effect-completed state side eid)))

(defn- print-msg
  "Prints the ability message"
  [state side {:keys [eid] :as ability} card targets payment-str]
  (when-let [message (:msg ability)]
    (let [desc (if (string? message) message (message state side eid card targets))
          cost-spend-msg (build-spend-msg payment-str "use")]
      (system-msg state (to-keyword (:side card))
                  (str cost-spend-msg (:title card) (str " to " desc))))))

(defn register-once
  "Register ability as having happened if :once specified"
  [state _ {:keys [once once-key]} {:keys [cid]}]
  (when once
    (swap! state assoc-in [once (or once-key cid)] true)))

(defn- do-effect
  "Trigger the effect"
  [state side {:keys [eid] :as ability} card targets]
  (if-let [ability-effect (:effect ability)]
    (ability-effect state side eid card targets)
    (effect-completed state side eid)))

(defn- ugly-counter-hack
  "This is brought over from the old do-ability because using `get-card` or `find-latest`
  currently doesn't work properly with `pay-counters`"
  [card cost]
  ;; TODO: Remove me some day
  (let [[counter-type counter-amount]
        (->> cost
             (remove map?)
             merge-costs
             (filter #(some #{:advancement :agenda :power :virus} %))
             first)]
    (if counter-type
      (let [counter (if (= :advancement counter-type)
                      [:advance-counter]
                      [:counter counter-type])]
        (update-in card counter - counter-amount))
      card)))

(declare checkpoint)

(defn merge-costs-paid
  ([cost-paid]
   (into {} (map (fn [[k {:keys [type value targets]}]]
                   [k {:type type
                       :value value
                       :targets targets}])
                 cost-paid)))
  ([cost-paid1 cost-paid2]
   (let [costs-paid [cost-paid1 cost-paid2]
         cost-keys (mapcat keys costs-paid)]
     (reduce (fn [acc cur]
               (let [costs (map cur costs-paid)
                     cost-obj {:type cur
                               :value (apply + (keep :value costs))
                               :targets (seq (apply concat (keep :targets costs)))}]
                 (assoc acc cur cost-obj)))
             {}
             cost-keys)))
  ([cost-paid1 cost-paid2 & costs-paid]
   (reduce merge-costs-paid (merge-costs-paid cost-paid1 cost-paid2) costs-paid)))

(defn- do-ability
  "Perform the ability, checking all costs can be paid etc."
  [state side {:keys [async eid cost player waiting-prompt action label] :as ability} card targets]
  (when waiting-prompt
    (add-to-prompt-queue
      state (cond
              player (if (= :corp player) :runner :corp)
              (= :corp side) :runner
              :else :corp)
      {:eid (select-keys eid [:eid])
       :card card
       :prompt-type :waiting
       :msg (str "Waiting for " waiting-prompt)}))
  ;; Ensure that any costs can be paid
  (wait-for (pay state side (make-eid state eid) card cost {:action (:cid card)})
            ;; If the cost can be and is paid, perform the ablity
            (let [payment-str (:msg async-result)
                  cost-paid (merge-costs-paid (:cost-paid eid) (:cost-paid async-result))]
              (if payment-str
                (let [ability (assoc-in ability [:eid :cost-paid] cost-paid)]
                  ;; Print the message
                  (print-msg state side ability card targets payment-str)
                  ;; Trigger the effect
                  (register-once state side ability card)
                  (do-effect state side ability (ugly-counter-hack card cost) targets)
                  ;; If the ability isn't async, complete it
                  (when-not async
                    (effect-completed state side eid)))
                (effect-completed state side eid)))))

(defn- do-choices
  "Handle a choices ability"
  [state side {:keys [choices eid not-distinct player prompt] :as ability} card targets]
  (let [s (or player side)
        ab (dissoc ability :choices :waiting-prompt)
        args (-> ability
                 (select-keys [:priority :cancel-effect :prompt-type :show-discard :end-effect :waiting-prompt])
                 (assoc :targets targets))]
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

;;; Prompts
(defn prompt!
  "Shows a prompt with the given message and choices. The given ability will be resolved
  when the user resolves the prompt. Cards should generally not call this function directly; they
  should use resolve-ability to resolve a map containing prompt data.

  Please refer to the documentation at the top of resolve_ability.clj for a full description."
  ([state side card message choices ability] (prompt! state side card message choices ability nil))
  ([state side card message choices ability args]
   (let [f #(resolve-ability state side ability card [%])]
     (show-prompt state side (:eid ability) card message choices f
                  (if-let [f (:cancel-effect args)]
                    (assoc args :cancel-effect #(f state side (:eid ability) card [%]))
                    args)))))

;; EVENTS

; Functions for registering trigger suppression events
(defn register-suppress
  "Registers each suppression handler in the given card definition. Suppression handlers
  can prevent the dispatching of a particular event."
  ([state side card] (register-suppress state side card (:suppress (card-def card))))
  ([state _ card events]
   (when events
     (let [abilities
           (->> (for [ability events]
                  {:event (:event ability)
                   :ability (dissoc ability :event)
                   :card card
                   :uuid (uuid/v1)})
                (into []))]
       (when (seq abilities)
         (swap! state update :suppress #(apply conj % abilities)))
       abilities))))

(defn unregister-suppress
  "Removes all event handler suppression effects as defined for the given card"
  ([state side card] (unregister-suppress state side card (:suppress (card-def card))))
  ([state _ card events]
   (let [abilities (map :event events)]
     (swap! state assoc :suppress
            (->> (:suppress @state)
                 (remove #(and (same-card? card (:card %))
                               (in-coll? abilities (:event %))))
                 (into []))))))

(defn unregister-suppress-by-uuid
  "Removes a single event handler with matching uuid"
  [state _ uuid]
  (swap! state assoc :suppress (remove-once #(= uuid (:uuid %)) (:suppress @state))))


(defn- default-locations
  [card]
  (case (to-keyword (:type card))
    :agenda #{:scored}
    (:asset :ice :upgrade) #{:servers}
    (:event :operation) #{:current :play-area}
    (:hardware :program :resource) #{:rig}
    (:identity :fake-identity) #{:identity}))

; Functions for registering and dispatching events.
(defn register-events
  "Registers each event handler defined in the given card definition.
  Also registers any suppression events."
  ([state side card] (register-events state side card nil))
  ([state side card events]
   (let [events (or (seq events) (remove :location (:events (card-def card))))
         abilities
         (->> (for [ability events]
                {:event (:event ability)
                 :location (let [location (:location ability)]
                             (cond
                               (or (list? location)
                                   (vector? location))
                               (into #{} location)
                               (keyword? location)
                               #{location}
                               :else
                               (default-locations card)))
                 :duration (or (:duration ability) :while-installed)
                 :condition (or (:condition ability) :active)
                 :unregister-once-resolved (or (:unregister-once-resolved ability) false)
                 :once-per-instance (or (:once-per-instance ability) false)
                 :ability (dissoc ability :event :duration :condition)
                 :card card
                 :uuid (uuid/v1)})
              (into []))]
     (when (seq abilities)
       (swap! state update :events #(apply conj % abilities)))
     (register-suppress state side card)
     abilities)))

(defn unregister-events
  "Removes all event handlers defined for the given card.
  Optionally input a partial card-definition map to remove only some handlers"
  ([state side card] (unregister-events state side card nil))
  ([state side card cdef]
   ;; Combine normal events and derezzed events. Any merge conflicts should not matter
   ;; as they should cause all relevant events to be removed anyway.
   (let [events (or (seq (concat (:events cdef) (:derezzed-events cdef)))
                    (let [cdef (card-def card)]
                      (remove :location (concat (:events cdef) (:derezzed-events cdef)))))
         abilities (map :event events)]
     (swap! state assoc :events
            (->> (:events @state)
                 (remove #(and (same-card? card (:card %))
                               (in-coll? abilities (:event %))
                               (= :while-installed (:duration %))))
                 (into [])))
     (unregister-suppress state side card))))

(defn unregister-floating-events
  "Removes all event handlers with a non-persistent duration"
  [state _ duration]
  (when (not= :while-installed duration)
    (swap! state assoc :events
           (->> (:events @state)
                (remove #(= duration (:duration %)))
                (into [])))))

(defn unregister-floating-events-for-card
  "Removes all event handlers with a non-persistent duration on a single card"
  [state _ card duration]
  (swap! state assoc :events
         (->> (:events @state)
              (remove #(and (same-card? card (:card %))
                            (= duration (:duration %))))
              (into []))))

(defn unregister-event-by-uuid
  "Removes a single event handler with matching uuid"
  [state _ uuid]
  (swap! state assoc :events (remove-once #(= uuid (:uuid %)) (:events @state))))

;; triggering events
(defn- get-side
  [ability]
  (to-keyword (get-in ability [:card :side])))

(defn- get-ability-side
  [ability]
  (get-in ability [:ability :side]))

(defn- is-active-player
  [state ability]
  (= (:active-player @state) (get-side ability)))

(defn- card-for-ability
  [state ability]
  (if (#{:while-installed :pending} (:duration ability))
    (when-let [card (get-card state (:card ability))]
      (if (and (= :while-installed (:duration ability))
               (not-empty
                 (clj-set/intersection (:location ability) (default-locations card))))
        (case (:condition ability)
          :active
          (when (active? card)
            card)
          :facedown
          (when (and (installed? card)
                     (facedown? card))
            card)
          :derezzed
          (when (and (installed? card)
                     (not (rezzed? card)))
            card)
          :hosted
          (when (:host card)
            card)
          ; else
          nil)
        card))
    (:card ability)))

(defn trigger-suppress
  "Returns true if the given event on the given targets should be suppressed, by triggering
  each suppression handler and returning true if any suppression handler returns true."
  [state side event & targets]
  (->> (:suppress @state)
       (filter #(= event (:event %)))
       (some #((:req (:ability %)) state side (make-eid state) (card-for-ability state %) targets))))

(defn gather-events
  "Prepare the list of the given player's handlers for this event.
  Gather all registered handlers from the state, then append the card-abilities if appropriate,
  then filter to remove suppressed handlers and those whose req is false.
  This is essentially Phase 9.3 and 9.6.7a of CR 1.1:
  http://nisei.net/files/Comprehensive_Rules_1.1.pdf"
  ([state side event targets] (gather-events state side event targets nil))
  ([state side event targets card-abilities] (gather-events state side (make-eid state) event targets nil))
  ([state side eid event targets card-abilities]
   (->> (:events @state)
        (filter #(= event (:event %)))
        (concat card-abilities)
        (filter identity)
        (filter (fn [ability]
                  (let [card (card-for-ability state ability)]
                    (and (not (apply trigger-suppress state side event card targets))
                         (can-trigger? state side eid (:ability ability) card targets)))))
        (sort-by (complement #(is-active-player state %)))
        doall)))

(defn- log-event
  [state event targets]
  (swap! state update :turn-events #(cons [event targets] %))
  (when (:run @state)
    (swap! state update-in [:run :events] #(cons [event targets] %))))

(defn trigger-event
  "Resolves all abilities registered as handlers for the given event key, passing them
  the targets given."
  [state side event & targets]
  (when (some? event)
    (log-event state event targets)
    (let [handlers (gather-events state side event targets)]
      (doseq [to-resolve handlers]
        (when-let [card (card-for-ability state to-resolve)]
          (resolve-ability state side (dissoc-req (:ability to-resolve)) card targets)
          (when (:unregister-once-resolved to-resolve)
            (unregister-event-by-uuid state side (:uuid to-resolve))))))))

(defn- trigger-event-sync-next
  [state side eid handlers event targets]
  (if-let [to-resolve (first handlers)]
    (if-let [card (card-for-ability state to-resolve)]
      (wait-for (resolve-ability state side (make-eid state (assoc eid :source card :source-type :ability)) (dissoc-req (:ability to-resolve)) card targets)
                (when (:unregister-once-resolved to-resolve)
                  (unregister-event-by-uuid state side (:uuid to-resolve)))
                (trigger-event-sync-next state side eid (rest handlers) event targets))
      (trigger-event-sync-next state side eid (rest handlers) event targets))
    (effect-completed state side eid)))

(defn trigger-event-sync
  "Triggers the given event synchronously, requiring each handler to complete before alerting the next handler. Does not
  give the user a choice of what order to resolve handlers."
  [state side eid event & targets]
  (if (nil? event)
    (effect-completed state side eid)
    (do (log-event state event targets)
        (let [handlers (gather-events state side eid event targets nil)]
          (trigger-event-sync-next state side eid handlers event targets)))))

(defn- trigger-event-simult-player
  "Triggers the simultaneous event handlers for the given event trigger and player.
  If none of the handlers require interaction, then they are all resolved automatically, each waiting for the previous
  to fully resolve as in trigger-event-sync. If at least one requires interaction, then a menu is shown to manually
  choose the order of resolution.

  :silent abilities are not shown in the list of handlers, and are resolved last in an arbitrary order."
  [state side eid handlers cancel-fn event-targets]
  (if (not-empty handlers)
    (letfn [;; Allow resolution as long as there is no cancel-fn, or if the cancel-fn returns false.
            (should-continue [state handlers]
              (and (< 1 (count handlers))
                   (not (and cancel-fn (cancel-fn state)))))
            (choose-handler [handlers]
              (let [handlers (when-not (and cancel-fn (cancel-fn state))
                               (filter #(and (card-for-ability state %)
                                             (not (:disabled (card-for-ability state %))))
                                handlers))
                    non-silent (filter #(let [silent-fn (:silent (:ability %))
                                              card (card-for-ability state %)]
                                          (not (and silent-fn
                                                    (silent-fn state side (make-eid state) card event-targets))))
                                       handlers)
                    titles (map :card non-silent)
                    interactive (filter #(let [interactive-fn (:interactive (:ability %))
                                               card (card-for-ability state %)]
                                           (and interactive-fn
                                                (interactive-fn state side (make-eid state) card event-targets)))
                                        handlers)]
                ;; If there is only 1 non-silent ability, resolve that then recurse on the rest
                (if (or (= 1 (count handlers)) (empty? interactive) (= 1 (count non-silent)))
                  (let [to-resolve (if (= 1 (count non-silent))
                                     (first non-silent)
                                     (first handlers))
                        ability-to-resolve (dissoc-req (:ability to-resolve))
                        others (if (= 1 (count non-silent))
                                 (remove-once #(= (get-cid to-resolve) (get-cid %)) handlers)
                                 (rest handlers))]
                    (if-let [the-card (card-for-ability state to-resolve)]
                      {:async true
                       :effect (req (wait-for (resolve-ability state (to-keyword (:side the-card))
                                                               (make-eid state (assoc eid :source the-card :source-type :ability))
                                                               ability-to-resolve
                                                               the-card event-targets)
                                              (when (:unregister-once-resolved to-resolve)
                                                (unregister-event-by-uuid state side (:uuid to-resolve)))
                                              (if (should-continue state handlers)
                                                (continue-ability state side
                                                                  (choose-handler others) nil event-targets)
                                                (effect-completed state side eid))))}
                      {:async true
                       :effect (req (if (should-continue state handlers)
                                      (continue-ability state side (choose-handler (rest handlers)) nil event-targets)
                                      (effect-completed state side eid)))}))
                  {:prompt "Choose a trigger to resolve"
                   :choices titles
                   :async true
                   :effect (req (let [to-resolve (some #(when (same-card? target (:card %)) %) handlers)
                                      ability-to-resolve (dissoc-req (:ability to-resolve))
                                      the-card (card-for-ability state to-resolve)]
                                  (wait-for
                                    (resolve-ability state (to-keyword (:side the-card))
                                                     (make-eid state (assoc eid :source the-card :source-type :ability))
                                                     ability-to-resolve the-card event-targets)
                                    (when (:unregister-once-resolved to-resolve)
                                      (unregister-event-by-uuid state side (:uuid to-resolve)))
                                    (if (should-continue state handlers)
                                      (continue-ability state side
                                                        (choose-handler
                                                          (remove-once #(same-card? target (:card %)) handlers))
                                                        nil event-targets)
                                      (effect-completed state side eid)))))})))]

      (continue-ability state side (choose-handler handlers) nil event-targets))
    (effect-completed state side eid)))

(defn ability-as-handler
  "Wraps a card ability as an event handler."
  [card ability]
  {:duration (or (:duration ability) :pending)
   :card card
   :ability ability})

(defn card-as-handler
  "Wraps a card's definition as an event handler."
  [card]
  (let [{:keys [effect prompt choices psi optional trace] :as cdef} (card-def card)]
    (when (or effect prompt choices psi optional trace)
      (ability-as-handler card cdef))))

(defn effect-as-handler
  "Wraps a five-argument function as an event handler."
  [card effect]
  (ability-as-handler card {:effect effect}))

(defn- event-title
  "Gets a string describing the internal engine event keyword"
  [event]
  (if (keyword? event)
    (name event)
    (str event)))

(defn trigger-event-simult
  "Triggers the given event by showing a prompt of all handlers for the event, allowing manual resolution of
  simultaneous trigger handlers. effect-completed is fired once all registered event handlers have completed.
  Parameters:
  state, side: as usual
  eid: the eid of the entire triggering sequence, which will be completed when all handlers are completed
  event: the event keyword to trigger handlers for
  first-ability: an ability map (fed to resolve-ability) that should be resolved after the list of handlers is determined
                 but before any of them is actually fired. Typically used for core rules that happen in the same window
                 as triggering handlers, such as trashing a corp Current when an agenda is stolen. Necessary for
                 interaction with New Angeles Sol and Employee Strike
  card-ability:  a card's ability that triggers at the same time as the event trigger, but is coded as a card ability
                 and not an event handler. (For example, :stolen on agendas happens in the same window as :agenda-stolen
  after-active-player: an ability to resolve after the active player's triggers resolve, before the opponent's get to act
  cancel-fn:     a function that takes one argument (the state) and returns true if we should stop the event resolution
                 process, likely because an event handler caused a change to the game state that cancels future handlers.
                 (Film Critic)
  targets:       a varargs list of targets to the event, as usual"
  [state side eid event {:keys [first-ability card-abilities after-active-player cancel-fn]} & targets]
  (if (nil? event)
    (effect-completed state side eid)
    (do (log-event state event targets)
        (let [active-player (:active-player @state)
              opponent (other-side active-player)
              is-player (fn [player ability]
                          (or (= player (get-side ability))
                              (= player (get-ability-side ability))))
              card-abilities (if (and (some? card-abilities)
                                      (not (sequential? card-abilities)))
                               [card-abilities]
                               card-abilities)
              handlers (gather-events state side eid event targets card-abilities)
              get-handlers (fn [player-side]
                             (filterv (partial is-player player-side) handlers))
              active-player-events (get-handlers active-player)
              opponent-events (get-handlers opponent)]
          (wait-for (resolve-ability state side (make-eid state eid) first-ability nil nil)
                    (show-wait-prompt state opponent
                                      (str (side-str active-player) " to resolve " (event-title event) " triggers")
                                      {:priority -1})
                    ; let active player activate their events first
                    (wait-for (trigger-event-simult-player state side (make-eid state eid) active-player-events cancel-fn targets)
                              (when after-active-player
                                (resolve-ability state side eid after-active-player nil nil))
                              (clear-wait-prompt state opponent)
                              (show-wait-prompt state active-player
                                                (str (side-str opponent) " to resolve " (event-title event) " triggers")
                                                {:priority -1})
                              (wait-for (trigger-event-simult-player state opponent (make-eid state eid) opponent-events cancel-fn targets)
                                        (clear-wait-prompt state active-player)
                                        (effect-completed state side eid))))))))

;; EVENT QUEUEING

(defn queue-event
  ([state event] (queue-event state event nil))
  ([state event context-map]
   (when (keyword? event)
     (swap! state update-in [:queued-events event] conj (assoc context-map :event event)))))

(defn make-pending-event
  [state event card ability]
  (let [ability {:event event
                 :duration :pending
                 :unregister-once-resolved true
                 :once-per-instance (or (:once-per-instance ability) false)
                 :ability ability
                 :card card
                 :uuid (uuid/v1)}]
    (swap! state update :events conj ability)))

(defn- gather-queued-event-handlers
  [state event-maps]
  (for [[event context-maps] event-maps]
    {:handlers (filterv #(= event (:event %)) (:events @state))
     :context-maps (into [] context-maps)}))

(defn- create-instances
  [{:keys [handlers context-maps]}]
  (apply concat
         (for [handler handlers]
           (if (:once-per-instance handler)
             [{:handler handler
               :context context-maps}]
             (for [context context-maps]
               {:handler handler
                :context [context]})))))

(defn- create-handlers
  [state eid event-maps]
  (->> (gather-queued-event-handlers state event-maps)
       (mapcat create-instances)
       (filter (fn [{:keys [handler context]}]
                 (let [card (card-for-ability state handler)
                       ability (:ability handler)]
                   (and (not (apply trigger-suppress state (to-keyword (:side card)) (:event handler) card context))
                        (can-trigger? state (to-keyword (:side card)) eid ability card context)))))
       (sort-by (complement #(is-active-player state (:handler %))))
       (seq)))

(defn- trigger-queued-event-player
  [state side eid handlers {:keys [cancel-fn] :as args}]
  (if (empty? handlers)
    (effect-completed state nil eid)
    (let [handlers (when-not (and cancel-fn (cancel-fn state))
                     (filter #(let [card (card-for-ability state (:handler %))]
                                (and card
                                     (not (:disabled card))
                                     (not (apply trigger-suppress state (to-keyword (:side card))
                                                 (get-in % [:handler :event]) card (:context %)))))
                             handlers))
          non-silent (filter #(let [silent-fn (:silent (:ability (:handler %)))]
                                (not (and silent-fn
                                          (silent-fn state side
                                                     (make-eid state eid)
                                                     (card-for-ability state (:handler %))
                                                     (:context %)))))
                             handlers)
          titles (keep #(card-for-ability state (:handler %)) non-silent)
          interactive (filter #(let [interactive-fn (:interactive (:ability (:handler %)))]
                                 (and interactive-fn
                                      (interactive-fn state side
                                                      (make-eid state eid)
                                                      (card-for-ability state (:handler %))
                                                      (:context %))))
                              handlers)]
      (if (or (= 1 (count handlers))
              (empty? interactive)
              (= 1 (count non-silent)))
        (let [handler (first handlers)
              to-resolve (:handler handler)
              ability (:ability to-resolve)
              context (:context handler)
              ability-card (card-for-ability state to-resolve)]
          (if ability-card
            (wait-for (resolve-ability state (to-keyword (:side ability-card))
                                       (make-eid state (assoc eid :source ability-card :source-type :ability))
                                       (dissoc-req ability)
                                       ability-card
                                       context)
                      (when (:unregister-once-resolved to-resolve)
                        (unregister-event-by-uuid state side (:uuid to-resolve)))
                      (trigger-queued-event-player state side eid (rest handlers) args))
            (trigger-queued-event-player state side eid (rest handlers) args)))
        (continue-ability
          state side
          (when (pos? (count handlers))
            {:async true
             :prompt "Choose a trigger to resolve"
             :choices titles
             :effect (req (let [handler (some #(when (same-card? target (card-for-ability state (:handler %))) %) handlers)
                                to-resolve (:handler handler)
                                ability (:ability to-resolve)
                                context (:context handler)
                                ability-card (card-for-ability state to-resolve)]
                            (wait-for
                              (resolve-ability state (to-keyword (:side ability-card))
                                               (make-eid state (assoc eid :source ability-card :source-type :ability))
                                               (dissoc-req ability)
                                               ability-card
                                               context)
                              (when (:unregister-once-resolved to-resolve)
                                (unregister-event-by-uuid state side (:uuid to-resolve)))
                              (let [handlers (remove-once #(same-card? target (card-for-ability state (:handler %))) handlers)]
                                (trigger-queued-event-player state side eid handlers args)))))})
          nil nil)))))

(defn- is-player
  [player {:keys [handler]}]
  (or (= player (get-side handler))
      (= player (get-ability-side handler))))

(defn- filter-handlers
  [handlers player-side]
  (filterv #(is-player player-side %) handlers))

(defn- mark-pending-abilities
  [state eid args]
  (let [event-maps (:queued-events @state)]
    (doseq [[event context-map] event-maps]
      (log-event state event context-map))
    (when-not (empty? event-maps)
      (let [handlers (create-handlers state eid event-maps)]
        (swap! state assoc
               :queued-events {}
               :events (->> (:events @state)
                            (remove #(= :pending (:duration %)))
                            (into [])))
        {:handlers handlers
         :context-maps (apply concat (vals event-maps))}))))

(defn- trigger-pending-abilities
  [state eid handlers args]
  (if (seq handlers)
    (let [active-player (:active-player @state)
          opponent (other-side active-player)
          active-player-handlers (filter-handlers handlers active-player)
          opponent-handlers (filter-handlers handlers opponent)]
      (show-wait-prompt state opponent (str (side-str active-player) " to resolve pending triggers"))
      (wait-for (trigger-queued-event-player state active-player (make-eid state eid) active-player-handlers args)
                (clear-wait-prompt state opponent)
                (show-wait-prompt state active-player (str (side-str opponent) " to resolve pending triggers"))
                (wait-for (trigger-queued-event-player state opponent (make-eid state eid) opponent-handlers args)
                          (clear-wait-prompt state active-player)
                          (effect-completed state nil eid))))
    (effect-completed state nil eid)))

;; CHECKPOINT
(defn internal-trash-cards
  [state _ eid maps]
  (if (seq maps)
    (let [{:keys [card value]} (first maps)]
      (wait-for (value state nil (make-eid state eid) card)
                (internal-trash-cards state nil eid (next maps))))
    (effect-completed state nil eid)))

(defn trash-when-expired
  [state _ eid context-maps]
  (if (seq context-maps)
    (->> context-maps
         (get-effect-maps state nil eid :trash-when-expired)
         (internal-trash-cards state nil eid))
    (effect-completed state nil eid)))

(defn unregister-expired-durations
  [state _ eid duration context-maps]
  (wait-for (trash-when-expired state nil (make-eid state eid) context-maps)
            (if duration
              (unregister-floating-effects state nil duration)
              (unregister-floating-events state nil duration))
            (effect-completed state nil eid)))

(defn checkpoint
  ([state eid] (checkpoint state nil eid nil))
  ([state _ eid] (checkpoint state nil eid nil))
  ([state _ eid {:keys [duration] :as args}]
   ;; a: Any ability that has met its condition creates the appropriate instances of itself and marks them as pending
   (let [{:keys [handlers context-maps]} (mark-pending-abilities state eid args)]
     ;; b: Any ability with a duration that has passed is removed from the game state
     (wait-for
       (unregister-expired-durations state nil (make-eid state eid) duration context-maps)
       ;; c: Check winning or tying by agenda points
       (check-win-by-agenda state)
       ;; d: uniqueness check
       ;; unimplemented
       ;; e: restrictions on card abilities or game rules, MU
       ;; unimplemented
       ;; f: stuff on agendas moved from score zone
       ;; unimplemented
       ;; g: stuff on installed cards that were trashed
       ;; unimplemented
       ;; h: empty servers
       (clear-empty-remotes state)
       ;; i: card counters/agendas become cards again
       ;; unimplemented
       ;; j: counters in discard are returned to the bank
       ;; unimplemented
       ;; 10.3.2: reaction window
       (trigger-pending-abilities state eid handlers args)))))

(defn end-of-phase-checkpoint
  ([state _ eid event] (end-of-phase-checkpoint state nil eid event nil))
  ([state _ eid event context]
   (when event
     (queue-event state event context))
   (checkpoint state nil eid {:duration event})))

;; PAYMENT

(defn- pay-next
  [state side eid costs card actions msgs]
  (if (empty? costs)
    (complete-with-result state side eid msgs)
    (wait-for (handler (first costs) state side (make-eid state eid) card actions)
              (pay-next state side eid (rest costs) card actions (conj msgs async-result)))))

(defn- sentence-join
  [strings]
  (if (<= (count strings) 2)
    (string/join " and " strings)
    (str (apply str (interpose ", " (butlast strings))) ", and " (last strings))))

(defn pay
  "Same as pay, but awaitable."
  [state side eid card & args]
  (let [args (flatten args)
        raw-costs (remove map? args)
        actions (filter map? args)]
    (if-let [costs (can-pay? state side eid card (:title card) raw-costs)]
      (wait-for (pay-next state side (make-eid state eid) costs card actions [])
                (let [payment-result async-result]
                  (wait-for (checkpoint state nil (make-eid state eid) nil)
                            (complete-with-result
                              state side eid
                              {:msg (->> payment-result
                                         (keep :msg)
                                         sentence-join)
                               :cost-paid (->> payment-result
                                               (keep #(not-empty (select-keys % [:type :targets :value])))
                                               (reduce
                                                 (fn [acc cost]
                                                   (assoc acc (:type cost) cost))
                                                 {}))}))))
      (complete-with-result state side eid nil))))
