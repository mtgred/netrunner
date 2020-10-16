(ns game.core.resolve-ability
  (:require
    [clojure.stacktrace :refer [print-stack-trace]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.payment :refer [build-spend-msg merge-costs pay]]
    [game.core.prompts :refer [show-prompt show-select]]
    [game.core.say :refer [system-msg]]
    [game.core.update :refer [update!]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [distinct-by server-cards to-keyword]]))

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

(declare do-ability resolve-ability-eid check-ability prompt! check-choices do-choices)

(def ability-types (atom {}))

(defn register-ability-type
  [kw ability-fn]
  (swap! ability-types assoc kw ability-fn))

(defn select-ability-kw
  [ability]
  (first (keys (select-keys ability (keys @ability-types)))))

;;; Ability related function
(defn should-trigger?
  "Checks if the specified ability definition should trigger.
  Checks for a :req, either in the top level map, or in an :optional or :psi sub-map
  Returns true if no :req found, returns nil if the supplied ability is nil"
  ([state side eid card targets {:keys [req] :as ability}]
   (when ability
     (let [ab (select-ability-kw ability)]
       (cond
         req (req state side eid card targets)
         ab (should-trigger? state side eid card targets (get ability ab))
         :else true)))))

(defn not-used-once?
  [state {:keys [once once-key]} {:keys [cid]}]
  (not (get-in @state [once (or once-key cid)])))

(defn can-trigger?
  "Checks if ability can trigger. Checks that once-per-turn is not violated."
  [state side eid ability card targets]
  (and (not-used-once? state ability card)
       (should-trigger? state side eid card targets ability)))

(defn is-ability?
  "Checks to see if a given map represents a card ability."
  [{:keys [effect msg]}]
  (or effect msg (seq (keys @ability-types))))

(defn resolve-ability
  ([state side {:keys [eid] :as ability} card targets]
   (resolve-ability state side (or eid (make-eid state {:source card :source-type :ability})) ability card targets))
  ([state side eid ability card targets]
   (resolve-ability-eid state side (assoc ability :eid eid) card targets)))

(defn- resolve-ability-eid
  [state side {:keys [eid choices] :as ability} card targets]
  (cond
    ;; Only has the eid, in effect a nil ability
    (and eid (= 1 (count ability)))
    (effect-completed state side eid)
    ;; This was called directly without an eid present
    (and ability (not eid))
    (resolve-ability-eid state side (assoc ability :eid (make-eid state eid)) card targets)
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
  [state side {:keys [eid] :as ability} card targets cost-str]
  (when-let [message (:msg ability)]
    (let [desc (if (string? message) message (message state side eid card targets))
          cost-spend-msg (build-spend-msg cost-str "use")]
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

(defn- do-ability
  "Perform the ability, checking all costs can be paid etc."
  [state side {:keys [async eid cost] :as ability} card targets]
  ;; Ensure that any costs can be paid
  (wait-for (pay state side (make-eid state eid) card cost {:action (:cid card)})
            ;; If the cost can be and is paid, perform the ablity
            (if-let [cost-str async-result]
              (do
                ;; Print the message
                (print-msg state side ability card targets cost-str)
                ;; Trigger the effect
                (register-once state side ability card)
                (do-effect state side ability (ugly-counter-hack card cost) targets)
                ;; If the ability isn't async, complete it
                (when-not async
                  (effect-completed state side eid)))
              (effect-completed state side eid))))

(defn- do-choices
  "Handle a choices ability"
  [state side {:keys [choices eid not-distinct player prompt] :as ability} card targets]
  (let [s (or player side)
        ab (dissoc ability :choices)
        args (select-keys ability [:priority :cancel-effect :prompt-type :show-discard :end-effect])]
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
   (show-prompt state side (:eid ability) card message choices #(resolve-ability state side ability card [%])
                (if-let [f (:cancel-effect args)]
                  (assoc args :cancel-effect #(f state side (:eid ability) card [%]))
                  args))))
