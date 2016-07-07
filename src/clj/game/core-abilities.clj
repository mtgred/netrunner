(in-ns 'game.core)
(declare corp-trace-prompt optional-ability
         check-optional check-psi check-trace complete-ability
         can-trigger? do-choices do-ability
         psi-game resolve-ability-eid resolve-psi resolve-trace show-select)

;;;; Functions for implementing card abilities and prompts

;;; Abilities
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

  OTHER KEYS
  :counter-cost / :advance-counter-cost -- number of counters to remove to resolve the ability
  :once -- its only value is :per-turn; signifies an effect that can only be triggered once per turn.
  :once-key -- by default, each :once is distinct per card. If multiple copies of a card can only resolve
               some ability once between all of them, then the card should specify a manual :once-key that can
               be any value, preferrably a unique keyword.
  :optional -- shows a 'Yes/No' prompt to let the user decide whether to resolve the ability.
  :end-turn -- if the ability is resolved, then this ability map will be resolved at the end of the turn."

  ;; perhaps the most important function in the game logic
  ([state side {:keys [eid] :as ability} card targets]
   (if eid
     (resolve-ability-eid state side (assoc ability :eid eid) card targets)
     (resolve-ability-eid state side (assoc ability :eid (or eid (make-eid state))) card targets)))
  ([state side eid ability card targets]
   (resolve-ability-eid state side (assoc ability :eid eid) card targets)))

(defn- resolve-ability-eid
  ([state side {:keys [eid] :as ability} card targets]
   (if (= 1 (count ability)) ;; only has the eid, in effect a nil ability
     (effect-completed state side eid card)
     (if (and ability (not eid))
       (resolve-ability-eid state side (assoc ability :eid (make-eid state)) card targets)
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
           (effect-completed state side eid card))
         (complete-ability state side ability card))))))

;;; Checking functions for resolve-ability
(defn- complete-ability
  [state side {:keys [eid choices optional prompt delayed-completion psi trace] :as ability} card]
  ;if it doesn't have choices and it doesn't have a true delayed-completion; or
  ;if it does have choices and has false delayed-completion
  (when (or (and (not choices) (not optional) (not psi) (not trace) (not delayed-completion))
            (and (or optional psi choices trace) (false? delayed-completion)))
    (effect-completed state side eid card)))

(defn- check-req
  "Check if the requirement is fulfilled, or no requirement present"
  [state side card targets {:keys [eid] :as ability}]
  (if-let [req (:req ability)]
    (req state side eid card targets)
    ;; return true if no requirement present
    true))

(defn- check-optional
  "Checks if there is an optional ability to resolve"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [optional (:optional ability)]
    (if (can-trigger? state side optional card targets)
      (optional-ability state (or (:player optional) side) eid card (:prompt optional) optional targets)
      (effect-completed state side eid card))))

(defn- check-psi
  "Checks if a psi-game is to be resolved"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [psi (:psi ability)]
    (if (can-trigger? state side psi card targets)
      (psi-game state side eid card psi)
      (effect-completed state side eid card))))

(defn- check-trace
  "Checks if there is a trace to resolve"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [trace (:trace ability)]
    (if (can-trigger? state side trace card targets)
      (corp-trace-prompt state card (assoc trace :eid (:eid ability)))
      (effect-completed state side eid card))))

(defn- can-trigger?
  "Checks if ability can trigger. Checks that once-per-turn is not violated."
  [state side {:keys [once once-key] :as ability} card targets]
  (let [cid (:cid card)]
    (and (not (get-in @state [once (or once-key cid)]))
         (check-req state side card targets ability))))

(defn- do-choices
  "Handle a choices ability"
  [state side {:keys [choices player priority cancel-effect not-distinct prompt eid] :as ability}
   card targets]
  (let [s (or player side)
        ab (dissoc ability :choices)
        args {:priority priority :cancel-effect cancel-effect}]
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
       (let [n ((:number choices) state side eid card targets)]
         (prompt! state s card prompt {:number n} ab args))
       (:card-title choices)
       (prompt!
         state s card prompt
         (assoc choices :autocomplete
                        (sort (map :title (filter #((:card-title choices) state side (make-eid state) nil [%]) @all-cards))))
         ab (assoc args :prompt-type :card-title))
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
  [state side {:keys [cost counter-cost advance-counter-cost] :as ability}
   {:keys [counter advance-counter] :as card} targets]
  ;; Ensure counter costs can be paid
  (when (and (or (not counter-cost)
                 (<= (or (second counter-cost) 0)
                     (get-in card [:counter (first counter-cost)] 0)))
             (or (not advance-counter-cost)
                 (<= advance-counter-cost (or advance-counter 0))))
    ;; Ensure that any costs can be paid.
    (when-let [cost-str (apply pay (concat [state side card] cost))]
      (let [c (if counter-cost
                (update-in card [:counter (first counter-cost)]
                           #(- (or % 0) (or (second counter-cost) 0)))
                card)
            c (if advance-counter-cost
                  (update-in c [:advance-counter] #(- (or % 0) (or advance-counter-cost 0)))
                  c)]
        ;; Remove any counters.
        (when (or counter-cost advance-counter-cost)
          (update! state side c)
          (when (is-type? card "Agenda")
            (trigger-event state side :agenda-counter-spent card)))
        ;; Print the message.
        (print-msg state side ability card targets cost-str)
        ;; Trigger the effect.
        (do-effect state side ability c targets)
        ;; Record the :end-turn effect.
        (register-end-turn state side ability card targets))
      ;; Record the ability has been triggered if it is restricted to happening once..
      (register-once state ability card))))

(defn- print-msg
  "Prints the ability message"
  [state side {:keys [eid] :as ability} card targets cost-str]
  (when-let [msg (:msg ability)]
    (when-let [desc (if (string? msg) msg (msg state side eid card targets))]
      (system-msg state (to-keyword (:side card))
                  (str (build-spend-msg cost-str "use")
                       (:title card) (when desc (str " to " desc)))))))

(defn- do-effect
  "Trigger the effect"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [effect (:effect ability)]
    (effect state side eid card targets)))

(defn- register-end-turn
  "Register :end-turn effect if present"
  [state side {:keys [eid] :as ability} card targets]
  (when-let [end-turn (:end-turn ability)]
    (swap! state update-in [side :register :end-turn]
           #(conj % {:ability end-turn :card card :targets targets :eid eid}))))

(defn- register-once
  "Register ability as having happened if :once specified"
  [state {:keys [once once-key] :as ability} {:keys [cid] :as card}]
  (when once (swap! state assoc-in [once (or once-key cid)] true)))


;;; Optional Ability
(defn optional-ability
  "Shows a 'Yes/No' prompt and resolves the given ability if Yes is chosen."
  ([state side card msg ability targets] (optional-ability state side (make-eid state) card msg ability targets))
  ([state side eid card msg ability targets]
   (show-prompt state side eid card msg ["Yes" "No"]
                #(let [yes-ability (:yes-ability ability)]
                  (if (and (= % "Yes")
                           yes-ability
                           (can-pay? state side (:title card) (:cost yes-ability)))
                    (resolve-ability state side (assoc yes-ability :eid eid) card targets)
                    (if-let [no-ability (:no-ability ability)]
                      (resolve-ability state side (assoc no-ability :eid eid) card targets)
                      (effect-completed state side eid card))))
                ability)))


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
  ([state side card msg choices ability] (prompt! state side card msg choices ability nil))
  ([state side card msg choices ability args]
   (letfn [(wrap-function [args kw]
             (let [f (kw args)] (if f (assoc args kw #(f state side (:eid ability) card [%])) args)))]
       (show-prompt state side (:eid ability) card msg choices #(resolve-ability state side ability card [%])
                    (-> args
                        (wrap-function :cancel-effect)
                        (wrap-function :end-effect))))))

(defn show-prompt
  "Engine-private method for displaying a prompt where a *function*, not a card ability, is invoked
  when the prompt is resolved. All prompts flow through this method."
  ([state side card msg choices f] (show-prompt state side (make-eid state) card msg choices f nil))
  ([state side card msg choices f args] (show-prompt state side (make-eid state) card msg choices f args))
  ([state side eid card msg choices f
    {:keys [priority prompt-type show-discard cancel-effect end-effect] :as args}]
   (let [prompt (if (string? msg) msg (msg state side nil card nil))
         priority-comp #(case % true 1 nil 0 %)
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
       (swap! state update-in [side :prompt]
              ;; insert the new prompt into the already-sorted queue based on its priority.
              #(let [[head tail] (split-with (fn [p] (>= (priority-comp (:priority p)) (priority-comp priority))) %)]
                (concat head (cons newitem tail))))))))

(defn show-select
  "A select prompt uses a targeting cursor so the user can click their desired target of the ability.
  As with prompt!, the preferred method for showing a select prompt is through resolve-ability."
  ([state side card ability] (show-select state side card ability nil))
  ([state side card ability args]
   ;; if :max is a function, call it and assoc its return value as the new :max number of cards
   ;; that can be selected.
   (letfn [(wrap-function [args kw]
             (let [f (kw args)] (if f (assoc args kw #(f state side (:eid ability) card [%])) args)))]
     (let [ability (update-in ability [:choices :max] #(if (fn? %) (% state side (make-eid state) card nil) %))]
     (swap! state update-in [side :selected]
            #(conj (vec %) {:ability (dissoc ability :choices) :req (get-in ability [:choices :req])
                            :max (get-in ability [:choices :max])}))
     (show-prompt state side card
                  (if-let [msg (:prompt ability)]
                    msg
                    (if-let [m (get-in ability [:choices :max])]
                      (str "Select up to " m " targets for " (:title card))
                      (str "Select a target for " (:title card))))
                  ["Done"] (fn [choice] (resolve-select state side))
                  (-> args
                      (assoc :prompt-type :select :show-discard (:show-discard ability))
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
        (effect-completed state side (:eid (:ability selected)) nil)))))

(defn show-wait-prompt
  "Shows a 'Waiting for ...' prompt to the given side with the given message.
  The prompt cannot be closed except by a later call to clear-wait-prompt.
  The prompt has default priority 1, but can be overridden."
  ([state side msg] (show-wait-prompt state side msg {:priority 1}))
  ([state side msg {:keys [priority] :as args}]
   (show-prompt state side nil (str "Waiting for " msg) nil
                (fn [c] (system-msg state side (str "is waiting for " msg))) ; this function is never called, because the prompt has no button.
                {:priority priority :prompt-type :waiting})))

(defn clear-wait-prompt
  "Removes the first 'Waiting for...' prompt from the given side's prompt queue."
  [state side]
  (when-let [wait (some #(when (= :waiting (:prompt-type %)) %) (-> @state side :prompt))]
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % wait) pr)))))

;;; Psi games
(defn psi-game
  "Starts a psi game by showing the psi prompt to both players. psi is a map containing
  :equal and :not-equal abilities which will be triggered in resolve-psi accordingly."
  ([state side card psi] (psi-game state side (make-eid state) card psi))
  ([state side eid card psi]
   (swap! state assoc :psi {})
   (register-once state psi card)
   (doseq [s [:corp :runner]]
     (show-prompt state s card (str "Choose an amount to spend for " (:title card))
                  (map #(str % " [Credits]") (range (min 3 (inc (get-in @state [s :credit])))))
                  #(resolve-psi state s eid card psi (Integer/parseInt (first (split % #" "))))
                  {:priority 2}))))

(defn resolve-psi
  "Resolves a psi game by charging credits to both sides and invoking the appropriate
  resolution ability."
  [state side eid card psi bet]
  (swap! state assoc-in [:psi side] bet)
  (let [opponent (if (= side :corp) :runner :corp)]
    (if-let [opponent-bet (get-in @state [:psi opponent])]
      (do (clear-wait-prompt state opponent)
          (gain state opponent :credit (- opponent-bet))
          (system-msg state opponent (str "spends " opponent-bet " [Credits]"))
          (gain state side :credit (- bet))
          (system-msg state side (str "spends " bet " [Credits]"))
          (trigger-event state side (keyword (str "psi-bet-" (name side))) bet)
          (trigger-event state side (keyword (str "psi-bet-" (name opponent))) opponent-bet)
          (trigger-event state side :psi-game nil)
          (if-let [ability (if (= bet opponent-bet) (:equal psi) (:not-equal psi))]
            (resolve-ability state (:side card) (assoc ability :eid eid :delayed-completion true) card nil)
            (effect-completed state side eid card)))
      (show-wait-prompt
        state side (str (clojure.string/capitalize (name opponent)) " to choose psi game credits")))))


;;; Traces
(defn init-trace
  "Shows a trace prompt to the runner, after the corp has already spent credits to boost."
  [state side card {:keys [base eid] :as ability} boost]
  (clear-wait-prompt state :runner)
  (show-wait-prompt state :corp "Runner to boost Link strength" {:priority 2})
  (trigger-event state side :pre-init-trace card)
  (let [bonus (or (get-in @state [:bonus :trace]) 0)
        base (if (fn? base) (base state side  (make-eid state) card nil) base)
        total (+ base boost bonus)]
    (system-msg state :corp (str "uses " (:title card)
                                 " to initiate a trace with strength " total
                                 " (" base
                                 (when (pos? bonus) (str " + " bonus " bonus"))
                                 " + " boost " [Credits]) (" (make-label ability) ")"))
    (swap! state update-in [:bonus] dissoc :trace)
    (show-prompt state :runner card (str "Boost link strength?") :credit #(resolve-trace state side eid %) {:priority 2})
    (swap! state assoc :trace {:strength total :ability ability :card card})
    (trigger-event state side :trace nil)))

(defn resolve-trace
  "Compares trace strength and link strength and triggers the appropriate effects."
  [state side eid boost]
  (clear-wait-prompt state :corp)
  (let [runner (:runner @state)
        {:keys [strength ability card]} (:trace @state)]
    (system-msg state :runner (str " spends " boost " [Credits] to increase link strength to "
                                   (+ (:link runner) boost)))
    (let [succesful (> strength (+ (:link runner) boost))
          which-ability (assoc (if succesful ability (:unsuccessful ability)) :eid (make-eid state))]
      (when-completed (resolve-ability state :corp (:eid which-ability) which-ability
                                       card [strength (+ (:link runner) boost)])
                      (do (trigger-event state :corp (if succesful :successful-trace :unsuccessful-trace))
                          (when-let [kicker (:kicker ability)]
                            (when (>= strength (:min kicker))
                              (resolve-ability state :corp kicker card [strength (+ (:link runner) boost)])))
                          (effect-completed state side eid nil))))))

(defn corp-trace-prompt
  "Starts the trace process by showing the boost prompt to the corp."
  [state card trace]
  (show-wait-prompt state :runner (str "Corp to initiate a trace from " (:title card)) {:priority 2})
  (show-prompt state :corp card "Boost trace strength?" :credit
               #(init-trace state :corp card trace %) {:priority 2}))
