(in-ns 'game.core)

(declare can-trigger? event-title
         register-suppress resolve-ability
         unregister-suppress)

(defn default-locations
  [card]
  (case (to-keyword (:type card))
    :agenda [:scored]
    (:asset :ice :upgrade) [:servers]
    (:event :operation) [:current :play-area]
    (:hardware :program :resource) [:rig]
    (:identity :fake-identity) [:identity]))

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
                 :location (or (:location ability) (default-locations card))
                 :duration (or (:duration ability) :while-installed)
                 :condition (or (:condition ability) :active)
                 :unregister-once-resolved (or (:unregister-once-resolved ability) false)
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
  [state side duration]
  (when (not= :while-installed duration)
    (swap! state assoc :events
           (->> (:events @state)
                (remove #(= duration (:duration %)))
                (into [])))))

(defn unregister-floating-events-for-card
  "Removes all event handlers with a non-persistent duration on a single card"
  [state side card duration]
  (swap! state assoc :events
         (->> (:events @state)
              (remove #(and (same-card? card (:card %))
                            (= duration (:duration %))))
              (into []))))

(defn unregister-event-by-uuid
  "Removes a single event handler with matching uuid"
  [state side uuid]
  (swap! state assoc :events (remove-once #(= uuid (:uuid %)) (:events @state))))

; Functions for registering trigger suppression events.
(defn register-suppress
  "Registers each suppression handler in the given card definition. Suppression handlers
  can prevent the dispatching of a particular event."
  ([state side card] (register-suppress state side card (:suppress (card-def card))))
  ([state side card events]
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
  ([state side card events]
   (let [abilities (map :event events)]
     (swap! state assoc :suppress
            (->> (:suppress @state)
                 (remove #(and (same-card? card (:card %))
                               (in-coll? abilities (:event %))))
                 (into []))))))

(defn unregister-suppress-by-uuid
  "Removes a single event handler with matching uuid"
  [state side uuid]
  (swap! state assoc :suppress (remove-once #(= uuid (:uuid %)) (:suppress @state))))

(declare card-for-ability)

(defn trigger-suppress
  "Returns true if the given event on the given targets should be suppressed, by triggering
  each suppression handler and returning true if any suppression handler returns true."
  [state side event & targets]
  (->> (:suppress @state)
       (filter #(= event (:event %)))
       (some #((:req (:ability %)) state side (make-eid state) (card-for-ability state %) targets))))

;; triggering events
(defn- get-side
  [ability]
  (-> ability :card :side to-keyword))

(defn- is-active-player
  [state ability]
  (= (:active-player @state) (get-side ability)))

(defn card-for-ability
  [state ability]
  (if (#{:while-installed :pending} (:duration ability))
    (when-let [card (get-card state (:card ability))]
      (if (and (= :while-installed (:duration ability))
               (= (default-locations card) (:location ability)))
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
          nil)
        card))
     (:card ability)))

(defn gather-events
  "Prepare the list of the given player's handlers for this event.
  Gather all registered handlers from the state, then append the card-abilities if appropriate,
  then filter to remove suppressed handlers and those whose req is false.
  This is essentially Phase 9.3 and 9.6.7a of CR 1.1:
  http://nisei.net/files/Comprehensive_Rules_1.1.pdf"
  ([state side event targets] (gather-events state side event targets nil))
  ([state side event targets card-abilities]
   (->> (:events @state)
        (filter #(= event (:event %)))
        (concat card-abilities)
        (filter identity)
        (filter (fn [ability]
                  (let [card (card-for-ability state ability)]
                    (and (not (apply trigger-suppress state side event card targets))
                         (can-trigger? state side (make-eid state) (:ability ability) card targets)))))
        (sort-by (complement #(is-active-player state %)))
        doall)))

(defn log-event
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
    (let [get-side #(-> % :card :side game.utils/to-keyword)
          is-active-player #(= (:active-player @state) (get-side %))
          handlers (gather-events state side event targets)]
      (doseq [to-resolve handlers]
        (when-let [card (card-for-ability state to-resolve)]
          (resolve-ability state side (dissoc (:ability to-resolve) :req) card targets)
          (when (:unregister-once-resolved to-resolve)
            (unregister-event-by-uuid state side (:uuid to-resolve))))))))

(defn- trigger-event-sync-next
  [state side eid handlers event targets]
  (if-let [to-resolve (first handlers)]
    (if-let [card (card-for-ability state to-resolve)]
      (wait-for (resolve-ability state side (make-eid state eid) (dissoc (:ability to-resolve) :req) card targets)
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
        (let [get-side #(-> % :card :side game.utils/to-keyword)
              is-active-player #(= (:active-player @state) (get-side %))
              handlers (gather-events state side event targets)]
          (trigger-event-sync-next state side eid handlers event targets)))))

(defn- trigger-event-simult-player
  "Triggers the simultaneous event handlers for the given event trigger and player.
  If none of the handlers require interaction, then they are all resolved automatically, each waiting for the previous
  to fully resolve as in trigger-event-sync. If at least one requires interaction, then a menu is shown to manually
  choose the order of resolution.

  :silent abilities are not shown in the list of handlers, and are resolved last in an arbitrary order."
  [state side eid event handlers cancel-fn event-targets]
  (if (not-empty handlers)
    (letfn [;; Allow resolution as long as there is no cancel-fn, or if the cancel-fn returns false.
            (should-continue [state handlers]
              (and (< 1 (count handlers))
                   (not (and cancel-fn (cancel-fn state)))))
            (choose-handler [handlers]
              (let [handlers (when-not (and cancel-fn (cancel-fn state))
                               (filter #(card-for-ability state %) handlers))
                    non-silent (filter #(let [silent-fn (:silent (:ability %))]
                                          (not (and silent-fn
                                                    (silent-fn state side (make-eid state) (:card %) event-targets))))
                                       handlers)
                    titles (map :card non-silent)
                    interactive (filter #(let [interactive-fn (:interactive (:ability %))]
                                           (and interactive-fn
                                                (interactive-fn state side (make-eid state) (:card %) event-targets)))
                                        handlers)]
                ;; If there is only 1 non-silent ability, resolve that then recurse on the rest
                (if (or (= 1 (count handlers)) (empty? interactive) (= 1 (count non-silent)))
                  (let [to-resolve (if (= 1 (count non-silent))
                                     (first non-silent)
                                     (first handlers))
                        ability-to-resolve (dissoc (:ability to-resolve) :req)
                        others (if (= 1 (count non-silent))
                                 (remove-once #(= (get-cid to-resolve) (get-cid %)) handlers)
                                 (rest handlers))]
                    (if-let [the-card (card-for-ability state to-resolve)]
                      {:async true
                       :effect (req (wait-for (resolve-ability state (to-keyword (:side the-card))
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
                                      ability-to-resolve (dissoc (:ability to-resolve) :req)
                                      the-card (card-for-ability state to-resolve)]
                                  (wait-for
                                    (resolve-ability state (to-keyword (:side the-card))
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
  [state side eid event {:keys [first-ability card-abilities after-active-player cancel-fn] :as options} & targets]
  (if (nil? event)
    (effect-completed state side eid)
    (do (log-event state event targets)
        (let [get-ability-side #(-> % :ability :side)
              active-player (:active-player @state)
              opponent (other-side active-player)
              is-player (fn [player ability]
                          (or (= player (get-side ability))
                              (= player (get-ability-side ability))))
              card-abilities (if (and (some? card-abilities)
                                      (not (sequential? card-abilities)))
                               [card-abilities]
                               card-abilities)
              handlers (gather-events state side event targets card-abilities)
              get-handlers (fn [player-side]
                             (filterv (partial is-player player-side) handlers))
              active-player-events (get-handlers active-player)
              opponent-events (get-handlers opponent)]
          (wait-for (resolve-ability state side (make-eid state eid) first-ability nil nil)
                    (show-wait-prompt state opponent
                                      (str (side-str active-player) " to resolve " (event-title event) " triggers")
                                      {:priority -1})
                    ; let active player activate their events first
                    (wait-for (trigger-event-simult-player state side (make-eid state eid) event active-player-events cancel-fn targets)
                              (when after-active-player
                                (resolve-ability state side eid after-active-player nil nil))
                              (clear-wait-prompt state opponent)
                              (show-wait-prompt state active-player
                                                (str (side-str opponent) " to resolve " (event-title event) " triggers")
                                                {:priority -1})
                              (wait-for (trigger-event-simult-player state opponent (make-eid state eid) event opponent-events cancel-fn targets)
                                        (clear-wait-prompt state active-player)
                                        (effect-completed state side eid))))))))

;; Functions for event parsing
(defn turn-events
  "Returns the targets vectors of each event with the given key that was triggered this turn."
  [state side ev]
  (mapcat rest (filter #(= ev (first %)) (:turn-events @state))))

(defn last-turn? [state side event]
  (if (-> @state side :register-last-turn event) true false))

(defn not-last-turn? [state side event]
  (cond

    ; Return false if no previous turn (i.e. turn 1).
    (-> @state side :register-last-turn nil?)
    false

    (-> @state side :register-last-turn event)
    false

    :else
    true))

(defn no-event?
  "Returns true if the given event has not happened yet this turn.
  Filters on events satisfying (pred targets) if given pred."
  ([state side ev] (no-event? state side ev (constantly true)))
  ([state side ev pred]
   (empty? (filter pred (turn-events state side ev)))))

(defn event-count
  "Returns the number of an event this turn."
  ([state side ev] (event-count state side ev (constantly true)))
  ([state side ev pred]
   (count (filter pred (turn-events state side ev)))))

(defn first-event?
  "Returns true if the given event has only occured once this turn.
  Includes itself if this is checked in the requirement for an event ability.
  Filters on events satisfying (pred targets) if given pred."
  ([state side ev] (first-event? state side ev (constantly true)))
  ([state side ev pred]
   (= 1 (event-count state side ev pred))))

(defn second-event?
  "Returns true if the given event has occurred twice this turn.
  Includes itself if this is checked in the requirement for an event ability.
  Filters on events satisfying (pred targets) if given pred."
  ([state side ev] (second-event? state side ev (constantly true)))
  ([state side ev pred]
   (= 2 (event-count state side ev pred))))

(defn first-successful-run-on-server?
  "Returns true if the active run is the first succesful run on the given server"
  [state server]
  (first-event? state :runner :successful-run #(= [server] %)))

(defn first-trash?
  "Returns true if cards have been trashed by either player only once this turn.
  Includes itself if this is checked in the requirement for an event ability.
  Filters on trash events satisfying (pred targets) if given pred.
  Note that trash event targets may be optionally followed by a reason for the trash, or nil."
  ([state] (first-trash? state (constantly true)))
  ([state pred]
   (= 1 (+ (event-count state nil :runner-trash pred)
           (event-count state nil :corp-trash pred)))))

(defn get-turn-damage
  "Returns the value of damage take this turn"
  [state side]
  (apply + (map #(nth % 2) (turn-events state :runner :damage))))

(defn get-installed-trashed
  "Returns list of cards trashed this turn owned by side that were installed"
  [state side]
  (filter #(-> % first installed?) (turn-events state side (keyword (str (name side) "-trash")))))

(defn first-installed-trash?
  "Returns true if this is the first trash of an installed card this turn by this side"
  [state side]
  (= 1 (count (get-installed-trashed state side))))

(defn first-installed-trash-own?
  "Returns true if this is the first trash of an owned installed card this turn by this side"
  [state side]
  (= 1 (count (filter #(= (:side (first %)) (side-str side)) (get-installed-trashed state side)))))


;; Functions for run event parsing
(defn run-events
  "Returns the targets vectors of each run event with the given key that was triggered this run."
  [state side ev]
  (when (:run @state)
    (mapcat rest (filter #(= ev (first %)) (get-in @state [:run :events])))))

(defn no-run-event?
  "Returns true if the given run event has not happened yet this run.
  Filters on run events satisfying (pred targets) if given pred."
  ([state side ev] (no-run-event? state side ev (constantly true)))
  ([state side ev pred]
   (empty? (filter pred (run-events state side ev)))))

(defn run-event-count
  "Returns the number of times a run event has happened this run."
  ([state side ev] (run-event-count state side ev (constantly true)))
  ([state side ev pred]
   (count (filter pred (run-events state side ev)))))

(defn first-run-event?
  "Returns true if the given run event has only occured once this run.
  Includes itself if this is checked in the requirement for a run event ability.
  Filters on run events satisfying (pred targets) if given pred."
  ([state side ev] (first-run-event? state side ev (constantly true)))
  ([state side ev pred]
   (= 1 (run-event-count state side ev pred))))
