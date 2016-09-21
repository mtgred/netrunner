(in-ns 'game.core)

(declare check-req clear-wait-prompt effect-completed event-title forfeit prompt! register-suppress
         show-wait-prompt trigger-suppress unregister-suppress)

; Functions for registering and dispatching events.
(defn register-events
  "Registers each event handler defined in the given card definition.
  Also registers any suppression events. (Crisium Grid.)"
  [state side events card]
  (doseq [e events]
    (swap! state update-in [:events (first e)] #(conj % {:ability (last e) :card card})))
  (register-suppress state side (:suppress (card-def card)) card))

(defn unregister-events
  "Removes all event handlers defined for the given card."
  [state side card]
  (let [cdef (card-def card)]
    ;; Combine normal events and derezzed events. Any merge conflicts should not matter
    ;; as they should cause all relevant events to be removed anyway.
    (doseq [e (merge (:events cdef) (:derezzed-events cdef))]
      (swap! state update-in [:events (first e)]
             #(remove (fn [effect] (= (get-in effect [:card :cid]) (:cid card))) %))))
  (unregister-suppress state side card))

(defn trigger-event
  "Resolves all abilities registered as handlers for the given event key, passing them
  the targets given."
  [state side event & targets]
  (let [get-side #(-> % :card :side game.utils/to-keyword)
        is-active-player #(= (:active-player @state) (get-side %))]
    (doseq [{:keys [ability] :as e} (sort-by (complement is-active-player) (get-in @state [:events event]))]
      (when-let [card (get-card state (:card e))]
        (when (and (not (apply trigger-suppress state side event (cons card targets)))
                   (or (not (:req ability)) ((:req ability) state side (make-eid state) card targets)))
          (resolve-ability state side ability card targets))))
    (swap! state update-in [:turn-events] #(cons [event targets] %))))

(defn- trigger-event-sync-next
  [state side eid handlers event & targets]
  (let [e (first handlers)
        ability (:ability e)]
    (if e
      (if-let [card (get-card state (:card e))]
        (if (and (not (apply trigger-suppress state side event (cons card targets)))
                 (or (not (:req ability)) ((:req ability) state side (make-eid state) card targets)))
          (when-completed (resolve-ability state side ability card targets)
                          (apply trigger-event-sync-next state side eid (next handlers) event targets))
          (apply trigger-event-sync-next state side eid (next handlers) event targets))
        (apply trigger-event-sync-next state side eid (next handlers) event targets))
      (do (swap! state update-in [:turn-events] #(cons [event targets] %))
          (effect-completed state side eid nil)))))

(defn trigger-event-sync
  "Triggers the given event synchronously, requiring each handler to complete before alerting the next handler. Does not
  give the user a choice of what order to resolve handlers."
  [state side eid event & targets]
  (let [get-side #(-> % :card :side game.utils/to-keyword)
        is-active-player #(= (:active-player @state) (get-side %))]

    (let [handlers (sort-by (complement is-active-player) (get-in @state [:events event]))
          card nil]
      (when-completed (apply trigger-event-sync-next state side handlers event targets)
                      (effect-completed state side eid nil)))))

(defn- trigger-event-simult-player
  "Triggers the simultaneous event handlers for the given event trigger and player.
  If none of the handlers require interaction, then they are all resolved automatically, each waiting for the previous
  to fully resolve as in trigger-event-sync. If at least one requires interaction, then a menu is shown to manually
  choose the order of resolution.

  :silent abilities are not shown in the list of handlers, and are resolved last in an arbitrary order."
  [state side eid event handlers event-targets]
  (if (pos? (count handlers))
    (letfn [(choose-handler [handlers]
              (let [non-silent (filter #(not (and (:silent (:ability %))
                                                  (let [ans ((:silent (:ability %)) state side (make-eid state) (:card %) event-targets)]
                                                    ans)))
                                       handlers)
                    cards (map :card non-silent)
                    titles (map :title cards)
                    interactive (filter #(let [interactive-fn (:interactive (:ability %))]
                                          (and interactive-fn (interactive-fn state side (make-eid state) (:card %) event-targets)))
                                        handlers)]
                ;; If there is only 1 non-silent ability, resolve that then recurse on the rest
                (if (or (= 1 (count handlers)) (empty? interactive) (= 1 (count non-silent)))
                  (let [to-resolve
                        (if (= 1 (count non-silent)) (first non-silent) (first handlers))
                        others (if (= 1 (count non-silent))
                                 (remove-once #(not= (:cid (:card to-resolve)) (:cid (:card %))) handlers)
                                 (next handlers))]
                    (if-let [the-card (get-card state (:card to-resolve))]
                      {:delayed-completion true
                       :effect (req (when-completed (resolve-ability state side (:ability to-resolve)
                                                                     the-card event-targets)
                                                    (if (< 1 (count handlers))
                                                      (continue-ability state side
                                                                        (choose-handler others) nil event-targets)
                                                      (effect-completed state side eid nil))))}
                      {:delayed-completion true
                       :effect (req (if (< 1 (count handlers))
                                      (continue-ability state side (choose-handler (next handlers)) nil event-targets)
                                      (effect-completed state side eid nil)))}))
                  {:prompt  "Select a trigger to resolve"
                   :choices titles
                   :delayed-completion true
                   :effect  (req (let [to-resolve (some #(when (= target (:title (:card %))) %) handlers)]
                                   (when-completed
                                     (resolve-ability state side (:ability to-resolve)
                                                      (:card to-resolve) event-targets)
                                     (if (< 1 (count handlers))
                                       (continue-ability state side
                                                         (choose-handler
                                                           (remove-once #(not= target (:title (:card %))) handlers))
                                                         nil event-targets)
                                       (effect-completed state side eid nil)))))})))]

      (continue-ability state side (choose-handler handlers) nil event-targets))
    (effect-completed state side eid nil)))

(defn ability-as-handler
  "Wraps a card ability as an event handler."
  [card ability]
  {:card card :ability ability})

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
  targets:       a varargs list of targets to the event, as usual"
  ([state side eid event {:keys [first-ability card-ability after-active-player] :as options} & targets]
   (let [get-side #(-> % :card :side game.utils/to-keyword)
         get-ability-side #(-> % :ability :side)
         active-player (:active-player @state)
         opponent (other-side active-player)
         is-player (fn [player ability] (or (= player (get-side ability)) (= player (get-ability-side ability))))

         ;; prepare the list of the given player's handlers for this event.
         ;; gather all registered handlers from the state, then append the card-ability if appropriate, then
         ;; filter to remove suppressed handlers and those whose req is false.
         ;; this is essentially "step 1" as described here:
         ;; http://ancur.wikia.com/wiki/User_blog:Jakodrako/Ability_Types_and_Resolution_Primer#Conditional_Abilities
         get-handlers (fn [player-side]
                        (let [abis (filter (partial is-player player-side) (get-in @state [:events event]))
                              abis (if (= player-side (get-side card-ability))
                                     (cons card-ability abis)
                                     abis)]
                          (filter #(and (not (apply trigger-suppress state side event (cons (:card %) targets)))
                                        (check-req state side (get-card state (:card %)) targets (:ability %)))
                                  abis)))
         active-player-events (get-handlers active-player)
         opponent-events (get-handlers opponent)]
     ; let active player activate their events first
     (when-completed
       (resolve-ability state side first-ability nil nil)
       (do (show-wait-prompt state opponent (str (side-str active-player) " to resolve " (event-title event) " triggers")
                             {:priority -1})
           (when-completed
             (trigger-event-simult-player state side event active-player-events targets)
             (do (when after-active-player
                   (resolve-ability state side after-active-player nil nil))
                 (clear-wait-prompt state opponent)
                 (show-wait-prompt state active-player
                                   (str (side-str opponent) " to resolve " (event-title event) " triggers")
                                   {:priority -1})
                 (when-completed (trigger-event-simult-player state opponent event opponent-events targets)
                                 (do (swap! state update-in [:turn-events] #(cons [event targets] %))
                                     (clear-wait-prompt state active-player)
                                     (effect-completed state side eid nil))))))))))


; Functions for registering trigger suppression events.
(defn register-suppress
  "Registers each suppression handler in teh given card definition. Suppression handlers
  can prevent the dispatching of a particular event. (Crisium Grid.)"
  [state side events card]
  (doseq [e events]
    (swap! state update-in [:suppress (first e)] #(conj % {:ability (last e) :card card}))))

(defn unregister-suppress [state side card]
  (doseq [e (:suppress (card-def card))]
    (swap! state update-in [:suppress (first e)]
           #(remove (fn [effect] (= (get-in effect [:card :cid]) (:cid card))) %))))

(defn trigger-suppress
  "Returns true if the given event on the given targets should be suppressed, by triggering
  each suppression handler and returning true if any suppression handler returns true."
  [state side event & targets]
  (reduce #(or %1 ((:req (:ability %2)) state side (make-eid state) (:card %2) targets))
          false (get-in @state [:suppress event])))


(defn turn-events
  "Returns the targets vectors of each event with the given key that was triggered this turn."
  [state side ev]
  (mapcat rest (filter #(= ev (first %)) (:turn-events @state))))

(defn first-event
  "Returns true if the given event has not occurred yet this turn."
  [state side ev]
  (empty? (turn-events state side ev)))

(defn second-event
  "Returns true if the given event has occurred exactly once this turn."
  [state side ev]
  (= (count (turn-events state side ev)) 1))

;;; Effect completion triggers
(defn register-effect-completed
  [state side eid card effect]
  (swap! state update-in [:effect-completed (:eid eid)] #(conj % {:card card :effect effect})))

(defn effect-completed
  ([state side eid] (effect-completed state side eid nil))
  ([state side eid card]
   (doseq [handler (get-in @state [:effect-completed (:eid eid)])]
     ((:effect handler) state side eid (:card card) nil))
   (swap! state update-in [:effect-completed] dissoc (:eid eid))))