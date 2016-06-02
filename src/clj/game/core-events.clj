(in-ns 'game.core)

(declare effect-completed forfeit prompt! register-suppress trigger-suppress unregister-suppress)

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
  "Triggers the given event synchronously, requiring each handler to complete before alerting the next handler."
  [state side eid event & targets]
  (let [get-side #(-> % :card :side game.utils/to-keyword)
        is-active-player #(= (:active-player @state) (get-side %))]

    (let [handlers (sort-by (complement is-active-player) (get-in @state [:events event]))
          card nil]
      (when-completed (apply trigger-event-sync-next state side handlers event targets)
                      (effect-completed state side eid nil)))))

;; INCOMPLETE
(defn trigger-event-async
  "Triggers the given event asynchronously, triggering effect-completed once
  all registered event handlers have completed."
  [state side eid event & targets]
  (let [awaiting (atom #{})]
    (let [get-side #(-> % :card :side game.utils/to-keyword)
          is-active-player #(= (:active-player @state) (get-side %))
          handlers (map #([% (make-eid state)]) (sort-by (complement is-active-player) (get-in @state [:events event])))]
      (doseq [h handlers]
        (let [e (first h)
              eid (second h)
              ability (:ability e)]
          (when-let [card (get-card state (:card e))]
            (when (and (not (apply trigger-suppress state side event (cons card targets)))
                       (or (not (:req ability)) ((:req ability) state side (make-eid state) card targets)))
              (when-completed (resolve-ability state side ability card targets)
                              (do (swap! awaiting disj eid)
                                  (when (empty? awaiting)
                                    (effect-completed state side eid nil))
                                  (prn "awaiting still " @awaiting))))))))))

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
  (swap! state update-in [:effect-completed eid] #(conj % {:card card :effect effect})))

(defn effect-completed
  [state side eid card]
  ;(prn "EFFECT-COMPLETED" eid)
  (doseq [handler (get-in @state [:effect-completed eid])]
    ((:effect handler) state side eid (:card card) nil))
  (swap! state update-in [:effect-completed] dissoc eid))