(in-ns 'game.core)

(declare forfeit prompt! register-suppress trigger-suppress unregister-suppress)

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
  (doseq [{:keys [ability] :as e} (get-in @state [:events event])]
    (when-let [card (get-card state (:card e))]
      (when (and (not (apply trigger-suppress state side event (cons card targets)))
                 (or (not (:req ability)) ((:req ability) state side card targets)))
        (resolve-ability state side ability card targets))))
  (swap! state update-in [:turn-events] #(cons [event targets] %)))

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
  (reduce #(or %1 ((:req (:ability %2)) state side (:card %2) targets))
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
