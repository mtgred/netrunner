(ns game.core.props
  (:require
    [game.core.card :refer [get-card ice? rezzed?]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [checkpoint queue-event trigger-event-sync]]
    [game.core.finding :refer [find-latest]]
    [game.core.gaining :refer [gain-credits]]
    [game.core.ice :refer [update-ice-strength]]
    [game.core.update :refer [update!]]))

(defn add-prop
  "Adds the given value n to the existing value associated with the key in the card.
  Example: (add-prop ... card :counter 1) adds one power/virus counter. Triggers events."
  ([state side eid card prop-type n] (add-prop state side eid card prop-type n nil))
  ([state side eid card prop-type n {:keys [placed suppress-checkpoint]}]
   (if-let [card (get-card state card)]
     (let [updated-card (update! state side (update card prop-type #(+ (or % 0) n)))
           args {:counter-type prop-type :amount n :placed placed}]
       (if (= prop-type :advance-counter)
         (do (when (and (ice? updated-card)
                        (rezzed? updated-card))
               (update-ice-strength state side updated-card))
             (queue-event state (if placed :advancement-placed :advance) (assoc args :card (get-card state updated-card))))
         (queue-event state :counter-added (assoc args :card (get-card state updated-card))))
       (if-not suppress-checkpoint
         (checkpoint state side eid)
         (effect-completed state side eid)))
     (effect-completed state side eid))))

(defn add-counter
  "Adds n counters of the specified type to a card"
  ([state side eid card prop-type n] (add-counter state side eid card prop-type n nil))
  ([state side eid card prop-type n {:keys [placed suppress-checkpoint] :as args}]
   (if-let [card (get-card state card)]
     (if (= prop-type :advancement)
       ;; if advancement counter use existing system
       (add-prop state side eid card :advance-counter n args)
       (let [updated-card (update! state side (update-in card [:counter prop-type] #(+ (or % 0) n)))]
         (queue-event state :counter-added {:card updated-card :counter-type prop-type :amount n :placed placed})
         (if-not suppress-checkpoint
           (checkpoint state side eid)
           (effect-completed state side eid))))
     (effect-completed state side eid))))

(defn set-prop
  "Like add-prop, but sets multiple keys to corresponding values without triggering events.
  Example: (set-prop ... card :counter 4 :current-strength 0)"
  [state side card & args]
  (update! state side (apply assoc (cons card args))))
