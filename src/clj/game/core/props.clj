(ns game.core.props
  (:require
    [game.core.card :refer [get-card ice? rezzed?]]
    [game.core.eid :refer [make-eid]]
    [game.core.engine :refer [trigger-event-sync]]
    [game.core.finding :refer [find-latest]]
    [game.core.ice :refer [update-ice-strength]]
    [game.core.update :refer [update!]]))

(defn add-prop
  "Adds the given value n to the existing value associated with the key in the card.
  Example: (add-prop ... card :counter 1) adds one power/virus counter. Triggers events."
  ([state side card prop-type n] (add-prop state side (make-eid state) card prop-type n nil))
  ([state side card prop-type n args] (add-prop state side (make-eid state) card prop-type n args))
  ([state side eid card prop-type n {:keys [placed]}]
   (let [updated-card (update! state side (update (get-card state card) prop-type #(+ (or % 0) n)))
         args {:counter-type prop-type :amount n :placed placed}]
     (if (= prop-type :advance-counter)
       (do (when (and (ice? updated-card)
                      (rezzed? updated-card))
             (update-ice-strength state side updated-card))
           (if-not placed
             (trigger-event-sync state side eid :advance (assoc args :card (get-card state updated-card)))
             (trigger-event-sync state side eid :advancement-placed (assoc args :card (get-card state updated-card)))))
       (trigger-event-sync state side eid :counter-added (get-card state updated-card) args)))))

(defn add-counter
  "Adds n counters of the specified type to a card"
  ([state side card prop-type n] (add-counter state side (make-eid state) card prop-type n nil))
  ([state side card prop-type n args] (add-counter state side (make-eid state) card prop-type n args))
  ([state side eid card prop-type n {:keys [placed] :as args}]
   (if (= prop-type :advancement)
     ;; if advancement counter use existing system
     (add-prop state side eid card :advance-counter n args)
     (let [updated-card (update! state side (update-in (get-card state card) [:counter prop-type] #(+ (or % 0) n)))]
       (trigger-event-sync state side eid :counter-added updated-card {:counter-type prop-type :amount n :placed placed})))))

(defn set-prop
  "Like add-prop, but sets multiple keys to corresponding values without triggering events.
  Example: (set-prop ... card :counter 4 :current-strength 0)"
  [state side card & args]
  (update! state side (apply assoc (cons card args))))

(defn add-icon
  "Adds an icon to a card. E.g. a Femme Fatale token.
  Card is the card adding the icon, target is card receiving the icon."
  [state side card target char color]
  ;; add icon
  (set-prop state side target :icon {:char char :color color :card card})
  ;; specify icon target on card
  (set-prop state side card :icon-target target))

(defn remove-icon
  "Remove the icon associated with the card and target."
  ([state side card] (remove-icon state side card (:icon-target card)))
  ([state side card target]
   (when target (set-prop state side (find-latest state target) :icon nil))
   (set-prop state side (find-latest state card) :icon-target nil)))
