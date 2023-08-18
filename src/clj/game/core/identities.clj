(ns game.core.identities
  (:require
    [game.core.card :refer [active?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [register-static-abilities unregister-static-abilities]]
    [game.core.eid :refer [make-eid]]
    [game.core.engine :refer [register-default-events resolve-ability unregister-events]]
    [game.core.initializing :refer [card-init deactivate]]
    [game.core.update :refer [update!]]))

(defn- actual-disable-identity
  "Actually disables the side's identity"
  [state side]
  (let [id (update! state side (assoc (get-in @state [side :identity]) :disabled true))]
    (unregister-events state side id)
    (unregister-static-abilities state side id)
    (when-let [leave-play (:leave-play (card-def id))]
      (leave-play state side (make-eid state) id nil))))

(defn disable-identity
  "Disables the side's identity"
  [state side]
  (let [disable-count (get-in @state [side :identity :num-disables])
        id (update! state side
                    (assoc (get-in @state [side :identity])
                           :num-disables ((fnil inc 0) disable-count)))]
    (when (= 1 (:num-disables id))
      (actual-disable-identity state side))))

(defn disable-card
  "Disables a card"
  [state side card]
  (deactivate state side card)
  (let [c (update! state side (assoc card :disabled true))]
    (when-let [disable-effect (:disable (card-def c))]
      (resolve-ability state side disable-effect c nil))))

(defn- actual-enable-identity
  "Actually enables the side's identity"
  [state side]
  (let [id (update! state side (assoc (get-in @state [side :identity]) :disabled false))
        {:keys [effect]} (card-def id)]
    (when effect
      (effect state side (make-eid state) id nil))
    (register-default-events state side id)
    (register-static-abilities state side id)))

(defn enable-identity
  "Enables the side's identity"
  [state side]
  (let [disable-count (get-in @state [side :identity :num-disables])
        id (update! state side
                    (assoc (get-in @state [side :identity])
                           :num-disables ((fnil dec 1) disable-count)))]
    (when (= 0 (:num-disables id))
      (actual-enable-identity state side))))

(defn enable-card
  "Enables a disabled card"
  [state side {:keys [disabled] :as card}]
  (when disabled
    (let [c (update! state side (dissoc card :disabled))]
      (when (active? card)
        (card-init state side c {:resolve-effect false})))))
