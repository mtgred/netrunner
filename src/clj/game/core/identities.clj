(ns game.core.identities
  (:require
    [game.core.card :refer [active? get-card has-subtype?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [register-constant-effects unregister-constant-effects]]
    [game.core.eid :refer [make-eid]]
    [game.core.engine :refer [register-events resolve-ability unregister-events]]
    [game.core.gaining :refer [toast-check-mu use-mu]]
    [game.core.ice :refer [update-breaker-strength]]
    [game.core.initializing :refer [card-init deactivate]]
    [game.core.moving :refer [move]]
    [game.core.servers :refer [type->rig-zone]]
    [game.core.update :refer [update!]]))

(defn- actual-disable-identity
  "Actually disables the side's identity"
  [state side]
  (let [id (assoc (get-in @state [side :identity]) :disabled true)]
    (update! state side id)
    (unregister-events state side id)
    (unregister-constant-effects state side id)
    (when-let [leave-play (:leave-play (card-def id))]
      (leave-play state side (make-eid state) id nil))))

(defn disable-identity
  "Disables the side's identity"
  [state side]
  (let [disable-count (get-in @state [side :identity :num-disables])
        id (assoc (get-in @state [side :identity])
                  :num-disables ((fnil inc 0) disable-count))]
    (update! state side id)
    (when (= 1 (:num-disables id))
      (actual-disable-identity state side))))

(defn disable-card
  "Disables a card"
  [state side card]
  (deactivate state side card)
  (let [c (assoc card :disabled true)]
    (update! state side c))
  (when-let [disable-effect (:disable (card-def card))]
    (resolve-ability state side disable-effect (get-card state card) nil)))

(defn- actual-enable-identity
  "Actually enables the side's identity"
  [state side]
  (let [id (assoc (get-in @state [side :identity]) :disabled false)
        {:keys [effect]} (card-def id)]
    (update! state side id)
    (when effect
      (effect state side (make-eid state) id nil))
    (register-events state side id)
    (register-constant-effects state side id)))

(defn enable-identity
  "Enables the side's identity"
  [state side]
  (let [disable-count (get-in @state [side :identity :num-disables])
        id (assoc (get-in @state [side :identity])
                  :num-disables ((fnil dec 1) disable-count))]
    (update! state side id)
    (when (= 0 (:num-disables id))
      (actual-enable-identity state side))))

(defn enable-card
  "Enables a disabled card"
  [state side {:keys [disabled] :as card}]
  (when disabled
    (let [c (dissoc card :disabled)]
      (update! state side c)
      (when (active? card)
        (card-init state side c {:resolve-effect false})))))

(defn flip-facedown
  "Flips a runner card facedown, either manually (if it's hosted) or by calling move to facedown"
  [state side {:keys [host] :as card}]
  (if host
    (let [card (deactivate state side card true)
          card (assoc-in card [:facedown] true)]
      (update! state side card))
    (move state side card [:rig :facedown])))

(defn flip-faceup
  "Flips a runner card facedown, either manually (if it's hosted) or by calling move to correct area.
  Wires events without calling effect/init-data"
  [state side {:keys [host] :as card}]
  (let [card (if host
               (dissoc card :facedown)
               (move state side card (type->rig-zone (:type card))))]
   (card-init state side card {:resolve-effect false :init-data false})
   (when-let [mu (:memoryunits card)]
     (use-mu state mu)
     (toast-check-mu state))
   (when (has-subtype? card "Icebreaker")
     (update-breaker-strength state side card))))
