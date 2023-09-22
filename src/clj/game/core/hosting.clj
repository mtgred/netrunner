(ns game.core.hosting
  (:require
    [game.core.card :refer [assoc-host-zones corp? get-card program? rezzed? runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [register-static-abilities unregister-static-abilities]]
    [game.core.eid :refer [make-eid]]
    [game.core.engine :refer [register-default-events unregister-events]]
    [game.core.initializing :refer [card-init]]
    [game.core.memory :refer [init-mu-cost]]
    [game.core.update :refer [update! update-hosted!]]
    [game.utils :refer [remove-once]]))

(defn remove-from-host
  "Removes a card from its host."
  [state side {:keys [cid] :as card}]
  (when-let [host-card (get-card state (:host card))]
    (update-hosted! state side (update-in host-card [:hosted] (fn [coll] (remove-once #(= (:cid %) cid) coll))))
    (when-let [hosted-lost (:hosted-lost (card-def host-card))]
      (hosted-lost state side (make-eid state) (get-card state host-card) (dissoc card :host)))))

(defn host
  "Host the target onto the card."
  ([state side card target] (host state side card target nil))
  ([state side card {:keys [zone cid host installed] :as target} {:keys [facedown no-mu]}]
   (when (not= cid (:cid card))
     (unregister-events state side target)
     (unregister-static-abilities state side target)
     (doseq [s [:runner :corp]]
       (if host
         (when-let [host-card (get-card state host)]
           (update! state side (update host-card :hosted
                                       (fn [coll] (remove-once #(= (:cid %) cid) coll)))))
         (swap! state update-in (cons s (vec zone))
                (fn [coll] (remove-once #(= (:cid %) cid) coll)))))
     (swap! state update-in (cons side (vec zone)) (fn [coll] (remove-once #(= (:cid %) cid) coll)))
     (let [card (get-card state card)
           card (assoc-host-zones card)
           target (assoc target
                         :host (dissoc card :hosted)
                         :facedown facedown
                         :zone [:onhost] ;; hosted cards should not be in :discard or :hand etc
                         :previous-zone (:zone target))
           ;; Update any cards hosted by the target, so their :host has the updated zone.
           target (update target :hosted #(map (fn [h] (assoc h :host target)) %))
           cdef (card-def card)
           tdef (card-def target)]
       (update! state side (update card :hosted conj target))
       (when (and installed
                  (or (runner? target)
                      (and (corp? target)
                           (rezzed? target))))
         (if (or (:recurring tdef)
                 (:prevent tdef)
                 (:corp-abilities tdef)
                 (:runner-abilities tdef))
           ;; Initialize the whole card
           (card-init state side target {:resolve-effect false
                                         :init-data true
                                         :no-mu no-mu})
           ;; Otherwise just register events and static abilities
           (do (register-default-events state side target)
               (register-static-abilities state side target)
               (when (and (program? target)
                          (not no-mu))
                 (init-mu-cost state target)))))
       (when-let [hosted-gained (:hosted-gained cdef)]
         (hosted-gained state side (make-eid state) (get-card state card) [target]))
       ;; Update all static abilities and floating effects
       (let [new-effects (reduce
                           (fn [all-effects current-effect]
                             (if (= cid (:cid (:card current-effect)))
                               (conj all-effects (assoc current-effect :card target))
                               (conj all-effects current-effect)))
                           []
                           (:effects @state))]
         (swap! state assoc :effects (into [] new-effects)))
       (get-card state target)))))
