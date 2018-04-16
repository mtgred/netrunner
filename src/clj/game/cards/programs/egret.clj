(in-ns 'game.core)

(declare can-host?)

(def card-programs-egret
  {"Egret"
   {:implementation "Added subtypes don't get removed when Egret is moved/trashed"
    :hosting {:req #(and (ice? %) (can-host? %) (rezzed? %))}
    :msg (msg "make " (card-str state (:host card)) " gain Barrier, Code Gate and Sentry subtypes")
    :effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (update-ice-strength state side h)
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))
    :events {:ice-strength-changed
             {:effect (req (unregister-events state side card)
                           (when (get-in card [:special :installing])
                             (update! state side (assoc (:host (get-card state card)) :subtype (combine-subtypes false(-> card :host :subtype) "Barrier" "Code Gate" "Sentry")))
                             (update! state side (update-in card [:special] dissoc :installing))
                             (trigger-event state side :runner-install card))
                           (continue state side nil))}}}})
