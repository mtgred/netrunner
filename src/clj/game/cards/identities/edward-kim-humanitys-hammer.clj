(in-ns 'game.core)

(def card-definitions-identities-edward-kim-humanitys-hammer
  {"Edward Kim: Humanitys Hammer"
   {:events {:access {:once :per-turn
                      :req (req (and (is-type? target "Operation")
                                     (turn-flag? state side card :can-trash-operation)))
                      :effect (req (trash state side target)
                                   (swap! state assoc-in [:run :did-trash] true)
                                   (swap! state assoc-in [:runner :register :trashed-card] true)
                                   (register-turn-flag! state side card :can-trash-operation (constantly false)))
                      :msg (msg "trash " (:title target))}
             :successful-run-ends {:req (req (and (= (:server target) [:archives])
                                                  (nil? (:replace-access (:run-effect target)))
                                                  (not= (:max-access target) 0)
                                                  (seq (filter #(is-type? % "Operation") (:discard corp)))))
                                   :effect (effect (register-turn-flag! card :can-trash-operation (constantly false)))}}}})
