(in-ns 'game.core)

(def card-definitions-assets-shattered-remains
  {"Shattered Remains"
   (advance-ambush 1 {:delayed-completion true
                      :effect (req (let [shat (get-card state card)]
                                     (when (< 0 (:advance-counter shat 0))
                                       (continue-ability
                                         state side
                                         (-> trash-hardware
                                             (assoc-in [:choices :max] (:advance-counter shat))
                                             (assoc :prompt (msg "Select " (:advance-counter shat) " pieces of hardware to trash")
                                                    :effect (effect (trash-cards targets))
                                                    :msg (msg "trash " (join ", " (map :title targets)))))
                                        shat nil))))})})
