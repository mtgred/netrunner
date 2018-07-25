(in-ns 'game.cards.events)

(def card-definition-unscheduled-maintenance
  {"Unscheduled Maintenance"
   {:events {:corp-install {:req (req (ice? target))
                            :effect (effect (register-turn-flag!
                                              card :can-install-ice
                                              (fn [state side card]
                                                (if (ice? card)
                                                  ((constantly false)
                                                   (toast state :corp "Cannot install ICE the rest of this turn due to Unscheduled Maintenance"))
                                                  true))))}}
    :leave-play (effect (clear-turn-flag! card :can-install-ice))}})
