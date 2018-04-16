(in-ns 'game.core)

(def card-hardware-emp-device
  {"EMP Device"
   {:abilities [{:req (req (:run @state))
                 :msg "prevent the Corp from rezzing more than 1 piece of ICE for the remainder of the run"
                 :effect (effect (register-events
                                   {:rez {:req (req (ice? target))
                                          :effect (effect (register-run-flag!
                                                            card :can-rez
                                                            (fn [state side card]
                                                              (if (ice? card)
                                                                ((constantly false)
                                                                 (toast state :corp "Cannot rez ICE the rest of this run due to EMP Device"))
                                                                true))))}
                                    :run-ends {:effect (effect (unregister-events card))}} (assoc card :zone '(:discard)))
                                 (trash card {:cause :ability-cost}))}]
    :events {:rez nil
             :run-ends nil}}})