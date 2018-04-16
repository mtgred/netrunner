(in-ns 'game.core)

(declare run-event)

(def card-events-running-interference
  {"Running Interference"
   (run-event
    {:events {:pre-rez nil
              :run-ends nil}}
    nil
    nil
    (effect (register-events {:pre-rez {:req (req (ice? target))
                                        :effect (effect (rez-cost-bonus (:cost target)))}
                              :run-ends {:effect (effect (unregister-events card))}}
                             (assoc card :zone '(:discard)))))})
