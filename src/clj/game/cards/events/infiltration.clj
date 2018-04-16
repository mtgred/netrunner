(in-ns 'game.core)

(declare run-event)

(def card-events-infiltration
  {"Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (continue-ability (if (= target "Expose a card")
                                        {:choices {:req installed?}
                                         :delayed-completion true
                                         :effect (effect (expose eid target))}
                                         {:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))})
                                      card nil))}})