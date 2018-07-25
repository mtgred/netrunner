(in-ns 'game.cards.events)

(def card-definition-infiltration
  {"Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (continue-ability (if (= target "Expose a card")
                                        {:choices {:req installed?}
                                         :async true
                                         :effect (effect (expose eid target))}
                                         {:msg "gain 2 [Credits]" :effect (effect (gain-credits 2))})
                                      card nil))}})
