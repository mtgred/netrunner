(in-ns 'game.cards.agendas)

(def card-definition-quantum-predictive-model
  {"Quantum Predictive Model"
   {:flags {:rd-reveal (req true)}
    :access {:req (req tagged)
             :async true
             :interactive (req true)
             :effect (req (wait-for (as-agenda state side card 1)
                                    (continue-ability
                                      state :runner
                                      {:prompt "Quantum Predictive Model was added to the corp's score area"
                                       :choices ["OK"]}
                                      card nil)))
             :msg "add it to their score area and gain 1 agenda point"}}})
