(in-ns 'game.core)

(def card-definitions-operations-successful-demonstration
  {"Successful Demonstration"
   {:req (req (last-turn? state :runner :unsuccessful-run))
    :msg "gain 7 [Credits]"
    :effect (effect (gain :credit 7))}})
