(in-ns 'game.cards.operations)

(def card-definition-successful-demonstration
  {"Successful Demonstration"
   {:req (req (last-turn? state :runner :unsuccessful-run))
    :msg "gain 7 [Credits]"
    :effect (effect (gain-credits 7))}})
