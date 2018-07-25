(in-ns 'game.cards.resources)

(def card-definition-compromised-employee
  {"Compromised Employee"
   {:recurring 1
    :events {:rez {:req (req (ice? target))
                   :msg "gain 1 [Credits]"
                   :effect (effect (gain-credits :runner 1))}}}})
