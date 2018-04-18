(in-ns 'game.core)

(def card-definitions-resources-compromised-employee
  {"Compromised Employee"
   {:recurring 1
    :events {:rez {:req (req (ice? target))
                   :msg "gain 1 [Credits]"
                   :effect (effect (gain :runner :credit 1))}}}})
