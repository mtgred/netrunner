(in-ns 'game.core)

(def card-definitions-resources-xanadu
  {"Xanadu"
   {:events {:pre-rez-cost {:req (req (ice? target))
                            :effect (effect (rez-cost-bonus 1))}}}})
