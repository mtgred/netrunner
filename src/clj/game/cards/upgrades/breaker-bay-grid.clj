(in-ns 'game.core)

(def card-definitions-upgrades-breaker-bay-grid
  {"Breaker Bay Grid"
   {:events {:pre-rez-cost {:req (req (in-same-server? card target))
                            :effect (effect (rez-cost-bonus -5))}}}})
