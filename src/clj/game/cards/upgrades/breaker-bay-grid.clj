(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-breaker-bay-grid
  {"Breaker Bay Grid"
   {:events {:pre-rez-cost {:req (req (in-same-server? card target))
                            :effect (effect (rez-cost-bonus -5))}}}})