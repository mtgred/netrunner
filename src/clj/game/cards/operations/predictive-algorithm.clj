(in-ns 'game.core)

(def card-definitions-operations-predictive-algorithm
  {"Predictive Algorithm"
   {:events {:pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 2]))}}}})
