(in-ns 'game.core)

(def card-operations-predictive-algorithm
  {"Predictive Algorithm"
   {:events {:pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 2]))}}}})