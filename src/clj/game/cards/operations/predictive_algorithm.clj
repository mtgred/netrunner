(in-ns 'game.cards.operations)

(def card-definition-predictive-algorithm
  {"Predictive Algorithm"
   {:events {:pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 2]))}}}})
