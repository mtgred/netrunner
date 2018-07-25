(in-ns 'game.cards.resources)

(def card-definition-xanadu
  {"Xanadu"
   {:events {:pre-rez-cost {:req (req (ice? target))
                            :effect (effect (rez-cost-bonus 1))}}}})
