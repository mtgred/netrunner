(in-ns 'game.cards.agendas)

(def card-definition-braintrust
  {"Braintrust"
   {:effect (effect (add-counter card :agenda (quot (- (get-counters card :advancement) 3) 2)))
    :silent (req true)
    :events {:pre-rez-cost {:req (req (ice? target))
                            :effect (req (rez-cost-bonus state side (- (get-counters card :agenda))))}}}})
