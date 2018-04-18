(in-ns 'game.core)

(def card-definitions-agendas-braintrust
  {"Braintrust"
   {:effect (effect (add-counter card :agenda (quot (- (:advance-counter card) 3) 2)))
    :silent (req true)
    :events {:pre-rez-cost {:req (req (ice? target))
                            :effect (req (rez-cost-bonus state side (- (get-in card [:counter :agenda] 0))))}}}})
