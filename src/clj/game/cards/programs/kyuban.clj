(in-ns 'game.cards.programs)

(def card-definition-kyuban
  {"Kyuban"
   {:hosting {:req #(and (ice? %) (can-host? %))}
    :events {:pass-ice {:req (req (same-card? target (:host card)))
                        :msg "gain 2 credits"
                        :effect (effect (gain-credits :runner 2))}}}})
