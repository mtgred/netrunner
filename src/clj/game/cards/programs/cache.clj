(in-ns 'game.cards.programs)

(def card-definition-cache
  {"Cache"
   {:abilities [{:counter-cost [:virus 1]
                 :effect (effect (gain-credits 1))
                 :msg "gain 1 [Credits]"}]
    :data {:counter {:virus 3}}}})
