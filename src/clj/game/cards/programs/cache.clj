(in-ns 'game.core)

(def card-definitions-programs-cache
  {"Cache"
   {:abilities [{:counter-cost [:virus 1]
                 :effect (effect (gain :credit 1))
                 :msg "gain 1 [Credits]"}]
    :data {:counter {:virus 3}}}})
