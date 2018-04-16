(in-ns 'game.core)

(declare can-host?)

(def card-programs-cache
  {"Cache"
   {:abilities [{:counter-cost [:virus 1]
                 :effect (effect (gain :credit 1))
                 :msg "gain 1 [Credits]"}]
    :data {:counter {:virus 3}}}})
