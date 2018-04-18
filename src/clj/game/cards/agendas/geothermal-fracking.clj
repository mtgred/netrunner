(in-ns 'game.core)

(def card-definitions-agendas-geothermal-fracking
  {"Geothermal Fracking"
   {:effect (effect (add-counter card :agenda 2))
    :silent (req true)
    :abilities [{:cost [:click 1]
                 :counter-cost [:agenda 1]
                 :msg "gain 7 [Credits] and take 1 bad publicity"
                 :effect (effect (gain :credit 7)
                                 (gain-bad-publicity :corp 1))}]}})
