(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-henry-phillips
  {"Henry Phillips"
   {:implementation "Manually triggered by Corp"
    :abilities [{:req (req (and this-server tagged))
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain :credit 2))}]}})
