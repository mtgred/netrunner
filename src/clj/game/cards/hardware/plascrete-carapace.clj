(in-ns 'game.core)

(def card-hardware-plascrete-carapace
  {"Plascrete Carapace"
   {:data [:counter {:power 4}]
    :prevent {:damage [:meat]}
    :abilities [{:counter-cost [:power 1]
                 :msg "prevent 1 meat damage"
                 :effect (req (damage-prevent state side :meat 1)
                              (when (= (get-in card [:counter :power]) 0)
                                (trash state side card {:unpreventable true})))}]}})
