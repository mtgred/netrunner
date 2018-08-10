(in-ns 'game.cards.upgrades)

(def card-definition-henry-phillips
  {"Henry Phillips"
   {:implementation "Manually triggered by Corp"
    :abilities [{:req (req (and this-server tagged))
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain-credits 2))}]}})
