(in-ns 'game.cards.assets)

(def card-definition-thomas-haas
  {"Thomas Haas"
   {:advanceable :always
    :abilities [{:label "Gain credits"
                 :msg (msg "gain " (* 2 (get-counters card :advancement)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain-credits (* 2 (get-counters card :advancement))))}]}})
