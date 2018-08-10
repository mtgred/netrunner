(in-ns 'game.cards.assets)

(def card-definition-grndl-refinery
  {"GRNDL Refinery"
   {:advanceable :always
    :abilities [{:label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                 :cost [:click 1]
                 :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain-credits (* 4 (get-counters card :advancement))))}]}})
