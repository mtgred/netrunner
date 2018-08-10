(in-ns 'game.cards.assets)

(def card-definition-alix-t4lb07
  {"Alix T4LB07"
   {:events {:corp-install {:effect (effect (add-counter card :power 1))}}
    :abilities [{:label "Gain 2 [Credits] for each counter on Alix T4LB07"
                 :cost [:click 1]
                 :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain-credits (* 2 (get-counters card :power))))}]}})
