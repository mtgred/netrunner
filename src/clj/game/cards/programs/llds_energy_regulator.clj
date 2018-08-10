(in-ns 'game.cards.programs)

(def card-definition-llds-energy-regulator
  {"LLDS Energy Regulator"
   {:interactions {:prevent [{:type #{:trash-hardware}
                              :req (req true)}]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent a hardware from being trashed"
                 :effect (effect (trash-prevent :hardware 1))}
                {:label "[Trash]: Prevent a hardware from being trashed"
                 :msg "prevent a hardware from being trashed"
                 :effect (effect (trash-prevent :hardware 1)
                                 (trash card {:cause :ability-cost}))}]}})
