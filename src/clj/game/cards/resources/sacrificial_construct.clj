(in-ns 'game.cards.resources)

(def card-definition-sacrificial-construct
  {"Sacrificial Construct"
   {:interactions {:prevent [{:type #{:trash-program :trash-hardware}
                              :req (req true)}]}
    :abilities [{:effect (effect (trash-prevent :program 1) (trash-prevent :hardware 1)
                                 (trash card {:cause :ability-cost}))}]}})
