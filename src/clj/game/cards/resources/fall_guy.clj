(in-ns 'game.cards.resources)

(def card-definition-fall-guy
  {"Fall Guy"
   {:interactions {:prevent [{:type #{:trash-resource}
                              :req (req true)}]}
    :abilities [{:label "[Trash]: Prevent another installed resource from being trashed"
                 :effect (effect (trash card {:unpreventable true :cause :ability-cost})
                                 (trash-prevent :resource 1))}
                {:label "[Trash]: Gain 2 [Credits]"
                 :msg "gain 2 [Credits]"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain-credits 2))}]}})
