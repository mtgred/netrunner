(in-ns 'game.cards.icebreakers)

(def card-definition-deus-x
  {"Deus X"
   {:interactions {:prevent [{:type #{:net}
                              :req (req true)}]}
    :abilities [{:msg "break any number of AP subroutines"
                 :effect (effect (trash card {:cause :ability-cost}))}
                {:msg "prevent any amount of net damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :net Integer/MAX_VALUE))}]}})
