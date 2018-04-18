(in-ns 'game.core)

(def card-definitions-icebreakers-deus-x
  {"Deus X"
   {:prevent {:damage [:net]}
    :abilities [{:msg "break any number of AP subroutines"
                 :effect (effect (trash card {:cause :ability-cost}))}
                {:msg "prevent any amount of net damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :net Integer/MAX_VALUE))}]}})
