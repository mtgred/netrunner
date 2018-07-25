(in-ns 'game.cards.resources)

(def card-definition-caldera
  {"Caldera"
   {:interactions {:prevent [{:type #{:net :brain}
                              :req (req true)}]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent 1 net damage"
                 :effect (effect (damage-prevent :net 1))}
                {:cost [:credit 3]
                 :msg "prevent 1 brain damage"
                 :effect (effect (damage-prevent :brain 1))}]}})
