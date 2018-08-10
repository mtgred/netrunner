(in-ns 'game.cards.resources)

(def card-definition-crash-space
  {"Crash Space"
   {:interactions {:prevent [{:type #{:meat}
                              :req (req true)}]}
    :recurring 2
    :abilities [{:label "Trash to prevent up to 3 meat damage"
                 :msg "prevent up to 3 meat damage"
                 :effect (effect (trash card {:cause :ability-cost}) (damage-prevent :meat 3))}]}})
