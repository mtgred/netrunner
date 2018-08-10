(in-ns 'game.cards.resources)

(def card-definition-biometric-spoofing
  {"Biometric Spoofing"
   {:interactions {:prevent [{:type #{:net :brain :meat}
                              :req (req true)}]}
    :abilities [{:label "[Trash]: Prevent 2 damage"
                 :msg "prevent 2 damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :brain 2)
                                 (damage-prevent :net 2)
                                 (damage-prevent :meat 2))}]}})
