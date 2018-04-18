(in-ns 'game.core)

(def card-definitions-resources-biometric-spoofing
  {"Biometric Spoofing"
   {:prevent {:damage [:net :meat :brain]}
    :abilities [{:label "[Trash]: Prevent 2 damage"
                 :msg "prevent 2 damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :brain 2)
                                 (damage-prevent :net 2)
                                 (damage-prevent :meat 2))}]}})
