(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-sharpshooter
  {"Sharpshooter"
   (auto-icebreaker ["Destroyer"]
                    {:abilities [{:label "[Trash]: Break any number of Destroyer subroutines"
                                  :msg "break any number of Destroyer subroutines"
                                  :effect (effect (trash card {:cause :ability-cost}))}
                                 (strength-pump 1 2)]})})
