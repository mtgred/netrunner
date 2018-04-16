(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-demara
  {"Demara"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 2 3)
                                 {:label "Bypass Barrier being encountered"
                                  :req (req (has-subtype? current-ice "Barrier"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :effect (effect (trash card {:cause :ability-cost}))}]})})