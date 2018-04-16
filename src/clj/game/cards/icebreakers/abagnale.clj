(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-abagnale
  {"Abagnale"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 2 2)
                                 {:label "Bypass Code Gate being encountered"
                                  :req (req (has-subtype? current-ice "Code Gate"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :effect (effect (trash card {:cause :ability-cost}))}]})})