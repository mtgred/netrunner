(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-leviathan
  {"Leviathan"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 3 3 "Code Gate")
                                 (strength-pump 3 5)]})})
