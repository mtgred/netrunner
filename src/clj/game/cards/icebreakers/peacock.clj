(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-peacock
  {"Peacock"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 2 3)]})})
