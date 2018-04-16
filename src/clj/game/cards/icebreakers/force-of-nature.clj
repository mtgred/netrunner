(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-force-of-nature
  {"Force of Nature"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 2 "Code Gate")
                                 (strength-pump 1 1)]})})
