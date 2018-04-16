(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-refractor
  {"Refractor"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 3)]})
   "Sadyojata"
   (deva "Sadyojata")})