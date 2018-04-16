(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-zu13-key-master
  {"ZU.13 Key Master"
   (cloud-icebreaker
     (auto-icebreaker ["Code Gate"]
                      {:abilities [(break-sub 1 1 "Code Gate")
                                   (strength-pump 1 1)]}))})
