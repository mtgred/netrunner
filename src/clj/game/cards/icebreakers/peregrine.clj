(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-peregrine
  {"Peregrine"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 3 3)
                                 {:label "Derez a Code Gate and return Peregrine to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Code Gate")))
                                  :msg (msg "derez " (:title current-ice) " and return Peregrine to their Grip")
                                  :effect (effect (derez current-ice)
                                                  (move card :hand))}]})})
