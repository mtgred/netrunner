(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-saker
  {"Saker"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 2)
                                 {:label "Derez a Barrier and return Saker to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Barrier")))
                                  :msg (msg "derez " (:title current-ice) " and return Saker to their Grip")
                                  :effect (effect (derez current-ice)
                                                  (move card :hand))}]})})