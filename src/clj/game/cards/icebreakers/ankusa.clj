(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-ankusa
  {"Ankusa"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 1 1)
                                 {:label "Add Barrier to HQ"
                                  :req (req (and (has-subtype? current-ice "Barrier")
                                                 (rezzed? current-ice)))
                                  :msg (msg "add " (:title current-ice) " to HQ after breaking all its subroutines")
                                  :effect (req (let [c current-ice]
                                                 (move state :corp c :hand nil)
                                                 (continue state side nil)))}]})})