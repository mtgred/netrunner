(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-alpha
  {"Alpha"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (= (:position run) (count run-ices)))
                                  :msg "break 1 subroutine on the outermost ICE protecting this server"}
                                 (strength-pump 1 1)]})})
