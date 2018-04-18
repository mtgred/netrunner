(in-ns 'game.core)

(def card-definitions-icebreakers-omega
  {"Omega"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :req (req (= 1 (:position run)))
                                  :msg "break 1 subroutine on the innermost ICE protecting this server"}
                                 (strength-pump 1 1)]})})
