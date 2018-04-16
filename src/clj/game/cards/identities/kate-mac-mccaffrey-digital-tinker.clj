(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-kate-mac-mccaffrey-digital-tinker
  {"Kate \"Mac\" McCaffrey: Digital Tinker"
   {:events {:pre-install {:req (req (and (#{"Hardware" "Program"} (:type target))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (install-cost-bonus [:credit -1]))}
             :runner-install {:req (req (and (#{"Hardware" "Program"} (:type target))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :silent (req true)
                              :msg (msg "reduce the install cost of " (:title target) " by 1 [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}})
