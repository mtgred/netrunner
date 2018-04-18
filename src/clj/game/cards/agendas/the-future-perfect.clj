(in-ns 'game.core)

(def card-definitions-agendas-the-future-perfect
  {"The Future Perfect"
   {:flags {:rd-reveal (req true)}
    :access
    {:psi {:req (req (not installed))
           :not-equal {:msg (msg "prevent it from being stolen")
                       :effect (final-effect (register-run-flag! card :can-steal
                                                                 (fn [_ _ c] (not= (:cid c) (:cid card)))))}}}}})
