(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-the-future-perfect
  {"The Future Perfect"
   {:flags {:rd-reveal (req true)}
    :access
    {:psi {:req (req (not installed))
           :not-equal {:msg (msg "prevent it from being stolen")
                       :effect (final-effect (register-run-flag! card :can-steal
                                                                 (fn [_ _ c] (not= (:cid c) (:cid card)))))}}}}})