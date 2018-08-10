(in-ns 'game.cards.agendas)

(def card-definition-the-future-perfect
  {"The Future Perfect"
   {:flags {:rd-reveal (req true)}
    :access
    {:psi {:req (req (not installed))
           :not-equal {:msg (msg "prevent it from being stolen")
                       :effect (effect (register-run-flag! card :can-steal
                                                           (fn [_ _ c] (not= (:cid c) (:cid card))))
                                       ;; TODO: investigate why this is needed??
                                       (effect-completed eid))}}}}})
