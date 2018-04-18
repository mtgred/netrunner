(in-ns 'game.core)

(def card-definitions-resources-network-exchange
  {"Network Exchange"
   {:msg "increase the install cost of non-innermost ICE by 1"
    :events {:pre-corp-install {:req (req (is-type? target "ICE"))
                                :effect (req (when (pos? (count (:dest-zone (second targets))))
                                               (install-cost-bonus state :corp [:credit 1])))}}}})
