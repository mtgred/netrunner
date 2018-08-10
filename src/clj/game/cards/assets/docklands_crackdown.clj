(in-ns 'game.cards.assets)

(def card-definition-docklands-crackdown
  {"Docklands Crackdown"
   {:abilities [{:cost [:click 2]
                 :msg "add 1 power counter"
                 :effect (effect (add-counter card :power 1))}]
    :events {:pre-install {:req (req (and (pos? (get-counters card :power))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (install-cost-bonus [:credit (get-counters card :power)]))}
             :runner-install {:silent (req true)
                              :req (req (and (pos? (get-counters card :power))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :msg (msg "increase the install cost of " (:title target) " by " (get-counters card :power) " [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}})
