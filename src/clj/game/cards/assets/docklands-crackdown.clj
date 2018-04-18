(in-ns 'game.core)

(def card-definitions-assets-docklands-crackdown
  {"Docklands Crackdown"
   {:abilities [{:cost [:click 2]
                 :msg "add 1 power counter"
                 :effect (effect (add-counter card :power 1))}]
    :events {:pre-install {:req (req (and (pos? (get-in card [:counter :power] 0))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (install-cost-bonus [:credit (get-in card [:counter :power])]))}
             :runner-install {:silent (req true)
                              :req (req (and (pos? (get-in card [:counter :power] 0))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :msg (msg "increase the install cost of " (:title target) " by " (get-in card [:counter :power]) " [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}})
