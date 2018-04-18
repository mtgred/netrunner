(in-ns 'game.core)

(def card-definitions-ice-chetana
  {"Chetana"
   {:subroutines [{:msg "make each player gain 2 [Credits]" :effect (effect (gain :runner :credit 2)
                                                                            (gain :corp :credit 2))}
                  (do-psi {:label "Do 1 net damage for each card in the Runner's grip"
                           :effect (effect (damage eid :net (count (get-in @state [:runner :hand])) {:card card}))
                           :msg (msg (str "do " (count (get-in @state [:runner :hand])) " net damage"))})]}})
