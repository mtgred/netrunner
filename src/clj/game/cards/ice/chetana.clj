(in-ns 'game.cards.ice)

(def card-definition-chetana
  {"Chetana"
   {:subroutines [{:msg "make each player gain 2 [Credits]"
                   :effect (effect (gain-credits :runner 2)
                                   (gain-credits :corp 2))}
                  (do-psi {:label "Do 1 net damage for each card in the Runner's grip"
                           :effect (effect (damage eid :net (count (get-in @state [:runner :hand])) {:card card}))
                           :msg (msg (str "do " (count (get-in @state [:runner :hand])) " net damage"))})]}})
