(in-ns 'game.cards.ice)

(def card-definition-cortex-lock
  {"Cortex Lock"
   {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                   :msg (msg "do " (available-mu state) " net damage")
                   :effect (effect (damage eid :net (available-mu state) {:card card}))}]}})
