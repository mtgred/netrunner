(in-ns 'game.core)

(def card-definitions-ice-cortex-lock
  {"Cortex Lock"
   {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                   :msg (msg "do " (:memory runner) " net damage")
                   :effect (effect (damage eid :net (:memory runner) {:card card}))}]}})
