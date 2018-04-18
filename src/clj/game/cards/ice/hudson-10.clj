(in-ns 'game.core)

(def card-definitions-ice-hudson-10
  {"Hudson 1.0"
   {:subroutines [{:msg "prevent the Runner from accessing more than 1 card during this run"
                   :effect (effect (max-access 1))}]
    :runner-abilities [(runner-break [:click 1] 1)]}})
