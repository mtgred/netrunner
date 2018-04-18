(in-ns 'game.core)

(def card-definitions-ice-red-tape
  {"Red Tape"
   {:subroutines [{:label "Give +3 strength to all ICE for the remainder of the run"
                   :msg "give +3 strength to all ICE for the remainder of the run"
                   :effect (effect (register-events
                                     {:pre-ice-strength {:effect (effect (ice-strength-bonus 3 target))}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card)
                                   (update-all-ice))}]
    :events {:pre-ice-strength nil :run-ends nil}}})
