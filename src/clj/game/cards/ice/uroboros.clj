(in-ns 'game.core)

(def card-definitions-ice-uroboros
  {"Uroboros"
   {:subroutines [(trace-ability 4 {:label "Prevent the Runner from making another run"
                                    :msg "prevent the Runner from making another run"
                                    :effect (effect (register-turn-flag! card :can-run nil))})

                  (trace-ability 4 end-the-run)]}})
