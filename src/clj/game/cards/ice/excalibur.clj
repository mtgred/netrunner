(in-ns 'game.core)

(def card-definitions-ice-excalibur
  {"Excalibur"
   {:subroutines [{:label "The Runner cannot make another run this turn"
                   :msg "prevent the Runner from making another run"
                   :effect (effect (register-turn-flag! card :can-run nil))}]}})
