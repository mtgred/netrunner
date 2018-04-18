(in-ns 'game.core)

(def card-definitions-ice-lockdown
  {"Lockdown"
   {:subroutines [{:label "The Runner cannot draw cards for the remainder of this turn"
                   :msg "prevent the Runner from drawing cards" :effect (effect (prevent-draw))}]}})
