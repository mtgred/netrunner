(in-ns 'game.cards.ice)

(def card-definition-lockdown
  {"Lockdown"
   {:subroutines [{:label "The Runner cannot draw cards for the remainder of this turn"
                   :msg "prevent the Runner from drawing cards"
                   :effect (effect (prevent-draw))}]}})
