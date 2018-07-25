(in-ns 'game.cards.ice)

(def card-definition-hourglass
  {"Hourglass"
   {:subroutines [{:msg "force the Runner to lose 1 [Click] if able"
                   :effect runner-loses-click}]}})
