(in-ns 'game.core)

(def card-definitions-ice-hourglass
  {"Hourglass"
   {:subroutines [{:msg "force the Runner to lose 1 [Click] if able"
                   :effect runner-loses-click}]}})
