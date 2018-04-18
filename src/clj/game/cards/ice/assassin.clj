(in-ns 'game.core)

(def card-definitions-ice-assassin
  {"Assassin"
   {:subroutines [(trace-ability 5 (do-net-damage 3))
                  (trace-ability 4 trash-program)]}})
