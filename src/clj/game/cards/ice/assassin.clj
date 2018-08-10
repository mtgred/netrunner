(in-ns 'game.cards.ice)

(def card-definition-assassin
  {"Assassin"
   {:subroutines [(trace-ability 5 (do-net-damage 3))
                  (trace-ability 4 trash-program)]}})
