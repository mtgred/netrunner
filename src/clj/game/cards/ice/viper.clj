(in-ns 'game.cards.ice)

(def card-definition-viper
  {"Viper"
   {:subroutines [(trace-ability 3 {:label "The Runner loses 1 [Click] if able"
                                    :msg "force the Runner to lose 1 [Click] if able"
                                    :effect runner-loses-click})
                  (trace-ability 3 end-the-run)]}})
