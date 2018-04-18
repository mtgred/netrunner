(in-ns 'game.core)

(def card-definitions-ice-caduceus
  {"Caduceus"
   {:subroutines [(trace-ability 3 (gain-credits 3))
                  (trace-ability 2 end-the-run)]}})
