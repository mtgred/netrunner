(in-ns 'game.cards.ice)

(def card-definition-caduceus
  {"Caduceus"
   {:subroutines [(trace-ability 3 (gain-credits-sub 3))
                  (trace-ability 2 end-the-run)]}})
