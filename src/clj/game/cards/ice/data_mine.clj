(in-ns 'game.cards.ice)

(def card-definition-data-mine
  {"Data Mine"
   {:subroutines [{:msg "do 1 net damage"
                   :effect (req (damage state :runner eid :net 1 {:card card})
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card))}]}})
