(in-ns 'game.cards.ice)

(def card-definition-it-s-a-trap
  {"It's a Trap!"
   {:expose {:msg "do 2 net damage"
             :async true
             :effect (effect (damage eid :net 2 {:card card}))}
    :subroutines [(assoc trash-installed :effect (req (trash state side target {:cause :subroutine})
                                                      (when current-ice
                                                        (no-action state side nil)
                                                        (continue state side nil))
                                                      (trash state side card)))]}})
