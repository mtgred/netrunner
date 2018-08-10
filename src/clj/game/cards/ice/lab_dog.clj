(in-ns 'game.cards.ice)

(def card-definition-lab-dog
  {"Lab Dog"
   {:subroutines [(assoc trash-hardware :label "Force the Runner to trash an installed piece of hardware"
                                        :player :runner
                                        :msg (msg "force the Runner to trash " (:title target))
                                        :effect (req (trash state side target)
                                                     (when current-ice
                                                       (no-action state side nil)
                                                       (continue state side nil))
                                                     (trash state side card)))]}})
