(in-ns 'game.cards.ice)

(def card-definition-formicary
  {"Formicary"
   {:optional {:prompt "Move Formicary?"
               :req (req (and (:run @state)
                   (zero? (:position run))
                   (not (contains? run :corp-phase-43))
                   (not (contains? run :successful))))
               :yes-ability {:msg "rez and move Formicary. The Runner is now approaching Formicary."
                             :effect (req (move state side card
                                                [:servers (first (:server run)) :ices]
                                                {:front true})
                                          (swap! state assoc-in [:run :position] 1))}
               :no-ability {:msg "rez Formicary without moving it"}}
    :subroutines [{:label "End the run unless the Runner suffers 2 net damage"
                   :async true
                   :effect (req (wait-for (resolve-ability
                                           state :runner
                                           {:optional
                                            {:prompt "Suffer 2 net damage? (If not, end the run)"
                                             :yes-ability {:async true
                                                           :msg "let the Runner suffer 2 net damage"
                                                           :effect (effect (damage eid :net 2 {:card card :unpreventable true}))}
                                             :no-ability end-the-run}}
                                           card nil)))}]}})
