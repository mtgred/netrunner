(in-ns 'game.cards.identities)

(def card-definition-mti-mwekundu-life-improved
  {"Mti Mwekundu: Life Improved"
   {:abilities [{:once :per-turn
                 :label "Install a piece of ice from HQ at the innermost position"
                 :req (req (and (:run @state)
                                (zero? (:position run))
                                (not (contains? run :corp-phase-43))
                                (not (contains? run :successful))))
                 :prompt "Choose ICE to install from HQ"
                 :msg "install ice at the innermost position of this server. Runner is now approaching that ice"
                 :choices {:req #(and (ice? %)
                                      (in-hand? %))}
                 :effect (req (corp-install state side target (:server run) {:no-install-cost true
                                                                             :front true})
                              (swap! state assoc-in [:run :position] 1))}]}})
