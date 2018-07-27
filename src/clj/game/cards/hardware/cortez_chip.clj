(in-ns 'game.cards.hardware)

(def card-definition-cortez-chip
  {"Cortez Chip"
   {:abilities [{:label "Increase rez cost of ice"
                 :prompt "Select a piece of ice"
                 :choices {:req ice?}
                 :msg (msg "increase the rez cost of " (card-str state target)
                           " by 2 [Credits] until the end of the turn")
                 :effect (effect (update! (assoc card :cortez-target target))
                                 (trash (get-card state card) {:cause :ability-cost}))}]
    :trash-effect {:effect (effect (register-events
                                     {:pre-rez {:req (req (= (:cid target)
                                                             (:cid (:cortez-target card))))
                                                :effect (effect (rez-cost-bonus 2))}
                                      :runner-turn-ends {:effect (effect (unregister-events card))}
                                      :corp-turn-ends {:effect (effect (unregister-events card))}}
                                     (get-card state card)))}
    :events {:pre-rez nil
             :runner-turn-ends nil
             :corp-turn-ends nil}}})
