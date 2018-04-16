(in-ns 'game.core)

(def card-hardware-cortez-chip
  {"Cortez Chip"
   {:abilities [{:prompt "Select a piece of ICE"
                 :choices {:req ice?}
                 :effect (req (let [ice target]
                                (update! state side (assoc card :cortez-target ice))
                                (trash state side (get-card state card) {:cause :ability-cost})
                                (system-msg state side
                                  (str "trashes Cortez Chip to increase the rez cost of " (card-str state ice)
                                       " by 2 [Credits] until the end of the turn"))))}]
    :trash-effect {:effect (effect (register-events {:pre-rez {:req (req (= (:cid target) (:cid (:cortez-target card))))
                                                               :effect (effect (rez-cost-bonus 2))}
                                                     :runner-turn-ends {:effect (effect (unregister-events card))}
                                                     :corp-turn-ends {:effect (effect (unregister-events card))}}
                                                    (get-card state card)))}
    :events {:pre-rez nil :runner-turn-ends nil :corp-turn-ends nil}}})
