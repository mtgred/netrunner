(in-ns 'game.cards.events)

(def card-definition-black-hat
  {"Black Hat"
   {:trace {:base 4
            :unsuccessful {:effect (effect (register-events (:events (card-def card))
                                                            (assoc card :zone '(:discard))))}}
    :events {:pre-access {:req (req (#{:hq :rd} target))
                          :effect (effect (access-bonus 2))}
             :runner-turn-ends {:effect (effect (unregister-events card))}}}})
