(in-ns 'game.cards.resources)

(def card-definition-beach-party
  {"Beach Party"
   {:in-play [:hand-size 5]
    :events {:runner-turn-begins {:msg "lose [Click]"
                                  :effect (effect (lose :click 1))}}}})
