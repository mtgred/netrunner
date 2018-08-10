(in-ns 'game.cards.operations)

(def card-definition-rolling-brownout
  {"Rolling Brownout"
   {:msg "increase the play cost of operations and events by 1 [Credits]"
    :events {:play-event {:once :per-turn
                          :msg "gain 1 [Credits]"
                          :effect (effect (gain-credits :corp 1))}
             :pre-play-instant {:effect (effect (play-cost-bonus [:credit 1]))}}}})
