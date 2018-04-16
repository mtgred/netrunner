(in-ns 'game.core)

(def card-operations-rolling-brownout
  {"Rolling Brownout"
   {:msg "increase the play cost of operations and events by 1 [Credits]"
    :events {:play-event {:once :per-turn
                          :msg "gain 1 [Credits]"
                          :effect (effect (gain :corp :credit 1))}
             :pre-play-instant {:effect (effect (play-cost-bonus [:credit 1]))}}}})