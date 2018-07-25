(in-ns 'game.cards.resources)

(def card-definition-donut-taganes
  {"Donut Taganes"
   {:msg "increase the play cost of operations and events by 1 [Credits]"
    :events {:pre-play-instant
             {:effect (effect (play-cost-bonus [:credit 1]))}}}})
