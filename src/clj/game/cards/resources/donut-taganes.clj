(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-donut-taganes
  {"Donut Taganes"
   {:msg "increase the play cost of operations and events by 1 [Credits]"
    :events {:pre-play-instant
             {:effect (effect (play-cost-bonus [:credit 1]))}}}})