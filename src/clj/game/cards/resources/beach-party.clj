(in-ns 'game.core)

(def card-definitions-resources-beach-party
  {"Beach Party"
   {:in-play [:hand-size-modification 5]
    :events {:runner-turn-begins {:msg "lose [Click]" :effect (effect (lose :click 1))}}}})
