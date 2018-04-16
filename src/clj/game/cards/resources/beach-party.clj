(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-beach-party
  {"Beach Party"
   {:in-play [:hand-size-modification 5]
    :events {:runner-turn-begins {:msg "lose [Click]" :effect (effect (lose :click 1))}}}})