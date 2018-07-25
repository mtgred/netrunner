(in-ns 'game.cards.operations)

(def card-definition-commercialization
  {"Commercialization"
   {:msg (msg "gain " (get-counters target :advancement) " [Credits]")
    :choices {:req ice?}
    :effect (effect (gain-credits (get-counters target :advancement)))}})
