(in-ns 'game.cards.operations)

(def card-definition-big-brother
  {"Big Brother"
   {:req (req tagged)
    :msg "give the Runner 2 tags"
    :async true
    :effect (effect (gain-tags :corp eid 2))}})
