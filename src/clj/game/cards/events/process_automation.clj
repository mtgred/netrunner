(in-ns 'game.cards.events)

(def card-definition-process-automation
  {"Process Automation"
   {:msg "gain 2 [Credits] and draw 1 card"
    :effect (effect (gain-credits 2) (draw 1))}})
