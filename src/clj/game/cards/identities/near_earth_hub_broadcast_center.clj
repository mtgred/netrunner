(in-ns 'game.cards.identities)

(def card-definition-near-earth-hub-broadcast-center
  {"Near-Earth Hub: Broadcast Center"
   {:events {:server-created {:req (req (first-event? state :corp :server-created))
                              :msg "draw 1 card"
                              :async true
                              :effect (effect (draw :corp eid 1 nil))}}}})
