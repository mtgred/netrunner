(in-ns 'game.core)

(def card-definitions-identities-near-earth-hub-broadcast-center
  {"Near-Earth Hub: Broadcast Center"
   {:events {:server-created {:req (req (first-event? state :corp :server-created))
                              :msg "draw 1 card"
                              :effect (effect (draw 1))}}}})
