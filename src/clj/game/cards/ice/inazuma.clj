(in-ns 'game.cards.ice)

(def card-definition-inazuma
  {"Inazuma"
   {:abilities [{:msg "prevent the Runner from breaking subroutines on the next piece of ICE they encounter this run"}
                {:msg "prevent the Runner from jacking out until after the next piece of ICE"
                 :effect (effect (register-events
                                   {:pass-ice {:effect (req (swap! state update-in [:run] dissoc :prevent-jack-out)
                                                            (unregister-events state side card))}} card)
                                 (prevent-jack-out))}]}})
