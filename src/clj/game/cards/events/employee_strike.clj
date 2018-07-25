(in-ns 'game.cards.events)

(def card-definition-employee-strike
  {"Employee Strike"
   {:msg "disable the Corp's identity"
    :disable-id true
    :effect (effect (disable-identity :corp))
    :leave-play (effect (enable-identity :corp))}})
