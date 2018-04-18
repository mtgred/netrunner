(in-ns 'game.core)

(def card-definitions-events-employee-strike
  {"Employee Strike"
   {:msg "disable the Corp's identity"
    :disable-id true
    :effect (effect (disable-identity :corp))
    :leave-play (effect (enable-identity :corp))}})
