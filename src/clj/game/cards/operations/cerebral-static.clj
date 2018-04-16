(in-ns 'game.core)

(def card-operations-cerebral-static
  {"Cerebral Static"
   {:msg "disable the Runner's identity"
    :effect (effect (disable-identity :runner))
    :leave-play (effect (enable-identity :runner))}})
