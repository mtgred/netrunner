(in-ns 'game.cards.operations)

(def card-definition-cerebral-static
  {"Cerebral Static"
   {:msg "disable the Runner's identity"
    :effect (effect (disable-identity :runner))
    :leave-play (effect (enable-identity :runner))}})
