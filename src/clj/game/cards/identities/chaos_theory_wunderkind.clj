(in-ns 'game.cards.identities)

(def card-definition-chaos-theory-wunderkind
  {"Chaos Theory: Wünderkind"
   {:effect (effect (gain :memory 1))
    :leave-play (effect (lose :runner :memory 1))}})
